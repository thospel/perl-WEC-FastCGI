package WEC::FastCGI::Request;
use 5.008;
use strict;
use warnings;
use Carp;
use WEC qw(api=1);
use WEC::FastCGI;
use WEC::FastCGI::Constants qw(:RecordTypes :Roles
                               REQUEST_COMPLETE LOST_CONNECTION KEEP_CONN);
use AutoLoader qw(AUTOLOAD);

our $VERSION = "1.000";
our @EXPORT_OK = qw(parse_params %role_names);

use base qw(WEC::Flow);
#use fields qw(flags type cgi used
#              data   data_buffer   data_closed
#              params params_buffer params_closed
#              stdin  stdin_buffer  stdin_closed
#              stdout stdout_buffer stdout_closed
#              stderr stderr_buffer stderr_closed);

our %role_names =
    (RESPONDER()	=> "Responder",
     AUTHORIZER()	=> "Authorizer",
     FILTER()		=> "Filter");

sub init_flow {
    my $req = shift;
    $req->{parent} || croak "No parent object";
    $req->{options} = shift || croak "No options";
    defined($req->{id}   = shift) || croak "No id";
    defined($req->{type} = shift) || croak "No role";
    $req->{flags}  = shift;
    $req->{stdin}  = $req->{data} = $req->{params} = "";
    $req->{stdout} = $req->{stderr} = "";
    # Buffersizes at which we flush
    $req->{stdin_buffer} = $req->{stdout_buffer} = $req->{data_buffer} = 4096;
    $req->{params_buffer} = 2**16-8;
    $req->{stderr_buffer} = 0;
    $req->{ended} = 0;
    return $req->{id};
}

package WEC::FastCGI::TieHandle;
use base qw(Tie::Handle);

sub TIEHANDLE {
    my $req = $_[1];
    return bless \$req, shift;
}

package WEC::FastCGI::TieSTDOUT;
our @ISA = qw(WEC::FastCGI::TieHandle);

sub WRITE {
    my $string = substr($_[1], $_[3], $_[2]);
    ${shift()}->stdout_flush($string);
    return length $string;
}

sub PRINT {
    ${shift()}->stdout(defined $\ ? join(defined $, ? $, : "", @_) . $\ :
                       join(defined $, ? $, : "", @_));
    return 1;
}

sub PRINTF {
    ${shift()}->stdout(sprintf(shift, @_));
    return 1;
}

sub CLOSE {
    ${shift()}->stdout_close;
}

package WEC::FastCGI::TieSTDERR;
our @ISA = qw(WEC::FastCGI::TieHandle);

sub WRITE {
    my $string = substr($_[1], $_[3], $_[2]);
    ${shift()}->stderr_flush($string);
    return length $string;
}

sub PRINT {
    ${shift()}->stderr(defined $\ ? join(defined $, ? $, : "", @_) . $\ :
                       join(defined $, ? $, : "", @_));
    return 1;
}

sub PRINTF {
    ${shift()}->stderr(sprintf(shift, @_));
    return 1;
}

sub CLOSE {
    ${shift()}->stderr_close;
}

package WEC::FastCGI::TieSTDIN;
use Carp qw(carp);
our @ISA = qw(WEC::FastCGI::TieHandle);

sub READLINE {
    my WEC::FastCGI::Request $req = ${shift()};
    return if $req->{stdin} eq "";
    return substr($req->{stdin}, 0, length($req->{stdin}), "") if !defined $/;
    return substr($req->{stdin}, 0, ${$/}, "") if ref $/;
    return substr($req->{stdin}, 0,
                  1+index($req->{stdin}, $/) || length($req->{stdin}), "");
}

sub GETC {
    my WEC::FastCGI::Request $req = ${shift()};
    return if $req->{stdin} eq "";
    return substr($req->{stdin}, 0, 1, "");
}

sub READ {
    my WEC::FastCGI::Request $req = ${shift()};
    return 0 if $req->{stdin} eq "";
    my $result = \shift;
    return length(substr($$result, shift) =
                  substr($req->{stdin}, 0, shift, ""));
}

sub CLOSE {
    # Do nothing
}

package WEC::FastCGI::Request;

1;
__END__

# Server side operation
sub do_cgi : method {
    my $req = shift;
    my $options = $req->{options} || croak "Request already closed";
    tie(*STDIN,  "WEC::FastCGI::TieSTDIN",  $req) if
        $options->{TieStdin};
    tie(*STDOUT, "WEC::FastCGI::TieSTDOUT", $req) if
        $options->{TieStdout};
    tie(*STDERR, "WEC::FastCGI::TieSTDERR", $req) if
        $options->{TieStderr};
    eval {
        local *ENV = ref($req->{params}) eq "HASH" ? $req->{params} : {} if
            $options->{FakeEnv};
        $req->{cgi} ||= $options->{CgiClass}->new if $options->{CgiClass};
        local $CGI::Q = $req->{cgi} if $options->{FakeCgi};
        $req->{used} ||= 1;
        my $fun = shift;
        $fun->($req, $req->{cgi}, @_);
    };
    my $fail = $@;
    untie(*STDERR) if $options->{TieStderr};
    untie(*STDOUT) if $options->{TieStdout};
    untie(*STDIN)  if $options->{TieStdin};
    die $fail if $fail;
}

sub parse_params {
    my %params;
    # The dummy assign is to avoid a perl bug where the arg is a substr alias
    # and you will pull in the padding
    for (my $dummy = shift) {
        while ($_ ne "") {
            my $nlen = ord(substr($_, 0, 1, ""));
            $nlen = unpack("N", chr($nlen & 0x7f) . substr($_, 0, 3, "")) if
                $nlen >= 128;
            my $vlen = ord(substr($_, 0, 1, ""));
            $vlen = unpack("N", chr($vlen & 0x7f) . substr($_, 0, 3, "")) if
                $vlen >= 128;
            my $name = substr($_, 0, $nlen, "");
            $params{$name} = substr($_, 0, $vlen, "");
        }
    }
    return \%params;
}

# Server side operation
sub got_params {
    # Copy to $str is needed to work around substr assign bug (fixed in 5.8.4)
    (my $req, my $str) = @_;
    if ($str ne "") {
        $req->do_cgi($req->{options}{Params}, $str) if $req->{options}{Params};
        $req->{params} .= $str;
    } else {
        # print STDERR "End of param stream\n";
        $req->{params} = parse_params($req->{params});
        $req->do_cgi($req->{options}{ParamsEof}, $req->{params})
            if $req->{options}{ParamsEof};
        $req->end() if $req->{type} == AUTHORIZER && !$req->{ended};
    }
}

# Server side operation
sub got_stdin {
    # Copy to $str is needed to work around substr assign bug (fixed in 5.8.4)
    (my $req, my $str) = @_;
    if ($str ne "") {
        $req->do_cgi($req->{options}{Stdin}, $str) if $req->{options}{Stdin};
        $req->{stdin} .= $str;
    } else {
        $req->do_cgi($req->{options}{StdinEof}, $req->{stdin}) if
            $req->{options}{StdinEof};
        $req->end() if $req->{type} == RESPONDER && !$req->{ended};
    }
}

# Client side operation
sub got_stdout {
    # Copy to $str is needed to work around substr assign bug (fixed in 5.8.4)
    (my $req, my $str) = @_;
    if ($str ne "") {
        $req->{options}{Stdout}->($req, $str) if $req->{options}{Stdout};
        $req->{stdout} .= $str;
    } elsif ($req->{options}{StdoutEof}) {
        $req->{options}{StdoutEof}->($req, $req->{stdout});
    }
}

# Client side operation
sub got_stderr {
    # Copy to $str is needed to work around substr assign bug (fixed in 5.8.4)
    (my $req, my $str) = @_;
    if ($str ne "") {
        $req->{options}{Stderr}->($req, $str) if $req->{options}{Stderr};
        $req->{stderr} .= $str;
    } elsif ($req->{options}{StderrEof}) {
        $req->{options}{StderrEof}->($req, $req->{stderr});
    }
}

# Server side operation
sub got_data {
    # Copy to $str is needed to work around substr assign bug (fixed in 5.8.4)
    (my $req, my $str) = @_;
    if ($str ne "") {
        $req->do_cgi($req->{options}{Data}, $str) if $req->{options}{Data};
        $req->{data} .= $str;
    } else {
        $req->do_cgi($req->{options}{DataEof}, $req->{data}) if
            $req->{options}{DataEof};
        $req->end() if $req->{type} == FILTER && !$req->{ended};
    }
}

# Client side operation (no argument form also usable Server side)
sub stdin {
    my $req = shift;
    return $req->{stdin} unless @_;
    croak "stdin already closed" if $req->{stdin_closed};
    $req->{stdin} .= utf8::is_utf8($_[0]) ? do {
        my $str = shift;
        utf8::downgrade($str, 1) || croak "Wide character in stdin";
        $str } : shift;
    if (length($req->{stdin}) >= $req->{stdin_buffer}) {
        $req->{parent}->send(STDIN_STREAM, $req->{id},
                             substr($req->{stdin}, 0, 2**16-8, ""))
            while length($req->{stdin}) >= 2**16-8;
        $req->{parent}->send(STDIN_STREAM, $req->{id},
                             substr($req->{stdin}, 0,
                                    length($req->{stdin}), ""))
            if length($req->{stdin}) >= $req->{stdin_buffer};
    }
}

# Client side operation
sub stdin_flush {
    my $req = shift;
    if ($req->{stdin} ne "") {
        my $b = $req->{stdin_buffer};
        $req->{stdin_buffer} = 0;
        $req->stdin(@_ ? @_ : "");
        $req->{stdin_buffer} = $b;
    } elsif ($req->{stdin_closed}) {
        croak "stdin already closed";
    }
}

# Client side operation
sub stdin_buffer {
    my $req = shift;
    return $req->{stdin_buffer} unless @_;
    my $old = $req->{stdin_buffer};
    $req->{stdin_buffer} = shift;
    return $old;
}

# Client side operation
sub stdin_close {
    my $req = shift;
    $req->stdin_flush(@_);
    $req->{parent}->send(STDIN_STREAM, $req->{id}, "");
    $req->{stdin_closed} = 1;
}

# Server side operation (no argument form also usable Client side)
sub stdout {
    my $req = shift;
    return $req->{stdout} unless @_;
    croak "stdout already closed" if $req->{stdout_closed};
    $req->{stdout} .= utf8::is_utf8($_[0]) ? do {
        my $str = shift;
        utf8::downgrade($str, 1) || croak "Wide character in stdout";
        $str } : shift;
    if (length($req->{stdout}) >= $req->{stdout_buffer}) {
        $req->{parent}->send(STDOUT_STREAM, $req->{id},
                                 substr($req->{stdout}, 0, 2**16-8, ""))
            while length($req->{stdout}) >= 2**16-8;
        $req->{parent}->send(STDOUT_STREAM, $req->{id},
                                 substr($req->{stdout}, 0,
                                        length($req->{stdout}), ""))
            if length($req->{stdout}) >= $req->{stdout_buffer};
    }
}

# Server side operation
sub stdout_flush {
    my $req = shift;
    if ($req->{stdout} ne "") {
        my $b = $req->{stdout_buffer};
        $req->{stdout_buffer} = 0;
        $req->stdout(@_ ? @_ : "");
        $req->{stdout_buffer} = $b;
    } elsif ($req->{stdout_closed}) {
        croak "stdout already closed";
    }
}

# Server side operation
sub stdout_buffer {
    my $req = shift;
    return $req->{stdout_buffer} unless @_;
    my $old = $req->{stdout_buffer};
    $req->{stdout_buffer} = shift;
    return $old;
}

# Server side operation
sub stdout_close {
    my $req = shift;
    $req->stdout_flush(@_);
    $req->{parent}->send(STDOUT_STREAM, $req->{id}, "");
    $req->{stdout_closed} = 1;
}

# Server side operation (no argument form also usable Client side)
sub stderr {
    my $req = shift;
    return $req->{stderr} unless @_;
    croak "stderr already closed" if $req->{stderr_closed};
    $req->{stderr} .= utf8::is_utf8($_[0]) ? do {
        my $str = shift;
        utf8::downgrade($str, 1) || croak "Wide character in stderr";
        $str } : shift;
    if (length($req->{stderr}) >= $req->{stderr_buffer}) {
        $req->{parent}->send(STDERR_STREAM, $req->{id},
                                   substr($req->{stderr}, 0, 2**16-8, ""))
            while length($req->{stderr}) >= 2**16-8;
        $req->{parent}->send(STDERR_STREAM, $req->{id},
                                 substr($req->{stderr}, 0,
                                        length($req->{stderr}), ""))
            if length($req->{stderr}) >= $req->{stderr_buffer};
    }
}

# Server side operation
sub stderr_flush {
    my $req = shift;
    if ($req->{stderr} ne "") {
        my $b = $req->{stderr_buffer};
        $req->{stderr_buffer} = 0;
        $req->stderr(@_ ? @_ : "");
        $req->{stderr_buffer} = $b;
    } elsif ($req->{stderr_closed}) {
        croak "stderr already closed";
    }
}

# Server side operation
sub stderr_buffer {
    my $req = shift;
    return $req->{stderr_buffer} unless @_;
    my $old = $req->{stderr_buffer};
    $req->{stderr_buffer} = shift;
    return $old;
}

# Server side operation
sub stderr_close {
    my $req = shift;
    $req->stderr_flush(@_);
    $req->{parent}->send(STDERR_STREAM, $req->{id}, "");
    $req->{stderr_closed} = 1;
}

# Client side operation (no argument form also usable Server side)
sub data {
    my $req = shift;
    return $req->{data} unless @_;
    croak "data already closed" if $req->{data_closed};
    $req->{data} .= utf8::is_utf8($_[0]) ? do {
        my $str = shift;
        utf8::downgrade($str, 1) || croak "Wide character in data";
        $str } : shift;
    if (length($req->{data}) >= $req->{data_buffer}) {
        $req->{parent}->send(DATA_STREAM, $req->{id},
                                 substr($req->{data}, 0, 2**16-8, ""))
            while length($req->{data}) >= 2**16-8;
        $req->{parent}->send(DATA_STREAM, $req->{id},
                                 substr($req->{data}, 0,
                                        length($req->{data}), ""))
            if length($req->{data}) >= $req->{data_buffer};
    }
}

# Client side operation
sub data_flush {
    my $req = shift;
    if ($req->{data} ne "") {
        my $b = $req->{data_buffer};
        $req->{data_buffer} = 0;
        $req->data(@_ ? @_ : "");
        $req->{data_buffer} = $b;
    } elsif ($req->{data_closed}) {
        croak "data already closed";
    }
}

# Client side operation
sub data_buffer {
    my $req = shift;
    return $req->{data_buffer} unless @_;
    my $old = $req->{data_buffer};
    $req->{data_buffer} = shift;
    return $old;
}

# Client side operation
sub data_close {
    my $req = shift;
    $req->data_flush(@_);
    $req->{parent}->send(DATA_STREAM, $req->{id}, "");
    $req->{data_closed} = 1;
}

# Server side operation
sub end {
    my $req = shift;
    croak "Request $req->{id} is already ended" if $req->{ended} > 0;
    $req->{ended} = 1;
    $req->stdout_close unless $req->{stdout_closed};
    $req->stderr_close unless $req->{stderr_closed};
    my $connection = $req->{parent};
    $connection->send(END_REQUEST, $req->{id},
                      pack("NCx3", shift || 0, shift || REQUEST_COMPLETE));
    $req->{cgi} = undef;
    $req->_drop;
    $connection->close_on_empty if
        !$req->{flags} & KEEP_CONN && $connection->handle;
}

# Server side:
# no arg means it's artificial as result of a connection close.
# defined arg means the client just sent us an
# abort/unknown type/duplicate begin
#
# Client side:
# no arg means it's artificial as result of a connection close.
# undef arg means it's a remote EOF and EofIsEnd is true
sub _abort_flow {
    my $req = shift;

    # Should be impossible because an ended request should not be in the
    # connection list, so it should be delivered to Connection, not Request
    # So this isn't a race condition as it might seem at first.
    if ($req->{ended} > 0) {
        $req->_drop;
        return;
    }
    $req->{ended} = 1;
    eval {
        my $rc = $req->{used} && $req->{options}{Abort} ?
            $req->{options}{Abort}->($req) : 0xffffffff;
        if (@_) {
            $req->{ended} = 0;
            if (defined(shift)) {
                # Server side real abort
                $req->{stdout} = $req->{stderr} = "";
                $req->end($rc);
            } else {
                # Client side EOF
                $req->got_end_request("\x00" x 8);
            }
        } else {
            $req->{ended} = 1;
            $req->{options}{RequestEnd}->($req, undef, LOST_CONNECTION) if
                $req->{options}{RequestEnd};
            $req->_drop if $req->{parent};
        }
    };
    return unless $@;
    my $err = $@;
    $req->_drop if $req->{parent};
    die $err;
}

# Server side operation
sub abort_request {
    shift->_abort_flow(1);
}

# Client side operation
sub abort {
    my $req = shift;
    $req->{parent}->send(ABORT_REQUEST, $req->{id}, "");
}

# Server side operation
sub unknown_type {
    my $req = shift;
    if ($req->{options}{UnknownApplicationRecord}) {
        $req->{options}{UnknownApplicationRecord}->
            ($req, $req->{parent}->type, shift);
    } else {
        warn("Unknown application record type ",
             $req->{parent}->type, ", data ", unpack("H*", $_[0]), "\n");
        $req->_abort_flow(1);
    }
}

# Client side operation (no argument form also usable Server side)
sub params {
    my $req = shift;
    return ref($req->{params}) eq "HASH" ? $req->{params} : () unless @_;
    croak "params already closed" if $req->{params_closed};

    for ($req->{params}) {
        while (@_) {
            if (ref($_[0])) {
                my $params = shift;
                for my $pname (keys %$params) {
                    (utf8::downgrade $pname, 1) ||
                        croak "Wide character in param key";
                    if (length($pname) < 128) {
                        $_ .= chr(length($pname));
                    } else {
                        $_ .= "\x80" | pack("N", length($pname));
                    }
                    if (utf8::is_utf8($params->{$pname})) {
                        my $pvalue = $params->{$pname};
                        (utf8::downgrade $pvalue, 1) ||
                            croak "Wide character in param value";
                        if (length($pvalue) < 128) {
                            $_ .= chr(length($pvalue));
                        } else {
                            $_ .= "\x80" | pack("N", length($pvalue));
                        }
                        $_ .= $pname;
                        $_ .= $pvalue;
                    } else {
                        if (length($params->{$pname}) < 128) {
                            $_ .= chr(length($params->{$pname}));
                        } else {
                            $_ .= "\x80" | pack("N", length($params->{$pname}));
                        }
                        $_ .= $pname;
                        $_ .= $params->{$pname};
                    }
                    $req->{parent}->send(PARAMS_STREAM, $req->{id},
                                             substr($_, 0, 2**16-8, ""))
                        while length >= 2**16-8;
                }
                next;
            }
            my $pname  = shift;
            (utf8::downgrade $pname, 1) || croak "Wide character in param key";
            my $pvalue = shift;
            (utf8::downgrade $pvalue, 1) ||
                croak "Wide character in param value";
            if (length($pname) < 128) {
                $_ .= chr(length($pname));
            } else {
                $_ .= "\x80" | pack("N", length($pname));
            }

            if (length($pvalue) < 128) {
                $_ .= chr(length($pvalue));
            } else {
                $_ .= "\x80" | pack("N", length($pvalue));
            }
            $_ .= $pname;
            $_ .= $pvalue;
            $req->{parent}->send(PARAMS_STREAM, $req->{id},
                                     substr($_, 0, 2**16-8, ""))
                while length >= 2**16-8;
        }
        $req->{parent}->send(PARAMS_STREAM, $req->{id},
                                 substr($_, 0, length, ""))
            if length($_) >= $req->{params_buffer};
    }
}

# Client side operation
sub params_flush {
    my $req = shift;
    croak "params already closed" if $req->{params_closed};
    $req->params(@_) if @_;
    return if $req->{params} eq "";
    $req->{parent}->send(PARAMS_STREAM, $req->{id},
                             substr($req->{params}, 0, 2**16-8, ""))
        while length($req->{params}) >= 2**16-8;
    return if $req->{params} eq "";
    $req->{parent}->send(PARAMS_STREAM, $req->{id}, $req->{params});
    $req->{params} = "";
}

# Client side operation
sub params_buffer {
    my $req = shift;
    return $req->{params_buffer} unless @_;
    my $old = $req->{params_buffer};
    $req->{params_buffer} = shift;
    return $old;
}

# Client side operation
sub params_close {
    my $req = shift;
    $req->params_flush(@_);
    $req->{parent}->send(PARAMS_STREAM, $req->{id}, "");
    $req->{params_closed} = 1;
}

# Client side operation
sub got_end_request {
    my $req = shift;
    if ($req->{ended} > 0) {
        warn("Received end_request on already ended request\n");
        return;
    }
    $req->{ended} = 1;
    $req->{options}{RequestEnd}->($req, unpack("NC", shift)) if
        $req->{options}{RequestEnd};
    $req->_drop if $req->{parent};
}

# Server side (invalid) operation
sub begin_request {
    my $req = shift;
    warn("begin_request on an already running request, aborting old one and restarting\n");
    $req->_abort_flow(1);
    $req->{parent}->begin_request(@_);
}

sub id {
    return shift->{id};
}

sub role {
    return shift->{type};
}

sub role_name {
    my $req = shift;
    return $role_names{$req->{type}} || croak "Unknown role id '$req->{type}'";
}

# Override WEC::Object to call do_cgi
sub add_alarm {
    my $req    = shift;
    return $req->SUPER::add_alarm(shift, $req->can("do_cgi"), @_);
}

1;
__END__

=head1 NAME

WEC::FastCGI::Request - A request in event driven FastCGI

=head1 SYNOPSIS

 # It inherits all methods from WEC::Object, except that the ones setting
 # a callback are modified to call that through the do_cgi method

 # Client side methods
 my $client	= $request->client;

 $request->params(@name_value_pairs_or_hashrefs);
 $request->params_flush(?@name_value_pairs_or_hashrefs?);
 $request->params_close(?@name_value_pairs_or_hashrefs?);

 $request->stdin($string);
 $request->stdin_flush(?$string?);
 $request->stdin_close(?$string?);

 $request->data($string);
 $request->data_flush(?$string?);
 $request->data_close(?$string?);

 my $string = $request->stdout;
 my $string = $request->stderr;

 $request->abort;

 # Server side methods
 my $server     = $request->server;

 my $params = $request->params;
 my $string = $request->stdin;
 my $string = $request->data;

 $request->stdout($string);
 $request->stdout_flush(?$string?);
 $request->stdout_close(?$string?);

 $request->stderr($string);
 $request->stderr_flush(?$string?);
 $request->stderr_close(?$string?);

 $request->extend;
 $request->abort_request;
 $request->end(?$app_status, ?$proto_status??);

 $request->do_cgi($fun, @args);

 # Methods that make sense on both sides
 my $connection = $request->connection;

 my $size = $request->params_buffer;
 my $old_size = $request->params_buffer($new_size);

 my $size = $request->stdin_buffer;
 my $old_size = $request->stdin_buffer($new_size);

 my $size = $request->data_buffer;
 my $old_size = $request->data_buffer($new_size);

 my $size = $request->stdout_buffer;
 my $old_size = $request->stdout_buffer($new_size);

 my $size = $request->stderr_buffer;
 my $old_size = $request->stderr_buffer($new_size);

 my $id = $request->id;
 my $role_id = $request->role;
 my $role_name = $request->role_name;

=head1 DESCRIPTION

This class is used by  both
the L<WEC::FastCGI::Server class|WEC::FastCGI::Server> and the
the L<WEC::FastCGI::Client class|WEC::FastCGI::Client> to describe requests.
Every request will also belong to some
L<connection object|WEC::FastCGI::Connection>.

As a client you create request objects using the request method
L<on a client|WEC::FastCGI::Client/request> or
L<on a connection|WEC::FastCGI::Connection/request>. If you are a server,
they are implcitely created for you when requests come in from a client
and you will mainly get them passed to you as a parameter in callbacks.

=head1 METHODS

Some of the methods only make sense in the protocol if you use them on the
client or the server side. There is no check if you use them from the wrong
side, they will just do whatever they would normally do at the other side,
and you will possibly be sending protocol messages in an unexpected direction.
If the other side has proper handlers for that, this may actually work, but
most likely you will just be completely confusing to everyone.

All methods will throw an exception in case of failure.

All streams are buffered, meaning that if you add to them, they initially go
into a local buffer for that stream. Only if that buffer reaches a certain
buffer size (possibly different for each strem type) will blobs be queued
for sending until the buffer drops to below the given size again. If you want
a buffer to be queued immediately, you need to use the corresponding flush
method. A stream close also implicitely does a flush first. Notice that the
stderr stream buffer size starts as 0, so by default stderr is autoflushing.

This class inherits from L<WEC::FieldObject|WEC::FieldObject>, so it also
inherits all methods of that class.

=over

=item Client side methods

=over

=item X<client>my $client = $request->client

Returns the L<WEC::FastCGI::Client object|WEC::FastCGI::Client> on which this
request is running.

=item X<params>$request->params(@name_value_pairs_or_hashrefs)

Name value pairs will be encoded and appended to the parameters stream.
Arguments are a sequence of name value pairs or references to hashes of name
value pairs. So the following four calls are equivalent:

    $request->params(foo => 1, bar => 2);
    $request->params({foo => 1, bar => 2});
    $request->params({foo => 1}, bar => 2);
    $request->params(foo => 1, {bar => 2});

The case where there are no arguments is special, it's a
L<server side method to query the received parameters|"params_empty">. Be
careful of that if you pass a potentially empty array.

=item X<params_flush>$request->params_flush(?@name_value_pairs_or_hashrefs?)

If given any arguments, first does a L<params|"params"> call on these.
Then it queues everything in the parameters buffer, leaving the working buffer
empty.

=item X<params_close>$request->params_close(?@name_value_pairs_or_hashrefs?)

First does a L<params_flush|"params_flush"> with the given parameters, then
closes the parameters stream. Any attempts to start using the stream again
will cause an exception.

=item X<stdin>$request->stdin($string)

The given string will be appended to the input stream. The case where there
are no arguments is special, it's a
L<server side method to query the received stdin|"stdin_empty">.

=item X<stdin_flush>$request->stdin_flush(?$string?)

If given an argument, first does a L<stdin|"stdin"> on it. Then it queues
everything in the input buffer, leaving the working buffer empty.

=item X<stdin_close>$request->stdin_close(?$string?)

First does a L<stdin_flush|"stdin_flush"> passing on any arguments, then
closes the stdin stream. Any attempts to start using the stream again will
cause an exception.

=item X<data>$request->data($string)

The given string will be appended to the data stream. The case where there
are no arguments is special, it's a
L<server side method to query the received data|"data_empty">.

=item X<data_flush>$request->data_flush(?$string?)

If given an argument, first does a L<data|"data"> on it. Then it queues
everything in the data buffer, leaving the working buffer empty.

=item X<data_close>$request->data_close(?$string?)

First does a L<data_flush|"data_flush"> passing on any arguments, then
closes the data stream. Any attempts to start using the stream again will
cause an exception.

=item X<stdout_empty>my $string = $request->stdout

Returns the current contents of the stdout buffer.

=item X<stderr_empty>my $string = $request->stderr

Returns the current contents of the stderr buffer.

=item X<abort>$request->abort

Sends an abort for the given request to the server. Notice that by the time
the abort arrives there, the request can already have been finished. In either
case, you can usually expect a request end return from the server pretty soon
(which can be handled in the L<RequestEnd|WEC::FastCGI::Client/RequestEnd>
callback).

Don't confuse this method with L<abort_request|"abort_request"> which is a
server side method signifying that the server gives up on that request.

=back

=item Server side methods

=over

=item X<server>my $server = $request->server

Returns the L<WEC::FastCGI::Server object|WEC::FastCGI::Server> on which this
request is running.

=item X<params_empty>my $params = $request->params

Returns the current contents of the parameter buffer. Before the server
received the close of the parameters stream, this will be the internal encoded
parameters string which probably isn't even finished yet. So in that case
the method will return undef.

After the parameters close this internal buffer will get decoded and replaced
by a reference to a hash of name value pairs (if names are duplicated the last
value wins). The method will return this reference.

Normally you will know from the protocol when the parameters have been decoded.
E.g. when you get the L<StdinEof|WEC::FastCGI::Server/StdinEof> callback
for a RESPONDER, the parameter stream should already have been closed.
However, it's probably a good idea to test the result of this method and for
example L<abort the request|"abort_request"> or replace the result with an
empty hash reference if it returns false.

=item X<stdin_empty>my $string = $request->stdin

Returns the current contents of the stdin buffer.

=item X<data_empty>my $string = $request->data

Returns the current contents of the data buffer.

=item X<stdout>$request->stdout($string)

The given string will be appended to the stdout stream. The case where there
are no arguments is special, it's a
L<client side method to query the received stdout|"stdout_empty">.

=item X<stdout_flush>$request->stdout_flush(?$string?)

If given an argument, first does a L<stdout|"stdout"> on it. Then it queues
everything in the stdout buffer, leaving the working buffer empty.

=item X<stdout_close>$request->stdout_close(?$string?)

First does a L<stdout_flush|"stdout_flush"> passing on any arguments, then
closes the stdout stream. Any attempts to start using the stream again will
cause an exception.

=item X<stderr>$request->stderr($string)

The given string will be appended to the stderr stream. The case where there
are no arguments is special, it's a
L<client side method to query the received stderr|"stderr_empty">.

=item X<stderr_flush>$request->stderr_flush(?$string?)

If given an argument, first does a L<stderr|"stderr"> on it. Then it queues
everything in the stderr buffer, leaving the working buffer empty. Since
the stderr buffer size defaults to 0, this normally won't be necessary.

=item X<stderr_close>$request->stderr_close(?$string?)

First does a L<stderr_flush|"stderr_flush"> passing on any arguments, then
closes the data stream. Any attempts to start using the stream again will
cause an exception.

=item X<extend>$request->extend

All the standard roles have a point where they they will automatically
L<end|"end"> (at parameters EOF for an authorizer, at stdin EOF for a responder
and at data EOF for a filter). If you are not yet ready with your answer at
that time, you might want to not send the end yet and continue working. You can
do that by calling extend on the request at any time before the automatic end,
which then will not be sent. It then becomes your responsibility to make sure
that at some later time your will do work for your request again and at some
point finish it with an L<end|"end"> (or L<abort_request|"abort_request">).
See L<EXAMPLE 2 in the server documentation|WEC::FastCGI::Server/"EXAMPLE 2">
for an example.

=item X<abort_request>$request->abort_request

You can call this if you want to give up on the current request which will
then be ended in the same way as if the client would have send an abort
request. Since in either case L<end|"end"> will get called the only difference
with directly calling L<end|"end"> is that you have less control over the
status returns and that the L<Abort"WEC::Client::Server/Abort> callback will
be called for you.

Don't confuse this with L<the abort method|"abort"> which is a client side
method that will request the server to abort.

=item X<end>$request->end(?$app_status, ?$proto_status??)

Ends the current request and returns the given application status (a 32-bit
unsigned integer, defaults to 0) and protocol status (an 8-bit unsigned
integer, defaults to REQUEST_COMPLETE) to the client.

Don't confuse this with the http status code, which is returned in the header.

=item X<do_cgi>$request->do_cgi($fun, @args)

This is a method for people implementing callbacks. It will tie stdin if
L<TieStdin|WEC::FastCGI::Server/TieStdin> is set, tie stdout if
L<TieStdout|WEC::FastCGI::Server/TieStdout> is set and tie stderr if
L<TieStdout|WEC::FastCGI::Server/TieStderr> is set. Next it will set up
a fake %ENV from the currently received parameter stream (or an empty %ENV
if the parameter stream has not been closed yet) if
L<FakeEnv|WEC::FastCGI::Server/FakeEnv> is set. If
L<CgiClass|WEC::FastCGI::Server/CgiClass> is set and no cgi object is
associated with the request yet, it will call new on the CgiClass and associate
the result with the request. And finally it will set $CGI::Q to the cgi object
associated with the request if L<FakeCgi|WEC::FastCGI::Server/FakeCgi> is set.

After all that, it will call:

    $fun->($request, $associated_cgi_object, @args);

When that returns all ties are undone and $CGI::Q and %ENV are restored, but
the CGI object remains associated with this request and will be reused without
creation the next time.

=back

=item Methods that make sense on both sides

=over

=item X<connection>my $connection = $request->connection

Return the L<WEC::FastCGI::Connection object|WEC::FastCGI::Connection> on
which the request is running.

=item X<params_buffer>my $size = $request->params_buffer

Returns the current size of the parameters buffer.

=item $old_size = $request->params_buffer($new_size)

Sets the size of the parameters buffer to $new_size, returns the old size.
Setting it to 0 effectively make L<params|"params"> autoflushing.

=item X<stdin_buffer>my $size = $request->stdin_buffer

Returns the current size of the stdin buffer.

=item my $old_size = $request->stdin_buffer($new_size)

Sets the size of the stdin buffer to $new_size, returns the old size.
Setting it to 0 effectively make L<stdin|"stdin"> autoflushing.

=item X<data_buffer>my $size = $request->data_buffer

Returns the current size of the data buffer.

=item my $old_size = $request->data_buffer($new_size)

Sets the size of the data buffer to $new_size, returns the old size.
Setting it to 0 effectively make L<data|"data"> autoflushing.

=item X<stdout_buffer>my $size = $request->stdout_buffer

Returns the current size of the stdout buffer.

=item my $old_size = $request->stdout_buffer($new_size)

Sets the size of the stdout buffer to $new_size, returns the old size.
Setting it to 0 effectively make L<stdout|"stdout"> autoflushing.

=item X<stderr_buffer>my $size = $request->stderr_buffer

Returns the current size of the stderr buffer.

=item my $old_size = $request->stderr_buffer($new_size)

Sets the size of the stderr buffer to $new_size, returns the old size.
Setting it to 0 effectively make L<stderr|"stderr"> autoflushing
(but the stderr buffer size already defaults to 0).

=item X<id>my $id = $request->id

Returns the id of the request. This will be a small integer (1 to 65535)
unique per connection.

=item X<role>my $role_id = $request->role

Returns the role id of the role for this request. Standard values are to
expect here are C<RESPONDER>, C<AUTHORIZER> or C<FILTER>.

=item X<role_name>my $role_name = $request->role_name

Returns a printable name for the role this request plays. Standard values to
expect here are C<Responder>, C<Authorizer> and C<Filter>.

=back

=head1 EXPORT

None by default.

=head1 SEE ALSO

L<WEC::FastCGI::Client>,
L<WEC::FastCGI::Server>,
L<WEC::FastCGI::Connection>,
L<http://www.fastcgi.com/devkit/doc/fcgi-spec.html>

=head1 AUTHOR

Ton Hospel, E<lt>WEC-FastCGI@ton.iguana.beE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2004 by Ton Hospel

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

package WEC::FastCGI::Connection;
use 5.008_001;	# Otherwise you will hit a closure leak
use strict;
use warnings;
use Carp;
use AutoLoader qw(AUTOLOAD);

use WEC::FastCGI::Constants qw(:RecordTypes :Roles 
                               VERSION MAX_REQUEST PRE_RECORD KEEP_CONN);
use WEC::FastCGI::Request qw(parse_params);
use WEC::Connection qw(HEADER BODY);

our $VERSION = "0.01";
our @EXPORT_OK = qw(parse_params %value_replies $flow_class);

use base qw(WEC::Connection);
# use fields qw(in_state type req_id clength try_slot);

our %value_replies =
    # FCGI_MPXS_CONNS is handled specially
    (FCGI_MAX_CONNS	=> 100,
     FCGI_MAX_REQS	=> 100);

our $flow_class = "WEC::FastCGI::Request";

sub init_server {
    my $connection = shift;
    $connection->{try_slot}	= 1;
    $connection->{in_state}	= HEADER;
    $connection->{in_want}	= PRE_RECORD;
    $connection->{in_process}	= \&record;
}

sub init_client {
    my $connection = shift;
    $connection->{try_slot}	= 1;
    $connection->{in_state}	= HEADER;
    $connection->{in_want}	= PRE_RECORD;
    $connection->{in_process}	= \&record;
}

my @type_handler;
$type_handler[BEGIN_REQUEST]	= "begin_request";	# C -> S
$type_handler[ABORT_REQUEST]	= "abort_request";	# C -> S
$type_handler[END_REQUEST]	= "got_end_request";	# S -> C
$type_handler[PARAMS_STREAM]	= "got_params";		# C -> S
$type_handler[STDIN_STREAM]	= "got_stdin";		# C -> S
$type_handler[STDOUT_STREAM]	= "got_stdout";		# S -> C
$type_handler[STDERR_STREAM]	= "got_stderr";		# S -> C
$type_handler[DATA_STREAM]	= "got_data";		# C -> S
$type_handler[GET_VALUES]	= "got_get_values";	# C -> S,M
$type_handler[GET_VALUES_RESULT]= "got_values";		# S -> C,M
$type_handler[UNKNOWN_TYPE]	= "got_unknown_type";	# S -> C,M

sub nop {}
# Possible continueing server input when the app already closed
# So all non management Client -> Server entries except BEGIN_REQUEST
*abort_request	= \&nop;
*got_stdin	= \&nop;
*got_params	= \&nop;
*got_data	= \&nop;

sub record {
    my $connection = shift;
    my $want = $connection->{in_want};
    if ($connection->{in_state} == HEADER) {
        (my $version, $connection->{type}, $connection->{req_id},
         $connection->{clength}, my $plength) =
             unpack("CCnnCx", substr($_, 0, $want, ""));
        die "Unknown FCGI record version $version (I only speak ${\VERSION})\n"
            unless $version == VERSION;
        # print STDERR "Record type $connection->{type}, req_id $connection->{req_id}, clength $connection->{clength}, plength $plength\n";
        $want = $connection->{clength} + $plength;
        if (length() < $want) {
            $connection->{in_state} = BODY;
            $connection->{in_want} = $want;
            return;
        }
    }
    my $handler = $type_handler[$connection->{type}] || "unknown_type";
    $connection->{in_state}	= HEADER;
    $connection->{in_want}	= PRE_RECORD;
    ($connection->{flows}{$connection->{req_id}} ||
     $connection)->$handler(substr(substr($_, 0, $want, ""),
                                   0, $connection->{clength}));
}

# Call as $conn->send($type, $request_id, $content)
sub send : method {
    my $connection = shift;
    die "Attempt to send on a closed Connection" unless 
        $connection->{out_handle};
    # print STDERR "$$: send($_[0], $_[1], ", unpack("H*", $_[2]), ")\n" unless tied(*STDERR);
    
    die "Message is utf8" if utf8::is_utf8($_[2]);
    my $clength = length($_[2]);
    die "Message too long" if $clength >= 2**16;
    my $pad = -$clength & 7;
    $connection->send0 if $connection->{out_buffer} eq "";
    $connection->{out_buffer} .=
        pack("CCnnCx", VERSION, shift, shift, $clength, $pad);
    $connection->{out_buffer} .= shift;
    $connection->{out_buffer} .= "\x00" x $pad if $pad;
    return;
}

sub drop_flow {
    my $connection = shift;
    my $id = shift;
    $connection->SUPER::drop_flow($id, shift);
    $connection->{try_slot} = $id if $id < $connection->{try_slot};
}

sub type {
    return shift->{type};
}

1;
__END__

# Server side operation
sub begin_request {
    my $connection = shift;
    if ($connection->{req_id} == 0) {
        warn("Invalid begin_request id 0 ignored\n");
        return;
    }
    # No need to check for MAX_REQUEST, already guaranteed because the
    # req_id comes from an "n" unpack format.
    my $req = $connection->new_flow
        ($flow_class, $connection->{options}, 
         $connection->{req_id}, unpack("nC", shift));
    ($connection->{options}{RequestBegin} || return)->($connection, $req);
}

# Server side operation
sub got_get_values {
    my $connection = shift;
    my $request = parse_params(shift);
    my $replies = $connection->{options}{Values} || \%value_replies;
    my $reply = "";
    for (keys %$request) {
        if (exists $replies->{$_}) {
            if (length($_) < 128) {
                $reply .= chr(length($_));
            } else {
                $reply .= "\x80" | pack("N", length($_));
            }
            if (length($replies->{$_}) < 128) {
                $reply .= chr(length($replies->{$_}));
            } else {
                $reply .= "\x80" | pack("N", length($replies->{$_}));
            }
            $reply .= $_;
            $reply .= $replies->{$_};
        } elsif ($_ eq "FCGI_MPXS_CONNS") {
            $reply .= pack("CCa*a", length, 1, $_, 
                           $connection->{host_mpx} ? 1 : 0);
        }
    }
    $connection->send(GET_VALUES_RESULT, 0, $reply);
}

# Client side operation
sub got_values {
    my $connection = shift;
    if ($connection->{options}{GetValuesResult}) {
        $connection->{options}{GetValuesResult}->($connection, parse_params(shift));
    } else {
        warn("Received GetValuesResult without a handler\n");
    }
}

# Client side operation
sub got_unknown_type {
    my $connection = shift;
    if ($connection->{options}{UnknownType}) {
        $connection->{options}{UnknownType}->($connection, unpack("C", shift));
    } else {
        warn("Server claims not to know about type ", unpack("C",shift),"\n");
    }
}

# Server side operation
sub unknown {
    my $connection = shift;
    $connection->send(UNKNOWN_TYPE, 0,
                      pack("Cx7", @_ ? shift : $connection->{type}));
}

# Server side operation
sub unknown_type {
    my $connection = shift;
    if ($connection->{options}{UnknownManagementRecord}) {
        $connection->{options}{UnknownManagementRecord}->
            ($connection, $connection->{type}, shift);
    } else {
        warn("Unknown management record type $connection->{type}, hex data '",
             unpack("H*", shift), "'\n");
        $connection->unknown;
    }
}

# Client side operation
sub get_values {
    my $connection = shift;
    my $request = "";
    for (@_) {
        if (length($_) < 128) {
            $request .= chr(length($_)) . "\x00";
        } else {
            $request .= "\x80" | pack("Nx", length($_));
        }
        $request .= $_;
    }
    $connection->send(GET_VALUES, 0, $request);
}

# Client side operation
sub request {
    my $connection = shift;
    my $role = (!ref($_[0]) && shift) || RESPONDER;
    croak "Role id '$role' is not a natural number" unless
        $role =~ /^\s*\d+\s*$/;
    croak "Excessive role id '$role'" if $role >= 2**16;
    croak "Role id '$role' is too small" if $role < 1;
    croak "Connection is closed" unless $connection->{in_handle};
    croak "Connection is closing down" if $connection->{close_on_empty};
    my $slot;
    for ($connection->{try_slot}..MAX_REQUEST) {
        if (!$connection->{flows}{$_}) {
            $slot = $_;
            last;
        }
    }
    croak("Out of request slots. You already have ",  
          keys %{$connection->{flows}}," requests pending !") unless $slot;
    $connection->{try_slot} = $slot+1;
    my $flags = defined($connection->{flows_left}) && 
        --$connection->{flows_left} == 0 ? 0 : KEEP_CONN;
    my $req = $connection->new_flow
        ($flow_class, $connection->{options}, $slot, $role, $flags);
    $connection->send(BEGIN_REQUEST, $slot, pack("nCx5", $role, $flags));
    if (@_) {
        $req->params("CONTENT_LENGTH", length($_[1])) if
            @_> 1 && ($role == FILTER || $role == RESPONDER);
        $req->params("FCGI_DATA_LENGTH", length($_[2])) if
            @_> 2 && $role == FILTER;
        $req->params_close(shift);
        if (@_) {
            $req->stdin(shift);
            $req->stdin_close;
            if (@_) {
                $req->data(shift);
                $req->data_close;
            }
        }
    }
    return $req;
}

1;

__END__

=head1 NAME

WEC::FastCGI::Connection - A connection in event driven FastCGI

=head1 SYNOPSIS

 # Client side methods
 $connection->request(?$role?, ?$params, ?$stdin, ?$data???);
 $connection->get_values(@names);
 $one_shot = $connection->one_shot;
 $old_one_shot = $connection->one_shot($boolean);

 # Server side methods
 $connection->type;
 $connection->unknown(?$type?);

 # Methods that make sense on both sides
 $connection->close;
 $address = $connection->peer_address;
 $handle = $connection->handle;
 $user_data = $connection->user_data;
 $old_user_data = $connection->user_data($new_user_data);
 $conn->send($type, $request_id, $content);

 my $nr_connections = WEC::FastCGI::Connection::object_count();

=head1 DESCRIPTION

This class is used by both
the L<WEC::FastCGI::Server class|WEC::FastCGI::Server> and the
the L<WEC::FastCGI::Client class|WEC::FastCGI::Client> to describe
their connections.

You normally don't create objects of this class yourself, they are
created for you by the L<WEC::FastCGI::Client class|WEC::FastCGI::Client>
when you do a L<connect|WEC::FastCGI::Client/"connect"> and by the
L<WEC::FastCGI::Server class|WEC::FastCGI::Server> when it accepts an
incoming connection.

Also, every L<WEC::FastCGI::Request object |"WEC::FastCGI::Request> will be a
member of some connection, which you can retrieve using
L<the connection method|WEC::FastCGI::Request/"connection">.

=head1 METHODS

Some of the methods only make sense in the protocol if you use them on the
client or the server side. There is no check if you use them from the wrong
side, they will just do whatever they would normally do at the other side,
and you will possibly be sending protocol messages in an unexpected direction.
If the other side has proper handlers for that, this may actually work, but
most likely you will just be completely confusing to everyone.

All methods will throw an exception in case of failure.

=over

=item Client side methods

=over

=item X<request>$connection->request(?$role?, ?$parameters, ?$stdin, ?$data???)

The client uses this to send a request to a server. If there are arguments
and the first argument isn't a reference, it should be the role that
the client assumes the server to play for this request. If no role is given,
it defaults to RESPONDER. Standard roles are:

=over

=item RESPONDER

This basically corresponds to plain CGI. The client sends an environment
($params) and a stdin stream. The server side responds with stdout and stderr
streams and then ends the request.

=item AUTHORIZER

This is used for authentication. The client sends only an environemnt
($params). The server responds with stdout and stderr and then ends the
request. The client then uses the application status to decide if the
authentication succeeded or not.

=item FILTER

This is used to postprocess (filter) a datastream. The client sends an
environemnt ($params), stdin and then the data stream. The server side
responds by sending back a stdout and stderr stream and then ending the
request.

=back

There are quite a few other constraints on the order that things happen and
their content. Have a look at the official FastCGI specification
(L<http://www.fastcgi.com/devkit/doc/fcgi-spec.html#S6>) for that.

After the (optional) role argument comes the optional $parameters argument. If
given this should be a reference to a hash of name/value pairs. These will
be sent as the request parameters stream and then be closed (these parameters
will likely be represented as an environment at the server side). If you don't
give this argument, no parameters stream is sent (or closed), but you can at a
later time use the L<params method|WEC::FastCGI::Request/params> on the request
to send one.

Next comes the (optional) $stdin argument. If given this should be a string
which will be sent as the stdin stream and then be closed. If the role is that
of a L<responder|RESPONDER> or a L<filter|FILTER> it will also
send an extra parameter CONTENT_LENGTH equal to the length of stdin (this
parameter will in fact come before the ones given as argument). If you don't
give this argument, no stdin stream is sent (or closed), but you can at a
later time use the L<stdin method|WEC::FastCGI::Request/stdin> on the request
to send one. You will still be responsible for setting CONTENT_LENGTH if
needed.

And finally comes the (optional) $data argument. If given this should be a
string which will be sent as the data stream and then be closed. If the role
is that of a L<filter|FILTER> it will also send an extra parameter
FCGI_DATA_LENGTH equal to the length of data (this parameter will in fact come
before the ones given as argument). FCGI_DATA_LAST_MOD will not be
automatically be sent, you'll have to work out that one for yourself.
If you don't give a $data argument, no data stream is sent (or closed), but you
can at a later time use the L<data method|WEC::FastCGI::Request/data> on the
request to send one. You will still be responsible for setting
FCGI_DATA_LENGTH and FCGI_DATA_LAST_MOD if needed.

The method returns a L<WEC::FastCGI::Request object|WEC::FastCGI::Request>
representing the pending request. You can then let it run its course, or
at a later point in time L<abort|WEC::FastCGI::Request/abort> it. In any
case, you will always get a L<RequestEnd|WEC::FastCGI::Client/RequestEnd>
callback (if there is one), however the request gets finished (normal end,
abort, connection closed, I/O error on connection).

=item X<get_values>$connection->get_values(@names)

The client uses this to query specific variables of a server (or connection).
Typically used at the beginning to automate certain aspects of system
configuration. @names is a list of strings representing the names of server
variables the client would like to know the value of.

When an answer arrives you can process them using the
L<GetValuesResult|WEC::FastCGI::Client/GetValuesResult> callback. That will
give you a hashreference mapping names for which there was an answer to their
string values.

The standard set of variables provides information to help the client perform
application and connection management:

=over

=item FCGI_MAX_CONNS

The maximum number of concurrent transport connections the server will accept.

=item FCGI_MAX_REQS

The maximum number of concurrent requests the server will accept.

=item FCGI_MPXS_CONNS

"1" if the server multiplexes connections (i.e. handle concurrent requests
over each connection), "0" otherwise.

=back

=item X<one_shot>$one_shot = $connection->one_shot

Allows a client to query the one_shot status of this connection. A true value
means that any request will be sent with the KEEP_CONN flag set to zero,
meaning the server is supposed to close the connection when that request
finishes. Otherwise the client is the one responsible to close a connection
when it's done using it.

The default depends on what was given for the
L<OneShot|WEC::FastCGI::Client/OneShot> option at client creation time.
(L<a request directly on the client|WEC::FastCGI::Client/request> always
sets one_shot true).

=item $old_one_shot = $connection->one_shot($boolean)

Changes the one_shot state of the connection, returns the old state.

=back

=item Server side methods

=over

=item X<type>$connection->type

Returns the type of the last record the server received on this connection.
Normally that will be same as the type of the event that caused the current
callback. Normally you don't care about this method unless you are using
the L<UnknownManagementRecord|WEC::FastCGI::Server/UnknownManagementRecord>
or L<UnknownApplicationRecord|WEC::FastCGI::Server/UnknownApplicationRecord>
callbacks to implement protocol extensions, and even there the type will
already be one of the arguments.

=item X<unknown>$connection->unknown(?$type?)

The server can use this method to tell the client that it doesn't know
about a certain management record type. If you don't give a $type parameter,
it will default to the type of the last record the server received on this
connection. You would normally only use this method if you are implementing
your own
L<UnknownManagementRecord|WEC::FastCGI::Server/UnknownManagementRecord>
callback.

=back

=item Methods that make sense on both sides

=over

=item X<close>$connection->close

Impolitely directly closes a connection. It will still cause the appropiate
callbacks to run if they exist (L<Abort|WEC::FastCGI::Server/Abort> for a
server, L<RequestEnd|WEC::FastCGI::Client/RequestEnd> for a client and
Close for both), but no more records will be exchanged with the other side.

=item X<peer_address>$address = $connection->peer_address

Returns the address of the other side in the same packed format as would be
returned by L<getpeername|perlfunc/getpeername>.

=item X<handle>$handle = $connection->handle

Returns the socket represented by this $connection object.

=item X<user_data>$user_data = $connection->user_data

With every $connection object you can associate one scalar of user data
(default undef). This method returns that user data.

=item $old_user_data = $connection->user_data($new_user_data)

Set new user data, returning the old value.

=item $conn->send($type, $request_id, $content)

This is an internals method for people extending the protocol. It will queue
a record of type $type and content $content for the request identified by
$request_id on this $connection. You can use the
L<id method|WEC::FastCGI::Request/id> on a request to get its id, while an id
of 0 is not associated with any specific request but signifies a management
record.

=item X<object_count>my $nr_connections = WEC::FastCGI::Connections::object_count()

This function is mainly meant for debugging leaks. Whenever a connection object
is created, an internal count is increased, and whenever one gets destroyed an
internal count is decreased. This class method allows you to retrieve the
current count, after which you can compare that with what you expected.

=back

=back

=head1 EXPORT

None by default, but the following ones may be requested:

=over

=item object_count

=back

=head1 SEE ALSO

L<WEC::FastCGI::Client>,
L<WEC::FastCGI::Server>,
L<WEC::FastCGI::Request>,
L<http://www.fastcgi.com/devkit/doc/fcgi-spec.html>

=head1 AUTHOR

Ton Hospel, E<lt>WEC-FastCGI@ton.iguana.beE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2004 by Ton Hospel

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

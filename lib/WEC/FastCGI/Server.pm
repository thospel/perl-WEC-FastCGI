package WEC::FastCGI::Server;
use 5.006;
use strict;
use warnings;
use POSIX qw(ENOTCONN);
use Carp;
use Socket qw(inet_aton inet_ntoa);

use WEC::Socket qw(is_tcp);
use WEC::FastCGI::Constants qw(:Roles);
use WEC::FastCGI::Connection;

our $VERSION = "0.01";

use base qw(WEC::Server);

our @EXPORT_OK = qw(check_fcgi);

sub check_fcgi(;*) {
    # $handle should be a listening unix/inet domain socket
    croak "This is not an FCGI connection" if
        getpeername(my $fh = @_ ? shift : \*STDIN) || $! != ENOTCONN;
}

my $default_options = {
    %{__PACKAGE__->SUPER::default_options},
    TieStdin	=> 1,
    TieStdout	=> 1,
    TieStderr	=> 1,
    Params	=> undef,
    Data	=> undef,
    Stdin	=> undef,
    ParamsEof	=> undef,
    DataEof	=> undef,
    StdinEof	=> undef,
    FakeEnv	=> 1,
    FakeCgi	=> 1,
    UnknownManagementRecord	=> undef,
    UnknownApplicationRecord	=> undef,
    RequestBegin	=> undef,
    IpAccept	=> undef,
    Values	=> undef,
    CgiClass	=> "WEC::FastCGI",
};

sub default_options {
    return $default_options;
}

sub connection_class {
    return "WEC::FastCGI::Connection";
}

sub init {
    my ($server, $params) = @_;

    $server->{handles} = [\*STDIN] unless defined($server->{handles});
    $server->{handles} = [$server->{handles}] unless ref $server->{handles} eq "ARRAY";
    check_fcgi($_) for @{$server->{handles}};

    my $options = $server->{options};
    if ($options->{IpAccept} && !ref($options->{IpAccept})) {
        if ($options->{IpAccept} =~ /^env/i) {
            if (defined($ENV{FCGI_WEB_SERVER_ADDRS})) {
                $options->{IpAccept} = [split /\s*,\s*/, 
                                        $ENV{FCGI_WEB_SERVER_ADDRS}];
            } else {
                delete $options->{IpAccept};
            }
        }
    }

    if ($options->{IpAccept}) {
        croak "IpAccept is not an array reference" unless
            ref($options->{IpAccept}) eq "ARRAY";
        # Maybe drop this sanity check and give the user what he asked for...
        $server->{IsTcp} = 0;
        for (@{$server->{handles}}) {
            if (is_tcp($_)) {
                $server->{IsTcp} = 1;
                last;
            }
        }
        croak "IpAccept list on non-tcp socket" unless $server->{IsTcp};
        croak "IpAccept list is empty" unless @{$options->{IpAccept}};
        $options->{IpAccept} = { map {inet_aton($_) || croak("Bad hostname '$_'") => 1} @{$options->{IpAccept}}};
    }
}

sub ip_accept {
    my $server = shift;
    return $server->{options}{IpAccept} &&
        [map inet_ntoa($_), keys %{$server->{options}{IpAccept}}];
}

1;

__END__

=head1 NAME

WEC::FastCGI::Server - Event driven FastCGI server

=head1 SYNOPSIS

 use WEC qw(api=1 loop);

 WEC->init;
 $server = WEC::FastCGI::Server->new(%options);
 # Valid option keys:
 #
 # Handle:	Socket to listen on
 # Values:      get_values results
 # Accepting:   accept connections
 # PreAccept:	Connection pending callback
 # Accept:	Connection accepted callback
 # RequestBegin: Request start callback
 # TieStdin:	Fake STDIN (default)
 # Stdin:	Stdin callback
 # StdinEof:	Stdin finished callback
 # Data:	Data callback
 # DataEof:	End of data callback
 # TieStdout:	Fake STDOUT (default)
 # TieStderr:	Fake STDERR (default)
 # Params:	Parameter callback
 # ParamsEof:	End of parameters callback
 # FakeEnv:	Fake environment (default)
 # FakeCgi:	Fake CGI api (default)
 # CgiClass:	CGI class to use (defaults to WEC::FastCGI)
 # Abort:	Abort callback
 # UnknownManagementRecord:	Unknown management record callback
 # UnknownApplicationRecord:	Unknown request record callback
 # Close:	Close callback
 # IpAccept:	IP whitelist
 # AutoClean:	Evaporate when last reference is dropped (default)

 my $array_reference = $server->IpAccept;
 my $accepting = $server->accepting;
 my $old = $server->accepting($boolean);
 my $nr_connections = $server->connections;
 my @connections    = $server->connections;
 my $handles = $server->handles;
 $server->abort;

 my $nr_servers = WEC::FastCGI::Server::object_count();
 WEC::FastCGI::Server::check_fcgi($handle);

 loop();

=head1 DESCRIPTION

WEC::FastCGI::Server is a class to help you implement an event based
FastCGI server (L<http://www.fastcgi.com/>). The Server is the side
with the listening socket waiting for requests. The client side sends
requests and waits for answers, and will usually be a webserver with
FastCGI support. for example apache (L<http://www.apache.org/>) with
mod_fastcgi (L<http://www.fastcgi.com/mod_fastcgi/docs/mod_fastcgi.html>).
Or you could even make something based on
L<WEC::FastCGI::Client|WEC::FastCGI::Client>.

This class uses L<WEC|WEC> as an eventloop wrapper, so it should be easy to
integrate into whichever event kernel you use.

From the main eventloop various callbacks will then be activated for
the events you've shown interest in. You can then do some action and
return to the eventloop. Whenever these docs speak about handling events,
remember that all of these are dispatched from the mainloop, so nothing will
happen for any pending events until you return from a callback to the main
eventloop. If however you do this properly and always return quickly, you
will effectively be able to handle multiple requests "simultanously".

Main events will first of all be when some client makes a connection to your
server. This connection will be represented using a
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object. See the
methods there for how to control such a connection.

Next, on a connection you can receive requests for work, which will be
represented by a L<WEC::FastCGI::Request|WEC::FastCGI::Request> object.
See the methods there for all things you can do with a request.

To ease usage and to make porting CGI programs easier, by default you
will be called like a normal CGI program in the main request handler
callback (to the extent possible). And to make the situation even more
familiar to perl CGI programmers, you will in fact also get a L<CGI|CGI>
object (of derived class L<WEC::FastCGI|WEC::FastCGI>) representing
the request which you can mostly use in the normal way.
In particular, STDIN, STDOUT, STDERR and %ENV will try to work as expected.

Quite often there will be a process manager (like mod_fastcgi) controlling
your server. This manager can send signals to your application, which is
then supposed to do the appropiate thing. By default loading of
L<WEC::FastCGI::Server|WEC::FastCGI::Server> gives you signal handlers
which probably means your program will simply exit if it receives an
unhandled signal. (the WEC::FastCGI event kernel will already have set sigpipe
to ignore for you).

=head1 EXAMPLE1

Here's the complete code for a very basic WEC::FastCGI program which
you can use under mod_fastcgi:

    #! /usr/bin/perl -wT
    use strict;
    # We'll use raw select as event handler
    use WEC qw(api=1 loop);
    # After that load the main module
    use WEC::FastCGI::Server;
    # like use CGI qw/:standard/
    use WEC::FastCGI qw/:standard/;

    # Set up a fcgi server
    WEC->init;
    # No Handle is given, so it will use STDIN
    # AutoClean must be turned off since we don't keep a
    # reference to the server around.
    WEC::FastCGI::Server->new(AutoClean	=> 0,
                              StdinEof	=> \&process);
    });
    # Start the mainloop
    loop;
    # This won't actually be reached
    exit;

    # Just like a normal CGI handler, nothing special here
    sub process {
        # First argument is the Request object, second the CGI object and
        # third is the stdin buffer. They won't be used in this example.
        # my ($r, $q, $stdin) = @_;

        # STDOUT is tied, so it can be used normally
        print(header,
              start_html("WEC::FastCGI demo"),
              h1("Environment"));

        # %ENV is faked too
        # (and DIFFERENT from the calling environment during this callback)
        print(escapeHTML("$_=$ENV{$_}"), br, "\n") for sort keys %ENV;

        # Parameters are available in the normal way
        print(h1("Parameters"));
        for (sort(param())) {
            my $val = param($_);
            print(escapeHTML("$_=$val"), br, "\n");
        }
        print(end_html);
    }

=head1 METHODS

All methods will throw an exception in case of failure.

=over

=item X<new>$server = WEC::FastCGI::Server(%options)

This creates a new WEC::FastCGI::Server object whose exact details
depend on the given options. It will set up a handler for the listening
socket given as one of the arguments waiting for connections. When one
comes in, this handler will accept it and build a
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object around the
accepted handle. This in turn will have handlers registered to process
requests from the client. When a work request comes in, a
L<WEC::FastCGI::Request|WEC::FastCGI::Request> object will be created
which will consolidate all work for that particular request. It will also
create a L<WEC::FastCGI|WEC::FastCGI> object (derived from L<CGI|CGI>)
which will allow you to interact with the request as if it's a normal
CGI request.

Notice that at any given time multiple connections can be active
(only restricted by operating system resources) and on each
of these multiple requests can be active (protocol maximum of 65535).

The basic creation call is:

    $server = WEC::FastCGI::Server->new(...);

but should only be done after a L<WEC->init|WEC/"init"> has been done. 
It's also ok to start servers if the main eventloop has already started
(e.g. demand based in a callback).

Many options allow you to change the exact functionality. All of them
must be given as key/value pairs.

=over

=item X<Handle>Handle => $fh

=item X<Handle>Handle => \@fhs

This is the socket (or a reference to a list of sockets) on which the server 
will wait for events. It's supposed to already have gotten any needed 
L<bind|bind(2)> and L<listen|listen(2)>.
(See L<unix|WEC::FastCGI::Socket/"unix"> and
L<inet|WEC::FastCGI::Socket/"inet"> in
L<WEC::FastCGI::Socket|WEC::FastCGI::Socket> for one convenient way to
create such a socket).

If this option is not given, it defaults to using STDIN (a setup where
the FastCGI processmanager starts servers with a listening unix domain
socket opened as STDIN is quite common).

=item X<Values>Values => $hash_reference

If given a value, it defines name/value pairs that will be used to
answer a client L<get_values |WEC::FastCGI::Connection/"get_values"> request.
The values will be returned as strings. If the client asks for a name not in
the hash, it won't get an answer for that name, but an undef value will be
sent back as an empty string and likely cause a warning.

If this option is not given, a default will be supplied:

    {MAX_CONNS	=> 100,
     MAX_REQS	=> 100,
     MPXS_CONNS	=> 1}

=item X<Accepting>Accepting => $boolean

If given a true value, the server will start accepting connections
as soon as the main eventloop gets started, otherwise there will be
no handling of incoming connections (yet). The socket will presumably
already have had a L<bind|perlfunc/"bind"> and L<listen|perlfunc/"listen">
applied to it though, so the operating system might already be pre-accepting
incoming connections for you, they just won't get an
L<accept|perlfunc/"accept">. You can later query or change this state using
the L<accepting|"accepting"> method.

Defaults to true if not given.

=item X<PreAccept>PreAccept => $function_reference

Sets up $function_reference to be called when a read event happens on
L<the listening socket|"Handle">, which means a connection attempt is
pending. Just before doing the accept it will then call:

    $function_reference->($server);

A typical use is to then check if the currently running server code has
been updated, and to reload the code. If you let the new code use the
same listening socket, the connection will still be pending and cause
a new read-event leading to an accept (possibly first running the
L<PreAccept|"PreAccept"> callback again).

Take care to not to lose requests on the reload. A typical strategy is to
fork and stop accepting new connections in the child, but keep servicing
the old connections until they are empty and can all be closed. In the parent
you exec the new code and start accepting new connections. The old connections
will be closed in the new process space since accepted connections will have
close on exec set.

=item X<Accept>Accept => $function_reference

Sets up a function reference to be called after a connection is
B<succesfully> accepted (so this callback will not be called if the
accept failed for some reason or the connection gets rejected due to
not matching the L<client whitelist in IpAccept|"option_IpAccept">). The call
will be done like this:

    $function_reference->($server, $connection);

where $server is the server object, $connection is the
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object representing
the newly accepted connection.

This callback forms a pair with the L<Close callback|"Close"> which gets
called when a connection closes. You can use these two to track creation
and destruction of connections.

=item X<RequestBegin>RequestBegin => $function_reference

This callback will be called whenever a work request is received on a
connection. It will be called like:

    $function_reference->($connection, $request);

$connection is L<the connection|WEC::FastCGI::Connection> on which the
request came in, while $request is the just created
L<request object|WEC::FastCGI::Request>.

This is the earliest callback you can get for a request, and you can use it
to do some preparation work. This is generally B<not> the logical place to
build your answer since you most likely haven't yet received any request
parameters (like params, stdin and data), and for normal roles you aren't
even B<allowed> to start answering yet.

=item X<TieStdin>TieStdin => $boolean

Giving a true value to this tag implies that all callbacks that can have a
L<CGI object|"CgiClass"> as their second parameter will be called with
STDIN L<tied|perltie> to the input stream coming from the client.

Every read from this tied STDIN will consume the corresponding data from
the internal stdin buffer. If you try to read more than is available,
you'll get the part that is available, or the normal EOF behaviour for the
read method you used if nothing is available. If however after such an EOF
on a later callback that has STDIN tied there's more stdin data again,
you can just continue reading at that point.

So a read on tied STDIN will never be blocking, but you cannot use only EOF
as an indication that the inputstream has finished. For that you can use the
L<StdinEof|"StdinEof"> callback or use extra knowledge you have about the
way the client sends things.

The tie is set up just before and torn down just after each callback.

This option defaults to true. You have to explicitely turn it off if you don't
want it.

=item X<Stdin>Stdin => $function_reference

Instead of waiting for some callback and then look what arrived on stdin in
the mean time, you can also ask to be notified immediately every time a chunk
of data arrives for stdin by using this callback. This allows start processing
early. The call will be:

    $function_reference->($request, $cgi, $input_chunk);

where $request is the L<request object|WEC::FastCGI::Request> for the request
on which this stdin chunk was sent, $cgi is the L<CGI object|"CgiClass"> and
the string $input_chunk is the new piece of input that just
arrived. B<After> this callback the $input_chunk will be appended to the
internal stdin buffer, so if you really want to, you can modify it inside the
callback to control what will get added. If the stdin stream is potentially
infinite and you process input already during this callback, it's a good idea
to set $input_chunk to the empty string, otherwise the stdin buffer can grow
without bounds. All this also implies that the newly arrived input is not yet
available on tied STDIN either.

=item X<StdinEof>StdinEof => $function_reference

Called when the client closes the request stdin stream like:

    $function_reference->($request, $cgi, $input_buffer);

where $request is the L<request object|WEC::FastCGI::Request> for the request
on which stdin got closed, $cgi is the L<CGI object|"CgiClass"> and
$input_buffer is an alias to the buffer containing the collected stdin.
You can process stdin from that or use L<the STDIN tie|"Stdin"> if that is
set up. Since it's an alias to the real buffer, any changes to this argument or
reading from STDIN will influence each other. In particular, if you use
the L<Fake CGI emulation|"FakeCgi">, the creation of the CGI object will likely
already have drained STDIN.

Since closing stdin is normally the last thing that the client does for
a normal CGI request (a responder), this is a very natural place to
do your final processing of a CGI request.

Just after the stdin close any
L<RESPONDER|WEC::FastCGI::Request/"RESPONDER"> request will get
automatically ended (even if you did not set up this callback) unless you
L<extended the request|WEC::FastCGI::Request/"extend">. See
L<EXAMPLE2|"EXAMPLE2"> for an extended request.

=item X<Data>Data => $function_reference

If you give this callback, it will be called whenever a chunk arrives on the
"data" stream. This stream is a lot less familiar to CGI programmers since
it doesn't exist there, it's only used by the
L<FILTER role|WEC::FastCGI::Request/"FILTER">. The call will be:

    $function_reference->($request, $cgi, $data_chunk);

where $request is the L<request object|WEC::FastCGI::Request> for the request
on which this data chunk was sent, $cgi is the L<CGI object|"CgiClass">  and
the string $data_chunk is the new piece of
data that just arrived. B<After> this callback the $data_chunk will be
appended to the internal data buffer, so if you really want to, you can
modify it inside the callback to control what will get added. If the data
stream is potentially infinite and you process data already during this
callback, it's a good idea to set $data_chunk to the empty string, otherwise
the data buffer can grow without bounds.

=item X<DataEof>DataEof => $function_reference

Called when the client closes the request data stream like:

    $function_reference->($request, $cgi, $data_buffer);

where $request is the L<request object|WEC::FastCGI::Request> for the request
on which the data stream got closed, $cgi is the L<CGI object|"CgiClass"> and
$data_buffer is an alias to the buffer containing the collected data.

Since closing the data stream is normally the last thing that the client does
for a normal FastCGI filter, this is a very natural place to do your final
processing of a filter request.

Just after the data stream close any
L<FILTER|WEC::FastCGI::Request/"FILTER"> request will get
automatically ended (even if you did not set up this callback) unless you
L<extended the request|WEC::FastCGI::Request/"extend">.

=item X<TieStdout>TieStdout => $boolean

Giving a true value to this tag implies that all callbacks that can have a
L<CGI object|"CgiClass"> as their second parameter will be called with
STDOUT L<tied|perltie> to the input stream coming from the client.

Anything you send to STDOUT will be appended to the stdout buffer. It's not
necessarily sent as soon as you get back to the mainloop though. It works in
the normal stdio manner, a chunk is sent out only if the buffer grows beyond
a certain size, or if you flush the stream. A close implies an automatic flush
and the end of a request implies a close.

The tie is set up just before and torn down just after each callback.

This option defaults to true. You have to explicitely turn it off if you don't
want it.

=item X<TieStderr>TieStderr => $boolean

Giving a true value to this tag implies that all callbacks that can have a
L<CGI object|"CgiClass"> as their second parameter will be called with
STDERR L<tied|perltie> to the input stream coming from the client.

Anything you print to STDERR will be appended to the stderr buffer. The default
maximum buffer size however is empty, so it will immediately get flushed
and queued to be sent to the client (which will happen when you return to the
main eventloop).

The tie is set up just before and torn down just after each callback.

This option defaults to true. You have to explicitely turn it off if you don't
want it.

=item X<Params>Params => $function_reference

If you give this callback, it will be called whenever a chunk of parameter
data arrives. The call will be:

    $function_reference->($request, $cgi, $params_chunk);

where $request is the L<request object|WEC::FastCGI::Request> for the request
on which this parameter chunk was sent, $cgi is the L<CGI object|"CgiClass">
and the string $params_chunk is the new piece of
parameters that just arrived. B<After> this callback the $params_chunk will be
appended to the internal params buffer, so if you really want to, you can
modify it inside the callback to control what will get added. If the parameter
stream is potentially infinite and you process parameters already during this
callback, it's a good idea to set $params_chunk to the empty string, otherwise
the params buffer can grow without bounds.

These chunks are not very convenient to use since they are still encoded
in the internal FastCGI protocol form, and the chunks don't even come in
on parameter boundaries. Most of the time it's more convenient to just let
them accumulate and process them with L<ParamsEof|"ParamsEof"> when they have
all arrived. Or don't even do that and just access the parameter hash at
any point when are sure the parameter end of file has arrived. After that
point the parameter hash will be available in %ENV in all callbacks that
can have a L<CGI object|"CgiClass"> as their second parameter (unless you
turned off L<FakeEnv|"FakeEnv">). Or you can use the
L<params method|WEC::FastCGI::Request/"params"> on the
L<request object|WEC::FastCGI::Request>.

=item X<ParamsEof>ParamsEof => $function_reference

Called when the client closes the request parameter stream like:

    $function_reference->($request, $cgi, $params_hashref);

where $request is the L<request object|WEC::FastCGI::Request> for the request
on which the parameter stream got closed, $cgi is the L<CGI object|"CgiClass">
and $params_hashref is an alias to the hashreference in which the decoded
parameterlist is stored.

The parameter hash will also be available in %ENV here and in all later
callbacks that can have a L<CGI object|"CgiClass"> as their second parameter
(unless you turned off L<FakeEnv|"FakeEnv">).

Since closing the parameter stream is normally the last thing that the client
does for a normal FastCGI authorizer, this is a very natural place to do your
final processing of an authorizer request.

Just after the parameter stream close any
L<AUTHORIZER|WEC::FastCGI::Request/"AUTHORIZER"> request will get
automatically ended (even if you did not set up this callback) unless you
L<extended the request|WEC::FastCGI::Request/"extend">.

=item X<FakeEnv>FakeEnv => $boolean

Giving a true value to this tag implies that all callbacks that can have a
L<CGI object|"CgiClass"> as their second parameter will get a faked %ENV set
up just before that callback which gets removed again after the callback.
This %ENV will contain the decoded parameters if this happens after the
end of the parameter stream, or be empty otherwise. The real environment of
the program will therefore not be directly accessable at this moment.
If you in some way exec a program at this point (e.g. through
L<exec|perlfunc/"exec"> or a L<pipe open|perlfunc/"open">, this fake
environment will be lost and you'll see that the B<real> environment was never
changed, since you'll get the environment of before the callback again.

If you give a false value to this tag, %ENV will not be faked and just be
the standard one, supposedly representing the real environment. You can still
use the L<params method|WEC::FastCGI::Request/"params"> method on the
L<request object|WEC::FastCGI::Request> to access the parameters.

This option defaults to true. You have to explicitely turn it off if you don't
want it.

=item X<FakeCgi>FakeCgi => $boolean

Giving a true value to this tag implies that all callbacks that can have a
L<CGI object|"CgiClass"> as their second parameter will also see a global
varible $CGI::Q with the same value. This is needed if you want to use
the methods from the L<CGI module|CGI> without needing to specify the CGI
object.

If you give a false value, the L<CGI object|WEC::FastCGI> is still created
(see L<CgiClass> if you don't want a CGI object at all), it's just not made
available in $CGI::Q, so if you want to use the CGI object you'll need to
use the object oriented style (the object will in the second parameter of
most of the callbacks where you'd like to it).

This option defaults to true. You have to explicitely turn it off if you don't
want it.

=item X<CgiClass>CgiClass => $string

This $string is the class of the CGI object that will be created for the
callbacks that can take a CGI object as their first argument. It's creation
will actually be delayed until the first such callback gets called for a
particular request, but will then be remembered and re-used for all callbacks
for that request. The object is created by simply calling:

    $string->new();

Notice that the exact creation moment matters. If for example a CGI argument
callback gets activated before the end of the stdin stream the L<CGI|CGI>
module will try to build its param() set before all parameters have actually
arrived, and you will probably miss some or all even if they arrive later,
since the CGI object won't get updated anymore.

Notice also that the creation of the CGI object will happen after
L<FakeEnv|"FakeEnv"> and L<TieStdin|"TieStdin"> have been satisfied. If you
don't use these, it's up to you to make sure that the CgiClass can handle
that situation.

The reference to the cgi object is dropped when the
L<end method|WEC::FastCGI::Request/"end"> is called on the
L<request object|WEC::FastCGI::Request>. Normally that will be the last
reference and the CGI object gets cleaned up (if it was an explicit call to
L<end|WEC::FastCGI::Request/"end"> during a callback and $CGI::Q also
has a reference, the cleanup may only happen when the callback returns and
$CGI::Q gets restored).

If you give this tag a false value, no CGI object will be created.
All callbacks that can get a CGI object as their first argument will now get
undef instead, and the callbacks will not get a localized $CGI::Q (it will
keep its global state, which probably will be that it does not exist).

The CGI object class defaults to L<WEC::FastCGI|WEC::FastCGI>.

=item X<Abort>Abort => $function_reference

This callback is called when the server receives an explicit abort request from
the client, the server did an implicit abort (typically as a result of
receiving an unknown application record type, see
L<UnknownApplicationRecord|"UnknownApplicationRecord">) or the connection over
which this request is running gets closed for any reason before the request got
properly finished (typically because the client got killed). You should return
a number representing the application returncode (it will be returned as an
unsigned 32-bit integer). Possibly you also want to do some private cleanup at
this point. The call will look like (notice that it is not a callback that can
have a CGI object as first argument).

    $return_code = $function_reference->($request, @possibly_more_args);

The Abort callback will actually only be called if this request ever caused
a callback of the type that can have a CGI object as second parameter.
Basically if you did not know about the request, you also will not be notified
that it got aborted. If no callback is done, the return_code will be 2**32-1.

After returning from this callback the stdout and stderr buffers will be
cleared (so their contents will not be flushed on the final close) and the
request will be ended (even if it was
L<extended|WEC::FastCGI::Request/"extend">).

=item X<UnknownManagementRecord>UnknownManagementRecord => $function_reference

This callback will be called whenever a connection gets a management record
of a type that is not handled (all standard FastCGI records have a handler,
so either the client is trying to use some extended protocol or there is a
bug). You are then either supposed to handle it, or report to the client that
you won't handle this type of record (typically using the
L<unknown method|WEC::FastCGI::Connection/"unknown"> of
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection>.

The callback gets called like:

    $function_reference->($connection, $type, $data);

where $connection is the
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> for which the
management record was received, $type is the record type and $data is the
data associated with this management record.

If this callback is not given, the default action is to L<warn|perlfunc/"warn">
(notice that this is not a callback with STDERR tied, so the warning will
probably end up wherever the main program STDERR goes) and next to to call

    $connection->unknown($type);

telling the client the request type is unknown.

This is the natural place to dispatch your own management record extensions.

=item X<UnknownType>UnknownApplicationRecord => $function_reference

This callback gets called whenever an unknown record type is received meant
for a particular running request (all standard FastCGI records have a handler,
so either the client is trying to use some extended protocol or there is a
bug). You should either handle the record or do something appropiate like
aborting the request.

The default action if this callback isn't given is to L<warn|perlfunc/"warn">
(notice that this is not a callback with STDERR tied, so the warning will
probably end up wherever the main program STDERR goes). Next the request gets
aborted as if the client sent an abort (including a possible call to the
L<Abort callback|"Abort"> and sending back an application returncode).

This is the natural place to dispatch your own application record extensions.

=item X<Close>Close => $function_reference

This callback gets called when an accepted connection gets closed for
any reason. The call will be like:

    $function_reference->($server, $connection, @reason);

where $server is the server object, $connection is the
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object representing
the just closed connection and @reason is either empty if the user did an
explicit L<close method|WEC::FastCGI::Connection/"close"> call or contains
one string with the close reason (for exemple "eof").

This callback forms a pair with L<Accept|"Accept"> so that with these two you
can track the creation and destruction of connections.

=item X<option_IpAccept>IpAccept => $array_reference

Gives a whitelist of hosts that are allowed to connect. Implicitely also
demands that the listening socket is a tcp socket. The hosts are converted to
a list of ips at the moment of the new call, so if a name gets another ip
associated with it later, it won't notice. Name resolution is done with
blocking DNS calls, so only pass ip-addresses if you don't want any blocking.

For example to only accept connections from localhost and 1.2.3.4, you'd use:

    IpAccept => ["localhost", "1.2.3.4"];

If a host not on this list connects, the PreAccept callback will be called
(if there is one), but after the accept it will be seen to be an invalid host
and the connection gets closed. There won't be any
L<connection object|WEC::FastCGI::Connection> created and there won't be
any L<Accept|"Accept"> callback.

If this option is not given or has a false value, there will be no host
whitelist and every host that succeeds in connecting to your listening socket
will be accepted (the default case).

=item X<option_IpAccept_env>IpAccept => "env"

"env" is a special value for the IpAccept option for compatibility with the
FastCGI specification. It means the program will look at the current value
of the FCGI_WEB_SERVER_ADDRS environment variable. If that one is set, it will
be interpreted as a comma separated list of hostnames that are allowed to
connect. If the environment variable is not set at all, every host is again
allowed to connect.

Take care if you ever use this option in a callback with a fake environment
set (see L<FakeEnv|"FakeEnv">) because it will use whatever is in the current
%ENV, not the "real" environment.

=item X<AutoClean>AutoClean => $boolean

WEC::FastCGI::Server objects have circular references internally, so
that in normal circumstances they would never get cleaned up until you
explicitely L<abort|"abort"> them or on final interpreter shutdown. By giving
this option it will abort itself as soon as the last external to a value in the loop goes away. This will happen even if there are connections on which new
work can arrive, or even if there is already work queued.

This option defaults to true, but in the rather common case that you want
to have the server running indefinitely and don't keep a main server reference
(like in the L<example|"EXAMPLE1">, you probably want to set it to false.
You can still decide to do an L<abort|"abort"> in some callback in that case
and cause a cleanup (the needed server object you can if necessary derive from
the L<connection|WEC::FastCGI::Connection> or
L<request|WEC::FastCGI::Request> object).

=back

=item X<ip_accept>my $array_reference = $server->ip_accept

If there is a server whitelist, it returns a reference to an array of
strings in some unspecified order, each of which is an ip-address.
Returns undef if there is no whitelist (see also the
L<IpAccept option|"option_IpAccept">).

So after this piece of code:

    $server->new(IpAccept => ["localhost", "1.2.3.4"], Handle => ...);
    $allow = $server->ip_accept;

$allow will normally contain ["127.0.0.1", "1.2.3.4"] or
["1.2.3.4", "127.0.0.1"].

=item X<accepting> my $accepting = $server->accepting

Returns a true value if the server is currently willing to accept new
connections (the default on creation)

=item my $old = $server->accepting($boolean)

Sets the accepting state of the server to the given boolean and returns the
old value.

This is typically used for a graceful restart which you can do by using a,
L<fork|perlfunc/"fork"> and L<exec|perlfunc/"exec"> a new server reusing the
current listening handle in the parent. In the child you then stop accepting
new connections, and exit when the last active connection stops (so the main
server remains running with the same process id).

=item X<connections> my $nr_connections = $server->connections

In scalar context, returns the number of connections to the server.

=item my @connections = $server->connections

In list context, returns a list of connections to the server. Each connection
will be represented by a
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object.

=item X<handle>my $handles = $server->handles

Returns a reference to an array of handles on which this server is listening.

=item X<abort>$server->abort

This will cause the server to forget about the listening socket and do
a close on all connections (which in turns causes an abort on all requests).
This will also cause the internal circular references to be cleaned up, so
all objects will also be freed when their last references go away.

=item X<object_count>my $nr_servers = WEC::FastCGI::Server::object_count()

This function is mainly meant for debugging leaks. Whenever a server object
is created, an internal count is increased, and whenever one gets destroyed an
internal count is decreased. This class method allows you to retrieve the
current count, after which you can compare that with what you expected.

You should not need this normally.

=item X<check_fcgi>WEC::FastCGI::Server::check_fcgi($handle)

An internal function that allows you to check if a given handle is an
unconnected socket. If not, it will raise an exception. This check is
already done when you L<pass a handle to new|"Handle">, so normally you
will not need this.

=back

=head1 EXPORT

None by default, but the following ones may be requested:

=over

=item object_count

=item check_fcgi

=back

=head1 EXAMPLE2

Here is a more complex example using request extension that will output one
line every second. That makes it easy to do more than one request at the same
time and observe if they run well together. If you are trying this example
under mod_fastcgi
(L<http://www.fastcgi.com/mod_fastcgi/docs/mod_fastcgi.html>), don't forget
to give the -flush option to the directive you use to run the application,
otherwise you won't see anything until the whole output has been
sent. Also notice that some browsers can only have one stream to a given exact
URL at the same time, so in that case either use two browsers, or make sure
the URLs are not exactly the same, for example by giving different request
parameters.

    #! /usr/bin/perl -wT-
    use strict;
    use WEC qw(api=1 loop);
    use WEC::FastCGI::Server;
    use WEC::FastCGI qw/:standard/;

    my $sleep = 1;

    WEC->init;
    WEC::FastCGI::Server->new(AutoClean	=> 0,
                              StdinEof	=> \&process);
    loop;
    exit;

    sub process {
        my $req = shift;
        # Make stdout autoflushing
        $req->stdout_buffer(0);
        # Send out headers
        print(header,
              start_html("WEC::FastCGI demo with slow output"),
              h1("Environment"));
        # No automatic end() after returning
        $req->extend;
        # Start printing an environment line in $sleep seconds
        $req->add_alarm($sleep, \&env_line);
    }

    sub env_line {
        my $req = shift;
        if (my ($key, $val) = each %ENV) {
            # Print next %ENV entry as long as there is one
            print(escapeHTML("$key=$val"), br, "\n");
            # Set up an alarm for the next ENV entry
            $req->add_alarm($sleep, \&env_line);
        } else {
            # Otherwise switch to printing parameters
            print(h1("Parameters"));
            my @param_names = sort(param());
            # Set up an alarm to start printing the parameters one by one
            $req->add_alarm($sleep, \&param_line, \@param_names);
        }
    }

    sub param_line {
        my ($req, $q, $param_names) = @_;
        if (defined(my $name = shift @$param_names)) {
            # Print next parameter as long as there is one
            my $val = param($name);
            print(escapeHTML("$name=$val"), br, "\n");
            $req->add_alarm($sleep, \&param_line, $param_names);
        } else {
            # It's all over now. End the html
            print(end_html);
            # And end the request, you MUST do this if you extend $req
            $req->end;
        }
    }

=head1 SEE ALSO

L<WEC>,
L<WEC::Socket>,
L<WEC::FastCGI::Client>,
L<WEC::FastCGI::Connection>,
L<WEC::FastCGI::Request>,
L<WEC::FastCGI>,
L<FCGI>,
L<CGI::Fast>,
L<http://www.fastcgi.com/devkit/doc/fcgi-spec.html>

=head1 AUTHOR

Ton Hospel, E<lt>WEC-FastCGI@ton.iguana.beE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2004 by Ton Hospel

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

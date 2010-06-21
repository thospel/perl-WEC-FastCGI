package WEC::FastCGI::Client;
use 5.006;
use strict;
use warnings;
use Carp;

use WEC::FastCGI::Connection;

our $VERSION = "1.000";

use base qw(WEC::Client);

my $default_options = {
    %{__PACKAGE__->SUPER::default_options},
    GetValuesResult => undef,
    OneShot	=> undef,
    StderrEof	=> undef,
    StdoutEof	=> undef,
    Stdout	=> undef,
    Stderr	=> undef,
    EofIsEnd	=> undef,
    UnknownType	=> undef,
    RequestEnd	=> undef,
};

sub default_options {
    return $default_options;
}

sub connection_class {
    return "WEC::FastCGI::Connection";
}

sub init {
    my ($client, $params) = @_;

    $client->{paths} = \my %paths;
    if (defined(my $paths = delete $params->{Paths})) {
        $paths{$_}++ for ref($paths) ? @$paths : $paths;
    }
    $client->{pids}  = \my %pids;
    if (defined(my $pids  = delete $params->{Pids})) {
        $pids{$_}++ for ref($pids) ? @$pids : $pids;
    }
}

sub DESTROY {
    my $client = shift;
    # Avoid trying to unlink linux abstract unix sockets
    for my $path (keys %{$client->{paths}}) {
        $path !~ s!^unix://!!i || $path =~ /^\x00/ || unlink($path) ||
            die "Could not unlink $path: $!";
    }
    for my $pid (keys %{$client->{pids}}) {
        kill(15, $pid) || warn("Could not kill $pid\n");
    }
    $client->SUPER::DESTROY;
}

sub add_path {
    my $client = shift;
    $client->{paths}{$_}++ for @_;
}

sub add_pid {
    my $client = shift;
    $client->{pids}{$_}++ for @_;
}

sub request {
    my $connection = shift->connect;
    $connection->one_shot(1);
    return $connection->request(@_);
}

1;
__END__

=head1 NAME

WEC::FastCGI::Client - Event driven FastCGI client

=head1 SYNOPSIS

 # Load an event kernel
 use WEC qw(api=1 loop);

 WEC->init;
 $client = WEC::FastCGI::Client->new(%options);
 # valid option keys:
 #
 # Destination:    Name of server socket
 # Paths:       destinations to unlink on destroy
 # Pids:        pids to kill on destroy
 # ConnectFail: Connect failure callback
 # Connect:	Connect callback
 # OneShot:	Only one request per connection
 # Stdout:	Stdout callback
 # StdoutEof:	Stdout finished callback
 # Stderr:	Stderr callback
 # StderrEof:	Stderr finished callback
 # GetValuesResult:	GetValues result callback
 # EofIsEnd:	EOF is interpreted as a formal request end
 # UnknownType:	Server reports unknown management record callback
 # RequestEnd:	Request ended callback
 # Close:	Close callback

 my $connection  = $client->connect(?$destination?);
 my $request     = $client->request(@args);
 my $destination = $client->destination;
 my $nr_connections = $client->connections();
 my @connections    = $client->connections();

 my $nr_clients = WEC::FastCGI::Client::object_count();

=head1 DESCRIPTION

WEC::FastCGI::Client is a class to help you implement an event based
FastCGI client (L<http://www.fastcgi.com/>). The Client is the side
with that makes connections and sends requests, a job normally done by a
webserver with FastCGI support. For example apache (L<http://www.apache.org/>)
uses mod_fastcgi (L<http://www.fastcgi.com/mod_fastcgi/docs/mod_fastcgi.html>)
as its FastCGI client. If you want to implement the side that receives,
processes and answers requests, see modules like
L<WEC::FastCGI::Server|WEC::FastCGI::Server>, L<CGI::Fast|CGI::Fast> or
L<FCGI|FCGI>.

This class uses L<WEC|WEC> as an eventloop wrapper, so it should be easy to
integrate into whichever event kernel you use.

From the main eventloop various callbacks will then be activated for
the events you've shown interest in. You can then do some action and
return to the eventloop. Whenever these docs speak about handling events,
remember that all of these are dispatched from the mainloop, so nothing will
happen for any pending events until you return from a callback to the main
eventloop. If however you do this properly and always return quickly, you
will effectively be able to handle multiple requests "simultanously".

You typically start things rolling by making a connection (represented by
a L<WEC::FastCGI::Connection object|WEC::FastCGI::Connection> to some FastCGI
server and over such connection you can then send one or more requests
(each represented by a L<WEC::FastCGI::Request object|WEC::FastCGI::Request>)
with possibly a set of parameters (key/value pairs), a stdin stream and
a data stream. The server will then process these and after some time
send back a response for each request, which then ends that request.

You can also abort pending requests, or send signals to a server to terminate
or restart it.

=head1 EXAMPLE1

Here is a basic FastCGI client that is able to start a server and send
that a few requests.

    #!/usr/bin/perl -w
    use WEC qw(api=1 loop unloop);
    use WEC::Socket qw(unix spawn);
    use WEC::FastCGI::Client;

    # The server we will run and communicate with
    my $program = "/home/user/fcgi-bin/echo";

    # A fraction of a typical CGI environment
    my %env =
        (REQUEST_METHOD => "GET",
         PATH_INFO      => "/ab",
         PATH_TRANSLATED=> "/var/htdocs/ab",
         DOCUMENT_ROOT  => "/var/htdocs",
         GATEWAY_INTERFACE => "CGI/1.1",
         QUERY_STRING   => "a=b",
         REQUEST_URI    => "/~user/fcgi-bin/echo",
         SCRIPT_FILENAME=> "/home/user/fcgi-bin/echo",
         SCRIPT_NAME    => "/~user/fcgi-bin/echo",
         );

    # Set up a server on a listening socket
    my ($socket, $name) = unix();
    my $pid = spawn($socket, $program);
    close $socket;

    # Set up a client
    my $s = WEC::FastCGI::Client->new
        (Destination    => $name,
         Paths          => $name,
         Pids           => $pid,
         OneShot	=> 1,
         RequestEnd     => \&request_end);

    # Make a connection to the server
    my $conn = $s->connect;
    # Send a request on the connection (stdin is empty)
    $conn->request(\%env, "");
    # Send a second request on an implicit second connection:
    $s->request(\%env, "");
    my $pending_requests = 2;
    # Start the mainloop
    loop;
    exit;

    sub request_end {
        (my WEC::FastCGI::Request $req, my $astatus, my $pstatus) = @_;
        print STDERR "Stdout='$req->{stdout}'\n";
        print STDERR "Stderr='$req->{stderr}'\n";
        print STDERR "End request $req->{id}, app status $astatus, proto status $pstatus\n";
        # Terminate the mainloop when all requests have been handled
        unloop if --$pending_requests == 0;
    }

=head1 METHODS

All methods will throw an exception in case of failure.

=over

=item X<new>$server = WEC::FastCGI::Client->new(%options)

This creates a new WEC::FastCGI::Client object whose exact details depend on
the given options. It will do nothing until you make a connection to some
server (and probably send requests). You can give callbacks that will be
triggered when results start coming back.

Notice that at any given time multiple connections can be active
(only restricted by operating system resources) and on each
of these multiple requests can be active (protocol maximum of 65535). However,
server often have restrictions on the amount of connections they accept and
if they are able to handle multiple simultanous requests (you can use
L<get_values |WEC::FastCGI::Connection/"get_values"> to query the capabilities
of a server). In such cases it might make sense to start up multiple servers.
You can use a single L<WEC::FastCGI::Client object|WEC::FastCGI::Client> to
connect to all these servers, or use many of them, whatever works out most
convenient.

The basic creation call is:

    $server = WEC::FastCGI::Client->new(...);

but should only be done after a L<WEC->init|WEC/"init"> has been done.
It's also ok to start clients if the main eventloop has already started
(e.g. demand based in a callback).

Many options allow you to change the exact functionality. All of them
must be given as key/value pairs.

=over

=item X<Destination>Destination => $string

This option sets the default target address for
L<the connect method|"connect">. The format is C<unix://filename> for
unix domain sockets and C<tcp://host:port> for internet sockets.
This corresponds to the names returned by the
L<WEC::FastCGI::Socket|WEC::FastCGI::Socket>
L<unix|WEC::FastCGI::Socket/"unix"> and L<inet|WEC::FastCGI::Socket/"inet">
functions.

If not given there is no default and L<the connect method|"connect"> will need
a destination argument.

=item X<Paths>Paths => $string

=item Paths => [@strings]

This registers one or more destinations (each in the same format as
the argument of the L<Destination option|"Destination">) as managed by the
client. When the client object gets destroyed, it will try to free that
resource (it will actually only do something (unlink) for non-abstrict
unix domain sockets).

While the server may seem a more logical place to manage a listening socket,
it is quite common for FastCGI clients to set up the listening socket and then
spawn a server with that socket at filedescriptor 0, so the server doesn't even
know the socket name.

=item X<Pids>Pids => $pid

=item Pids => [@pids]

This registers one or more process ids as managed by the client. When the
client object gets destroyed, it will try to terminate each process by
sending it a TERM signal. Currently it doesn't try to track if the signal
actually succeeded in killing the process, but that might change in a future
version.

=item X<ConnectFail>ConnectFail => $function_reference

If the connection attempt triggered by the L<connect method|"connect"> fails
this will be called like:

    $function_reference->($client, $connection, $status, $destination)

where $client is the L<WEC::FastCGI::Client|WEC::FastCGI::Client> on which the
L<connect|"connect"> was done, $connection is the
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object representing the
failed connection (the same as was returned by the L<connect method|"connect">,
$status is the failure reason (an errno dualvar just like L<$!|perlvar/"$!">)
and $destination is a string representing the socket address that was being
connected to.

This callback is always called as if the connect was slow, so from the main
eventloop after the call to the L<connect method|"connect"> returned.

If this option is not given and a connection fails, the default action is
to L<die|perlfunc/"die">.

=item X<Connect>Connect => $function_reference

If the connection attempt triggered by the L<connect method|"connect">
succeeds, this will be called like:

    $function_reference->($connection, $destination)

where $connection is the L<WEC::FastCGI::Connection|WEC::FastCGI::Connection>
object representing the connection (the same as was returned by the
L<connect method|"connect"> and $destination is a string representing the socket
address that was being connected to (this is what was given as argument for
connect. Use the L<WEC::FastCGI::Connection peer_address method|WEC::FastCGI::Connection/"peer_address"> to get the actual peer).

This callback is always called as if the connect was slow, so from the main
eventloop after the call to the L<connect method|"connect"> returned.

This callback is paired with L<Close|"Close"> callback, meaning that if the
point is reached that this callback would be called (if it exists), at some
later time the point will be reached where the L<Close|"Close"> callback will
be called (if it exists) and the other way round. So you can use these two to
track successful connections.

=item X<OneShot>OneShot => $boolean

If this option is set to true, connections will default to being one_shot,
which means that normally there will be only one request on a connection
and the server gets the responsibility to close the connection.

This can be changed per connection using the
L<one_shot method|WEC::FastCGI::Connection/"one_shot">.

Defaults to false.

=item X<Stdout>Stdout => $function_reference

If you give this callback, it will be called whenever a chunk arrives on the
"stdout" stream. The call will be:

    $function_reference->($request, $stdout_chunk);

where $request is the L<request object|WEC::FastCGI::Request> for the request
on which this stdout chunk was sent and the string $stdout_chunk is the new
piece of stdout that just arrived. B<After> this callback the $stdout_chunk
will be appended to the internal stdout buffer, so if you really want to,
you can modify it inside the callback to control what will get added. If the
stdout stream is potentially infinite and you process data already during this
callback, it's a good idea to set $stdout_chunk to the empty string, otherwise
the stdout buffer can grow without bounds.

=item X<StdoutEof>StdoutEof => $function_reference

Called when the server closes the request stdout stream like:

    $function_reference->($request, $stdout_buffer);

where $request is the L<request object|WEC::FastCGI::Request> for the
request on which stdout got closed. $stdout_buffer is an alias to the buffer
containing the collected stdout.

=item X<Stderr>Stderr => $function_reference

If you give this callback, it will be called whenever a chunk arrives on the
"stderr" stream. The call will be:

    $function_reference->($request, $stderr_chunk);

where $request is the L<request object|WEC::FastCGI::Request> for the request
on which this stderr chunk was sent and the string $stderr_chunk is the new
piece of stderr that just arrived. B<After> this callback the $stderr_chunk
will be appended to the internal stderr buffer, so if you really want to,
you can modify it inside the callback to control what will get added. If the
stderr stream is potentially infinite and you process data already during this
callback, it's a good idea to set $stdout_chunk to the empty string, otherwise
the stdout buffer can grow without bounds.

=item X<StderrEof>StderrEof => $function_reference

Called when the server closes the request stderr stream like:

    $function_reference->($request, $stderr_buffer);

where $request is the L<request object|WEC::FastCGI::Request> for the
request on which stderr got closed. $stderr_buffer is an alias to the buffer
containing the collected stderr.

=item X<GetValuesResult>GetValuesResult => $function_reference

When the client receives the result of a
L<get_values|WEC::FastCGI::Connection/"get_values"> request, this gets called
like:

    $function_reference->($connection, $hash_ref)

where $connection is the L<WEC::FastCGI::Connection|WEC::FastCGI::Connection>
object on which the L<get_values|WEC::FastCGI::Connection/"get_values">
was done and $hash_ref is a reference to a hash representing the
answer (mapping requested parameters to their values).

If this callback is not given, the client will print a warning that an
unhandled GetValuesResult was received.

=item X<EofIsEnd>EofIsEnd => $boolean

Normally when the server is done processing a request, it's supposed to
execute an L<END_REQUEST|WEC::FastCGI::Request/"END_REQUEST"> record
(in WEC::FastCGI::Request this corresponds to the server using
L<the end method|END_REQUEST|WEC::FastCGI::Request/"end">).

However, some existing FastCGI libraries will actually just close the
connection and exit, which would normally be interpreted as an aborted
request by WEC::FastCGI::Client.

By giving a true value to this tag, such a sudden end of file on the connection
will be interpreted as a clean exit with application returncode 0 for the
currently pending requests.

The default is false.

=item X<UnknownType>UnknownType => $function_reference

When the client send a management request unknown to the server, the server
will respond with a record stating it doesn't know how to handle that.
If you give this option, the client will then call $function_reference like:

    $function_reference->($connection, $type)

where $connection is the
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object on which the
server complaint was received and $type is a number saying what type of
request the server was unable to handle.

If no callback is registered, the client will simply output a warning and
continue.

Normally you won't need this since the WEC::FastCGI modules by default
only sends standard management records for which handlers are supposed to
exist.

=item X<RequestEnd>RequestEnd => $function_reference

If given, this callback will be called whenever a request is formally closed
with a L<END_REQUEST|WEC::FastCGI::Request/"END_REQUEST"> (in
WEC::FastCGI::Request this corresponds to the server using
L<the end method|END_REQUEST|WEC::FastCGI::Request/"end">) or for
all requests when a connection gets closed. In other words, every client
side request will reach the point that this callback can get called, so
you can use it to track when requests finish.

The call will be done like:

    $function_reference->($request, $application_status, $protocol_status)

where $request is the L<WEC::FastCGI::Request|WEC::FastCGI::Request> object
that is being ended and $application_status is the returncode from the
application and $protocol_status is a protocol-level status code. Possible
values are:

=over

=item REQUEST_COMPLETE

normal end of request.

=item CANT_MPX_CONN

The request was rejected. This happens when a client sends concurrent requests
over one connection to a server that is designed to process one request at a
time per connection.

=item OVERLOADED

The request was rejected. This happens when the server runs out of some
resource, e.g. database connections.

=item UNKNOWN_ROLE

The request was rejected. This happens when the client has specified a role
that is unknown to the server.

=item LOST_CONNECTION

This is a non-standard response that's generated locally when the connection
on which a as of yet unfinished request is running gets closed (due to
a local L<close|WEC::FastCGI::Connection/"close">, a remote EOF or an error
on the connection. In this case the application returncode obviously can't
be coming from the application and will be undef.

=back

=item X<Close>Close => $function_reference

When a successful connection closes, this is called like:

    $function_reference->($client, $connection, @args)

where $client is the L<WEC::FastCGI::Client|WEC::FastCGI::Client> object for
which the connection was created, $connection is the
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object that just got
closed and @args give extra context about the reason.

If the user explicitely closed the connection (without stating a reason in the
L<close|"close"> arguments), @args will be empty. On a clean end of file from
the peer, it will be the single string "eof". In all other cases it represents
an error. The operation that failed will be in $args[0], the failure reason
(an errno dualvar just like L<$!|perlvar/"$!">) will be in $args[1].

This callback forms a pair with the L<Connect callback|"Connect"> meaning
it can only get called if and only if the L<Connect method|"connect"> was
succesful. It is B<not> called if a L<connection attempt|"connect"> fails. See
the L<ConnectFail callback|"ConnectFail"> for that case.

=back

=item X<connect>my $connection = $client->connect(?$destination?)

Attempts to connect to $destination, a string whose format is
C<unix://filename> for unix domain sockets and C<tcp://host:port> for internet
sockets. If no destination is given, it uses the default from the
L<Destination|"Destination"> option and if even that isn't given, it will
L<croak|Carp/"croak">. It returns a
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object representing the
(pending) connection.

The method does not wait for the connection attempt to finish, but returns
immediately. When the connection attempt finishes, it will call the
L<Connect|"Connect"> or L<ConnectFail|"ConnectFail"> or callback (if they
exist) depending on success or failure of the attempt.

You don't have to wait for the connect to finish to submit requests to the
connection though. They will be buffered and sent when the connection comes
through.

=item X<request>my $request = $client->request(@args)

This is a convenience function for servers that handle one request per
connection. It's basically equivalent to:

    my $connection = $client->connect;
    $connection->one_shot(1);
    $request = $connection->request(@args);

To make sure that the request isn't lost in the case of a failed connection,
you'd might want to set up a L<ConnectFail handler|"ConnectFail"> (though
the default handler that L<dies|perlfunc/"die"> might be good enough). See the
L<WEC::FastCGI::Connection request method|WEC::FastCGI::Connection/"request">
for the meaning of @args.

=item X<destination>my $destination = $client->destination

Returns the name of the socket the client connects to by default
(or undef if not set)

=item X<connections> my $nr_connections = $client->connections()

In scalar context, returns the number of connections to servers.
This includes connections in progress (that may still fail).

=item my @connections = $client->connections()

In list context, returns a list of connections to servers.
This includes connections in progress (that may still fail).
Each connection will be represented by a
L<WEC::FastCGI::Connection|WEC::FastCGI::Connection> object.

=item X<object_count>my $nr_clients = WEC::FastCGI::Client::object_count()

This function is mainly meant for debugging leaks. Whenever a client object
is created, an internal count is increased, and whenever one gets destroyed an
internal count is decreased. This class method allows you to retrieve the
current count, after which you can compare that with what you expected.

=back

=head1 EXPORT

None by default, but the following ones may be requested:

=over

=item object_count

=back

=head1 SEE ALSO

L<WEC>,
L<WEC::Socket>,
L<WEC::FastCGI::Server>,
L<WEC::FastCGI::Connection>,
L<WEC::FastCGI::Request>,
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

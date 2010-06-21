package WEC::FastCGI;
use 5.006;
use strict;
use warnings;

use vars qw(@ISA $VERSION);

$VERSION = "1.000";
require CGI;
@ISA = qw(CGI);

# override the initialization behavior so that
# state is NOT maintained between invocations
sub save_request {
    # no-op
}

1;

__END__

=head1 NAME

WEC::FastCGI - CGI Interface for a WEC::FastCGI based server

=head1 SYNOPSIS

    use WEC qw(api=1 loop);
    use WEC::FastCGI::Server;
    use WEC::FastCGI qw/:standard/;

    WEC->init;
    WEC::FastCGI::Server->new(AutoClean	=> 0,
                              StdinEof	=> \&process);
    loop;

    sub process {
        print header;
        print start_html("Fast CGI Rocks");
        print
            h1("Fast CGI Rocks"),
            "Invocation number ",b($COUNTER++),
            " PID ",b($$),".",
            hr;
        print end_html;
    }

=head1 DESCRIPTION

WEC::FastCGI is a subclass of the CGI object created by CGI.pm.  It is
specialized to work well with the Open Market FastCGI standard
(L<http://www.fastcgi.com/>), which greatly speeds up CGI scripts by turning
them into persistently running server processes. Scripts that perform
time-consuming initialization processes, such as loading large modules or
opening persistent database connections, will see large performance
improvements.

=head1 OTHER PIECES OF THE PUZZLE

However, unlike with normal CGI classes (even L<CGI::Fast|CGI::Fast>), you
should not run in a while loop creating new objects and handling one connection
at a time. Instead let L<WEC::FastCGI::Server|WEC::FastCGI::Server> create the
CGI object and use it from callbacks (see the example in the
L<SYNOPSIS|"Synopsis">).

In order to use WEC::FastCGI you'll need a standalone Fast CGI client
(L<WEC::FastCGI::Client|WEC::FastCGI::Client> lets you easily write such
clients) or (much more common) use a FastCGI-enabled Web server. Almost all
modern webservers are able to handle FastCGI (after maybe activating some
addon module). For example, apache (L<http://httpd.apache.org/>) has the
mod_fastcgi module
(L<http://www.fastcgi.com/mod_fastcgi/docs/mod_fastcgi.html>). Look at
the fastci website (L<http://www.fastcgi.com/>) for information about other
servers.

=head1 WRITING FASTCGI PERL SCRIPTS

FastCGI scripts are persistent: one or more copies of the script are started up
when the web server initializes, and stay around until the web server exits or
they die a natural death.  After performing whatever one-time initialization
it needs, the script enters the eventkernel main loop waiting for incoming
connections and requests sent over such a connection. Your code will then be
told about the request, you can send out an answer and then return (which
will return to the main loop).

A typical WEC::FastCGI script will look like this:

    #!/usr/bin/perl -wT
    use WEC qw(api=1 loop);
    use WEC::FastCGI::Server;
    use WEC::FastCGI qw/:standard/;

    do_some_initialization();
    WEC->init;
    WEC::FastCGI::Server->new(AutoClean	=> 0,
                              StdinEof	=> \&process_request);
    loop;

Each time a new request is completely received, process_request will be called
with a WEC::FastCGI object as its second parameter, The rest of the time your
script sits waiting in the call to loop(). The server can use signals to
request your script to terminate or you can deecide to exit earlier. A new
version of the script will be respawned by the web server to take its place
(this may be necessary in order to avoid Perl memory leaks in long-running
scripts).

CGI.pm's default CGI object mode also works. Calls to header(), start_form(),
etc. will all operate on the current request.

=head1 INSTALLING FASTCGI SCRIPTS

See the FastCGI developer's kit documentation for full details. On the
Apache server, the following line can be added to configuration:

    AddHandler fastcgi-script fcgi

This will make scripts that end with the extension F<.fcgi> be FastCGI
scripts.

Or you can in a C<directory> context add:

    SetHandler fastcgi-script

which will make all scripts below the directory be FastCGI scripts.

You also need to add a directive to start scripts. For static scripts
(servers are always started), you'd use something like:

    FastCgiServer /usr/etc/httpd/fcgi-bin/file_upload.fcgi -processes 2

While for dynamic scripts (servers are started on demand), you'd use something
like:

    FastCgiConfig

(you can also tell the server about externally managed scripts using
FastCgiExternalServer).

This only very lightly touches on what can be configered. Please see the
mod_fastcgi documentation
(L<http://www.fastcgi.com/mod_fastcgi/docs/mod_fastcgi.html>) for more details.
One interesting option is C<-flush> which tells the web server not to buffer
your output until it sees the request end.

=head1 USING FASTCGI SCRIPTS AS CGI SCRIPTS

Won't work out of the box, though you could make it work if you wanted to.
There's little point to it though.

=head1 EXTERNAL FASTCGI SERVER INVOCATION

FastCGI supports a TCP/IP transport mechanism which allows FastCGI scripts to
run external to the webserver, perhaps even on a remote machine. To configure
apache to connect to an external FastCGI server, you would add something like
the following to your configuration:

    FastCgiExternalServer /usr/etc/httpd/fcgi-bin/file_upload.fcgi -host localhost:1234

And you can then change your application code to set up its own socket by
doing something like:

    #!/usr/bin/perl -wT
    use WEC qw(api=1 loop);
    use WEC::FastCGI::Server;
    use WEC::FastCGI qw/:standard/;

    my $socket = inet(LocalAddr => "localhost", LocalPort => 1234);
    WEC->init;
    WEC::FastCGI::Server->new(Handle	=> $socket,
    			      AutoClean	=> 0,
                              StdinEof	=> \&process_request);
    loop;

=head1 SEE ALSO

L<WEC>,
L<WEC::Socket>,
L<WEC::FastCGI::Server>,
L<WEC::FastCGI::Client>,
L<WEC::FastCGI::Connection>,
L<WEC::FastCGI::Request>,
L<CGI>,
L<CGI::Fast>,
L<FCGI>,
L<http://www.fastcgi.com>
L<http://www.fastcgi.com/devkit/doc/fcgi-spec.html>
L<http://www.fastcgi.com/mod_fastcgi/docs/mod_fastcgi.html>
L<http://httpd.apache.org/>

=head1 AUTHOR

Ton Hospel, E<lt>WEC-FastCGI@ton.iguana.beE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2004 by Ton Hospel

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

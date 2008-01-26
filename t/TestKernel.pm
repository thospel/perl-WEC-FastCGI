use 5.008_001;
use warnings;
use strict;
use POSIX qw(ECONNRESET);
use Time::HiRes qw(time);

use WEC::Test(TraceLine => 0,
              Class => "WEC::FastCGI",
              Parts =>[qw(Request Connection Client Server)]);
my $c;
{
    no warnings 'redefine';
    sub check_fd {
        $client = $server = $c = undef;
        return WEC::Test::check_fd(@_);
    }
}
use WEC::Socket qw(unix inet);

WEC->init;
check_fd();

# Load constants module
use_ok("WEC::FastCGI::Constants");
can_ok("WEC::FastCGI::Constants",
       qw(RESPONDER AUTHORIZER FILTER LOST_CONNECTION));
WEC::FastCGI::Constants->import(qw(RESPONDER AUTHORIZER FILTER
                                   LOST_CONNECTION));
# Load client module
use_ok("WEC::FastCGI::Client");
can_ok("WEC::FastCGI::Client", qw(new connect destination object_count));
check_fd();

# Load server module
use_ok("WEC::FastCGI::Server");
can_ok("WEC::FastCGI::Server",
       qw(new check_fcgi abort object_count));
WEC::FastCGI::Server->import(qw(check_fcgi));
can_ok(__PACKAGE__, qw(check_fcgi));
can_ok("WEC::FastCGI::Connection",
       qw(handle type close close_on_empty send object_count));
can_ok("WEC::FastCGI::Request",qw(id role role_name parse_params params
                                    stdin stdin_flush stdin_close
                                    stdout stdout_flush stdout_close
                                    stderr stderr_flush stderr_close
                                    data data_flush data_close object_count));
WEC::FastCGI::Request->import(qw(parse_params));
check_fd();

{
    package Faker;
    sub new {
        return bless [rand], shift;
    }
}

# Naive query string constructor (no escaping)
sub query_string {
    my $hash = shift;
    return join(";", map "$_=$hash->{$_}", keys %$hash);
}

for my $type (qw(Unix Tcp)) {
    check_fd();
    check_objects();
    $socket_type = $type;
    # Basic client test
    $hit = 0;
    my ($c1, $c2, $fd, $l, $p);
    WEC->init;
    eval {WEC::FastCGI::Client->new("RequestEnd") };
    like($@, qr/requires an even number of parameters/i,
         "Odd number of args fails");
    # eval {WEC::FastCGI::Client->new() };
    # like($@, qr/No mandatory/i, "Missing mandatory parameter");
    eval {WEC::FastCGI::Client->new(zzzz => 12) };
    like($@, qr/Unknown option/i, "Unknown parameter");
    # Check fd leaks. First do a sanity check of the current status
    check_fd();
    check_objects();
    $client = WEC::FastCGI::Client->new();
    ok($client, "Can create a client");
    check_fd;

    # Now dual create without the fd check destroying $client
    $client = WEC::FastCGI::Client->new();
    ok($client, "Can create a client twice");
    ($l, $p) = $type eq "Unix" ? unix() : inet();
    $fd = fileno($l);
    ok(defined($fd), "Has an open destination socket");
    $client = WEC::FastCGI::Client->new
        (Destination => $p, Paths => $p, Connect => sub {
            $hit++;
            is(shift, $c1, "callback arg is connection");
            eval { $c1->close };
            is($@, "", "Can close connection");
            unloop("foo");
        });
    ok($client, "Can create a client thrice");
    $destination = $client->destination;
    is($destination, $p, "Destination is path");

    $c1 = $client->connect;
    ok($c1, "Connect with callback");
    is($hit, 0, "Callback not hit early");
    is (loop(), "foo", "Right returncode from loop");
    is($hit, 1, "One callback");
    $fd = fileno($l);
    ok(defined($fd), "Destination is still open");

    WEC->init;
    $fd = fileno($l);
    ok(defined($fd), "Destination is still open");
    eval { check_fcgi($l) };
    is($@, "", "destination is a valid FCGI socket");
    eval { WEC::FastCGI::Server->new("Handle") };
    like($@, qr/requires an even number of parameters/i,
         "Odd number of args fails");
    # eval {WEC::FastCGI::Server->new(Handle	=> $l) };
    # like($@, qr/No mandatory/i, "Missing mandatory parameter");
    eval {WEC::FastCGI::Server->new(Handle	=> $l,
                                    zzzz		=> 12) };
    like($@, qr/Unknown option/i, "Unknown parameter");
    eval {WEC::FastCGI::Server->new(Handle	=> $l,
                                    IpAccept	=> 12) };
    like($@, qr/not an array reference/i, "Not an array reference");
    eval {WEC::FastCGI::Server->new(Handle	=> $l,
                                    IpAccept	=> {foo => 12}) };
    like($@, qr/not an array reference/i, "Not an array reference");
    $server = eval {
        WEC::FastCGI::Server->new(Handle	=> $l,
                                  IpAccept	=> ["localhost"]) };
    if ($type eq "Unix") {
        like($@, qr/non-tcp socket/i, "Not a tcp socket");
    } elsif ($type eq "Tcp") {
        ok($server, "Perfectly good tcp check");
        is_deeply($server->ip_accept, ["127.0.0.1"],
                  "IpAccept is properly translated and listed");
        $server = eval {
            WEC::FastCGI::Server->new(Handle	=> $l,
                                      IpAccept	=> ["1.2.3.4.5.6"]) };
        like($@, qr/Bad hostname/i, "Cannot parse bad hostname");
        eval { WEC::FastCGI::Server->new(Handle	=> $l,
                                         IpAccept	=> []) };
        like($@, qr/list is empty/i, "IpAccept list is empty");
        delete $ENV{FCGI_WEB_SERVER_ADDRS};
        $server = WEC::FastCGI::Server->new(Handle	=> $l,
                                            IpAccept=> "ENV");
        ok($server, "Perfectly good tcp check");
        is($server->ip_accept, undef, "No ENV means no whitelist");
        local $ENV{FCGI_WEB_SERVER_ADDRS}="199.170.183.28,199.170.183.71";
        $server = "";
        $server = WEC::FastCGI::Server->new(Handle	=> $l,
                                            IpAccept=> "ENV");
        ok($server, "Perfectly good tcp check");
        is_deeply([sort @{$server->ip_accept}], [qw(199.170.183.28
                                                    199.170.183.71)],
                  "IpAccept is properly translated and listed");
        $ENV{FCGI_WEB_SERVER_ADDRS}="";
        $server = eval {
            WEC::FastCGI::Server->new(Handle	=> $l,
                                      IpAccept	=> "ENV") };
        like($@, qr/list is empty/i, "IpAccept list is empty");
    } else {
        die "Unknown type $type";
    }

    $server = WEC::FastCGI::Server->new(Handle	=> $l);
    ok($server, "Can create a server");
    my $nr = $server->connections;
    is($nr, 0, "New server has no connections");
    my @c = $server->connections;
    is(@c, 0, "New server has no connections");
    is_deeply($server->handles, [$l], "Returned handle is what we gave");
    ok($server->accepting, "By default the server accepts connections");
    ok($server->accepting(0), "Setting a new value returns the old one");
    is($server->accepting, 0, "Setting a new value works");
    is($server->accepting(undef), 0,
       "Setting a new value returns the old one");
    is($server->accepting, undef, "Setting a new value works");
    is($server->accepting(1), undef, "Setting a new value works");
    is($server->accepting, 1, "Setting a new value works");
    $server = "";               # Force destructor to run before new one

    $server = WEC::FastCGI::Server->new(Handle	=> $l);
    ok($server, "Can create a server twice");
    eval { $server->abort };
    is($@, "", "Can abort server");
    eval { $server->abort };
    ok($@, "Can't abort twice");
    $server = "";
    is(fileno($l), $fd, "Destination is still open");
    $l = undef;
    check_fd();
    # c1 should be all that's keeping 1 Connection object alive
    check_objects(Connection => 1);
    $c1 = undef;
    check_objects();

    # Querying props
    $WEC::FastCGI::Connection::value_replies{"a" x 256} = "fooz";
    $WEC::FastCGI::Connection::value_replies{"b" x 256} = "e" x 256;
    my $default_reply = {
        %WEC::FastCGI::Connection::value_replies,
        FCGI_MPXS_CONNS => 1};

    ok(scalar (%WEC::FastCGI::Connection::value_replies), "There are default values");
    WEC->init;
    $hit = 0;
    ($l, $p) = $type eq "Unix" ?
        unix(# Check acceptance of these options too
             BindAttempts	=> 10,
             Listen		=> 5) : inet();
    $client = WEC::FastCGI::Client->new
        (Destination => $p, Paths => $p,
         GetValuesResult => sub {
             is(shift, $c, "First arg is connection");
             isa_ok($c, "WEC::FastCGI::Connection");
             is_deeply(shift, $default_reply, "Got right result");
             $hit++;
             $server->abort;
             is($server->connections, 0,
                "No more connections registered after abort");
             $c->close;
             unloop("goo");
         },
         Connect => sub {
             ++$hit;
             ok($hit==1 || $hit==3, "Connected at the right moment");
             $c->get_values("a" x 256, "b" x 256, "c" x 256,
                            qw(FCGI_MAX_CONNS FCGI_MAX_REQS FCGI_MPXS_CONNS));
         });
    ok($client, "Can create a client");
    $fd = fileno($l);
    ok(defined($fd), "Destination is open");
    $server = WEC::FastCGI::Server->new
        (Handle	=> $l,
         # Should be acceptable
         $type eq "Tcp" ? (IpAccept => ["127.0.0.1"]) : (),
         PreAccept	=> sub {
             isa_ok(shift, "WEC::FastCGI::Server");
             ++$hit;
             ok($hit ==1 || $hit == 2,
                "PreAccept entered at the right moment");
         },
         Accept	=> sub {
             my $serv = shift;
             isa_ok($server, "WEC::FastCGI::Server");
             is($serv, $server, "Accept is on the server itself");

             my $conn = shift;
             isa_ok($conn, "WEC::FastCGI::Connection");
             ++$hit;
             ok($hit == 2 || $hit ==3,
                "Accept entered at the right moment");
             my $nr = $server->connections;
             is($nr, 1, "One connection");
             my @conn = $server->connections;
             is(@conn, 1, "One connection");
             is($conn[0], $conn, "Connections is the second argument");
         });
    ok($server, "Can create a server");
    $l = undef;
    $c = $client->connect;
    is(loop(), "goo", "Right returncode from loop");
    is($hit, 4, "Executed all parts");
    check_fd();
    $c = "";
    check_objects();

    # Early data push
    $c = make_pair([GetValuesResult => sub {
        is(shift, $c, "First arg is connection");
        is_deeply(shift, $default_reply, "Got right result");
        $hit++;
        $server->abort;
        $c->close;
        unloop("goo");
    },
                    Connect => , sub {
                        $hit++;
                    }], []);
    $c->get_values("a" x 256, "b" x 256, "c" x 256,
                   qw(FCGI_MAX_CONNS FCGI_MAX_REQS FCGI_MPXS_CONNS));
    is(loop(), "goo", "Right returncode from loop");
    is($hit, 2, "Executed GetValuesResult");
    check_fd();
    check_objects();

    # Query props without handler
    {
        local $SIG{__WARN__} = sub {
            like("@_", qr/GetValuesResult without a handler/i,
                 "Error if no handler");
            $server->abort;
            $c->close;
            unloop();
        };
        $c = make_pair([Connect => sub {
            $hit++;
            $c->get_values(qw(FCGI_MAX_CONNS FCGI_MAX_REQS FCGI_MPXS_CONNS));
        }], undef);
        plain_loop();
    }
    is($hit, 1, "Message sent");
    check_fd();

    # Raw send and client unknown type handling
    $c = make_pair([UnknownType => sub {
        is(shift, $c, "First arg is connection");
        is(shift, 0, "Got right type");
        $hit++;
        $server->abort;
        $c->close;
        eval { $c->send(0, 0, "") };
        ok($@, "Can't send on closed connection");
        unloop("goo");
    },
               Connect => sub {
                   $hit++;
                   $c->send(0, 0, "");
                   eval { $c->send(0, 0, "a" x 65536) };
                   ok($@, "Can't send too long messages");
               }], []);
    my $server_unknown_warning = "";
    {
        local $SIG{__WARN__} = sub {
            $hit++;
            $server_unknown_warning .= shift;
        };
        is(plain_loop(), "goo", "Right returncode from loop");
        like($server_unknown_warning, qr/Unknown management record.*hex data/i,
             "Server side warning");
    }
    is($hit, 3, "Executed UnknownType");
    check_fd();
    check_objects();

    # Unknown message without handler
    $c = make_pair([GetValuesResult => sub {
        is(shift, $c, "First arg is connection");
        is_deeply(shift, $default_reply, "Got right result");
        $hit++;
        $server->abort;
        $c->close;
        unloop("goo");
    },
               Connect => sub {
                   $hit++;
                   $c->send(0, 0, "");
                   $c->get_values("a" x 256, "b" x 256, "c" x 256,
                                  qw(FCGI_MAX_CONNS FCGI_MAX_REQS FCGI_MPXS_CONNS));
               }], []);

    {
        my $warn = "";
        local $SIG{__WARN__} = sub {
            $hit++;
            $warn .= shift;
        };
        is(plain_loop(), "goo", "Right returncode from loop");
        ok($warn =~ s/^\Q$server_unknown_warning//,
           "Still have server warning");
        like($warn, qr/Server claims not to know about type 0/i,
             "Client warning");
    }
    is($hit, 4, "Executed GetValuesResult");
    check_fd();
    check_objects();

    # Server side UnknownType
    $c = make_pair([GetValuesResult => sub {
        is(shift, $c, "First arg is connection");
        is_deeply(shift, $default_reply, "Got right result");
        $hit++;
        $server->abort;
        $c->close;
        unloop("baz");
    },
               Connect => sub {
                   $hit++;
                   $c->send(0, 0, "Boo !");
                   $c->get_values("a" x 256, "b" x 256, "c" x 256,
                                  qw(FCGI_MAX_CONNS FCGI_MAX_REQS FCGI_MPXS_CONNS));
               }], [UnknownManagementRecord => sub {
                   $hit++;
                   isa_ok(shift, "WEC::FastCGI::Connection",
                          "Called on a connection");
                   is(shift, 0, "Right type");
                   is(shift, "Boo !", "Body arrived");
               }]);

    {
        my $warn = "";
        local $SIG{__WARN__} = sub {
            # Will never be hit
            $hit++;
            $warn .= shift;
        };
        is(plain_loop(), "baz", "Right returncode from loop");
        is($warn, "", "No unknown warnings");
    }
    is($hit, 3, "Executed UnknownType and GetValuesResult");
    check_fd();
    check_objects();

    # My first real request !
    my $r;
    $c = make_pair([
               RequestEnd => sub {
                   is(shift, $r, "First arg is request");
                   is(shift, 5, "Second arg is application status");
                   is(shift, 7, "Third arg is protocol status");
                   $hit++;
                   $server->abort;
                   $c->close;
                   eval { $c->request };
                   ok($@, "Bad role");
                   unloop("baz");
               },
               Connect => sub {
                   my $conn = shift;
                   $hit++;
                   eval { $r = $conn->request };
                   is($@, "", "Plain request works");
                   isa_ok($r, "WEC::FastCGI::Request");
                   is($r->id, 1, "First request gets id 1");
                   is($r->role, RESPONDER(), "Default to responder");
                   is($r->role_name, "Responder", "Default to responder");
                   eval { $conn->request("Boo!") };
                   ok($@, "Bad role");
                   eval { $conn->request("00") };
                   ok($@, "Bad role");
                   eval { $conn->request(1.2) };
                   ok($@, "Bad role");
                   eval { $conn->request(65536) };
                   ok($@, "Bad role");
               }], [RequestBegin => sub {
                   $hit++;
                   my ($c, $req) = @_;
                   isa_ok($c,   "WEC::FastCGI::Connection");
                   isa_ok($req, "WEC::FastCGI::Request");
                   is($req->id, 1, "First request gets id 1");
                   is($req->role, RESPONDER(), "Default to responder");
                   is($req->role_name, "Responder", "Default to responder");
                   $req->end(5, 7);
               }]);
    is(loop(), "baz", "Right returncode from loop");
    is($hit, 3, "Executed all parts");
    check_fd();
    check_objects(Request => 1);
    # Request $r should be keeping one request and one object alive
    $r = undef;
    check_objects();

    # Test some other defaults
    # This time we use a local autocleaning $r
    $c = make_pair([
               RequestEnd => sub {
                   is(shift, $r, "First arg is request");
                   is(shift, 0, "Second arg is application status");
                   is(shift, 0, "Third arg is protocol status");
                   $hit++;
                   $server->abort;
                   $c->close;
                   unloop("baz");
               },
               Connect => sub {
                   my $conn = shift;
                   $hit++;
                   eval { $r = $conn->request(AUTHORIZER()) };
                   is($@, "", "Plain request works");
                   is($r->id, 1, "First request gets id 1");
                   is($r->role, AUTHORIZER(), "Role set is role used");
                   is($r->role_name, "Authorizer", "Role 2 is known");
               }], [RequestBegin => sub {
                   $hit++;
                   my ($c, $req) = @_;
                   is($req->id, 1, "First request gets id 1");
                   is($req->role, AUTHORIZER(), "Role set is role used");
                   is($req->role_name, "Authorizer", "Role 2 is known");
                   $req->end;
               }]);
    is(loop(), "baz", "Right returncode from loop");
    is($hit, 3, "Executed all parts");
    check_fd();
    $r = undef;
    check_objects();

    # Two requests on the same connection
    $c = make_pair([
               RequestEnd => sub {
                   $hit++;
                   if (shift->id == 2) {
                       $server->abort;
                       $c->close;
                       unloop("bat");
                   }
               },
               Connect => sub {
                   my $conn = shift;
                   $hit++;
                   my $r = $conn->request(FILTER());
                   is($r->id, 1, "First request gets id 1");
                   is($r->role, FILTER(), "Role set is role used");
                   is($r->role_name, "Filter", "Role 3 is known");
                   $r = $conn->request(0);
                   is($r->id, 2, "Second request gets id 2");
               }], [RequestBegin => sub {
                   $hit++;
                   my ($c, $req) = @_;
                   if ($req->id == 1) {
                       is($req->role, FILTER(), "Role set is role used");
                       is($req->role_name, "Filter", "Role 3 is known");
                       $req->end;
                   } elsif ($req->id == 2) {
                       is($req->role, RESPONDER(),
                          "Role set is role used");
                       $req->end(6);
                   } else {
                       fail("Bad Id");
                   }
               }]);
    is(loop(), "bat", "Right returncode from loop");
    is($hit, 5, "Executed all parts");
    check_fd();
    check_objects();

    # Two connections with each one request
    my $server_count = 0;
    WEC->init;
    $hit = 0;
    my $client_count = 0;
    ($l, $p) = $socket_type eq "Unix" ?
        unix() : inet(LocalAddr => "localhost");
    $client = WEC::FastCGI::Client->new
        (Connect => sub {
            my $conn = shift;
            $hit++;
            if ($conn == $c1) {
                my $r = $conn->request(FILTER());
                is($r->id, 1, "First request gets id 1");
            } elsif ($conn == $c2) {
                my $r = $conn->request;
                is($r->id, 1, "Second request gets id 1 too");
            } else {
                die "Bad connection";
            }
        },
         Destination => $p, Paths => $p,
         RequestEnd => sub {
             $hit++;
             if (++$client_count == 2) {
                 $server->abort;
                 $c1->close;
                 $c2->close;
                 unloop("bat");
             }
         });
    $server = WEC::FastCGI::Server->new
        (Handle	=> $l,
         Accept	=> sub {
             $hit++;
             my $serv = shift;
             is($serv, $server, "Right first argument");
             my $c = shift;
             is($server->connections, ++$server_count,
                "All connections registered");
             my @c = $server->connections;
             ok($c[0] == $c || $c[1] == $c, "Right connection");
         },
         RequestBegin => sub {
             $hit++;
             my ($c, $req) = @_;
             is($req->id, 1);
             is($server->connections, $server_count, "All connections registered");
             my @c = $server->connections;
             ok($c[0] == $c || $c[1] == $c, "Right connection");
             $req->end;
             is($server->connections, $server_count,
                "Same amount of connections registered (close_on_empty)");
         },
         Close => sub {
             $hit++;
             is($server->connections, --$server_count,
                "Connection is dropped before callback");
         });
    $l = undef;
    $c1 = $client->connect();
    $c2 = $client->connect();
    is(loop(), "bat", "Right returncode from loop");
    is($hit, 10, "Executed all parts");
    is($server_count, 0, "All server connections got closed");
    check_fd();
    check_objects(Connection => 2);
    $c1 = $c2 = undef;
    check_objects();

    # Testing early abort
    $c = make_pair(
              [RequestEnd => sub {
                  if (++$hit == 2) {
                      is(shift->id, 1, "Got end for request 1");
                      is(shift, -1 & 0xffffffff, "app returncode -1");
                      is(shift, 0, "proto returncode 0");
                      $server->abort;
                      $c->close;
                  } else {
                      is($hit, 3, "Third event");
                      is(shift->id, 2, "Got end for request 2");
                      is(shift, undef, "app returncode undef");
                      is(shift, LOST_CONNECTION(), "proto returncode 254");
                      unloop("bat");
                  }
              }, Connect => sub {
                  ++$hit;
                  my $conn = shift;
                  my $r = $conn->request;
                  my $r1 = $conn->request;
                  $r->abort;
              }], undef);
    is(loop(), "bat", "Right returncode from loop");
    is($hit, 3, "Executed all parts");
    check_fd();
    check_objects();

    if ($type eq "Unix") {
        # Nothing
    } elsif ($type eq "Tcp") {
        my $accept_hit;
        my $r;
        $c = make_pair(
                  [EofIsEnd => 0,
                   Connect => sub {
                       $hit++;  # First or second
                       ok($hit == 1 || $hit == 2, "Got connected");
                       $c = shift;
                       $c->request;
                   }, RequestEnd => sub {
                       # is(++$hit, 3, "Request ends as third");
                       ++$hit;
                       ok($hit == 3 || $hit == 4, "Request aborted");
                       shift;
                       is(shift, undef, "app status");
                       is(shift, LOST_CONNECTION(), "proto status");
                   }, Close => sub {
                       my ($cl, $conn, $operation, $reason) = @_;
                       is($cl, $client, "First arg is client");
                       is($conn, $c, "Second arg is connection");
                       if ($operation eq "eof") {
                           pass("sysread warning replacement");
                           is(++$hit, 4, "warning didn't increase $hit");
                       } else {
                           ok($operation eq "sysread" &&
                              $reason == ECONNRESET, "Right reason");
                           is(++$hit, 4, "warning didn't increase $hit") if
                               $reason == ECONNRESET;
                       }
                       is (++$hit, 5, "Close as third");
                       $server->abort;
                       unloop("bat");
                   }],
                  [IpAccept => ["127.0.0.2"],
                   PreAccept	=> sub {
                       is(++$accept_hit, 1, "PreAccept done");
                   },
                   Accept	=> sub {
                       ++$accept_hit;
                       fail("Accept while we should reject");
                   }]);
        {
            local $SIG{__WARN__} = sub {
                $hit++;         # Second or first
                if ($hit == 1 || $hit == 2) {
                    like(shift, qr/dropped connection from tcp:\/\/127\.0\.0\.1/i,
                         "Refuse connection from localhost");
                    is(++$accept_hit, 2, "Rejection done");
                } elsif ($hit == 3) {
                    $! = ECONNRESET;
                    is(shift, "sysread: $!\n", "Connection reset warning");
                } else {
                    fail("Bad hit: $hit");
                }
            };
            is(plain_loop(), "bat", "Right returncode from loop");
        }
        is($hit, 5, "Executed all parts");
        is($accept_hit, 2, "PreAceept and Reject");
        check_fd();
        $c = "";
        check_objects();

        $c = make_pair([
                   Close => sub {
                       $hit++;
                       $server->abort;
                       unloop("bat");
                   }, RequestEnd => sub {
                       # Shouldn't be reached
                       fail("Should not reach RequestEnd");
                   }, Connect => sub {
                       $hit++;  # First or second
                   }],
                  [IpAccept => ["127.0.0.2"],
                   ]);
        {
            local $SIG{__WARN__} = sub {
                $hit++;         # Second or first
                like(shift, qr/dropped connection from tcp:\/\/127\.0\.0\.1/i,
                     "Refuse connection from localhost");
            };
            is(plain_loop(), "bat", "Right returncode from loop");
        }
        is($hit, 3, "Executed all parts");
        check_fd();
        check_objects();
    } else {
        die "Unhandled case $type";
    }
    # Sending stuff all over the place
    $ENV{foo} = "bar";
    my $fd0 = fileno(STDIN);
    ok(defined($fd0), "STDIN is open");
    eval { check_fcgi(\*STDIN) };
    ok($@, "STDIN isn't a unix socket");
    my $no_fcgi = $@;
    $no_fcgi =~ s/line\s+.*\n?//;
    eval { check_fcgi() };
    like($@, qr/^\Q$no_fcgi/, "STDIN isn't a unix socket");
    my $fd1 = fileno(STDOUT);
    ok(defined($fd1), "STDOUT is open");
    my $fd2 = fileno(STDERR);
    ok(defined($fd2), "STDERR is open");
    is($CGI::Q, undef, "No default CGI object");
    my %params = (foo => 5, bar => 6,
                  baz => "b" x 1e5, "c" x 1e5 => "ah", "d" x 256 => "e" x 256);
    $c = make_pair([
               Connect => sub {
                   $hit++;
                   my $conn = shift;
                   eval { check_fcgi($c->handle) };
                   like($@, qr/^\Q$no_fcgi/, "STDIN isn't a unix socket");
                   my $r = $conn->request;
                   # Send a bad application record
                   $conn->send(0, $r->id, "Farm");
                   $r->stdin("a" x 100001);
                   $r->stdin("std");
                   $r->stdin("in");
                   $r->data("a" x 100001);
                   $r->data("da");
                   $r->data("ta");
                   $r->params_close(\%params);
                   $r->data_close;
                   $r->stdin_close;
               },
               RequestEnd => sub {
                   is(++$hit, 24, "At last the end");
                   is(shift->id, 1, "Got end for request 1");
                   is(shift, 0, "Second arg is application status");
                   is(shift, 0, "Third arg is protocol status");
                   $server->abort;
                   $c->close;
                   unloop("bat");
               },
               Stdout	=> sub {
                   isa_ok(shift, "WEC::FastCGI::Request");
                   ++$hit;
                   if ($hit == 16) {
                       is(shift, "a" x (2**16-8), "First piece of stdout");
                   } elsif ($hit == 17) {
                       is(shift, "a" x 34473, "Second piece of stdout");
                   } else {
                       is($hit, 20, "STDOUT");
                       is(shift, "stdout");
                   }
               },
               StdoutEof => sub {
                   is(++$hit, 21, "close STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
               },
               Stderr => sub {
                   isa_ok(shift, "WEC::FastCGI::Request");
                   ++$hit;
                   if ($hit == 18) {
                       is(shift, "a" x (2**16-8), "stderr");
                   } elsif ($hit == 19) {
                       is(shift, "a" x 34473, "stderr");
                   } elsif ($hit == 22) {
                       is(shift, "stderr", "stderr");
                   } else {
                       is($hit, 22, "Unexpected stderr");
                       diag("Unexpected stderr: " . shift);
                   }
               },
               StderrEof => sub {
                   is(++$hit, 23, "close STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
               }
               ], [CgiClass => "Faker",
                   FakeCgi  => 0,
                   Stdin => sub {
                       isa_ok(shift, "WEC::FastCGI::Request");

                       isa_ok(shift, "Faker", "Class");
                       is($CGI::Q, undef, "No default CGI object");

                       ++$hit;
                       if ($hit == 3) {
                           is(<STDIN>, undef, "STDIN tie works");
                           is(shift, "a" x (2**16-8), "stdin chunk");
                           is_deeply(\%ENV, {}, "Env set");
                       } elsif ($hit == 4) {
                           is(shift, "a" x 34473, "stdin chunk");
                           is_deeply(\%ENV, {}, "Env set");
                       } else {
                           is($hit, 14, "data eof");
                           is(shift, "stdin", "stdin chunk");
                           is_deeply(\%ENV, \%params, "Env set");
                       }
                   },
                   StdinEof => sub {
                       is(++$hit, 15, "data eof");
                       my $r = shift;
                       isa_ok($r, "WEC::FastCGI::Request");
                       isa_ok(shift, "Faker", "Class");
                       is($CGI::Q, undef, "No default CGI object");
                       is_deeply(\%ENV, \%params, "Env set");
                       is(shift, "a" x 100001 . "stdin", "stdin chunk");
                       is(<STDIN>, "a" x 100001 . "stdin", "STDIN tie works");
                       is(<STDIN>, undef, "STDIN tie closes");
                       close(STDIN);
                       is(<STDIN>, undef, "STDIN tie is closed");
                       $r->stdout("a" x 100001);
                       print STDOUT "std";
                       $r->stdout("out");
                       $r->stderr("a" x 100001);
                       close(STDOUT);
                       print STDERR "stderr";
                       close(STDERR);
                   },
                   Data => sub {
                       ++$hit;

                       isa_ok(shift, "WEC::FastCGI::Request");

                       isa_ok(shift, "Faker", "Class");
                       is($CGI::Q, undef, "No default CGI object");

                       if ($hit == 5) {
                           is(shift, "a" x (2**16-8), "data chunk");
                           is_deeply(\%ENV, {}, "Env set");
                       } elsif ($hit == 6) {
                           is(shift, "a" x 34473, "data chunk");
                           is_deeply(\%ENV, {}, "Env set");
                       } else {
                           is($hit, 12, "data");
                           is(shift, "data", "data chunk");
                           is_deeply(\%ENV, \%params, "Env set");
                       }
                   },
                   DataEof => sub {
                       is(++$hit, 13, "data eof");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       isa_ok(shift, "Faker", "Class");
                       is($CGI::Q, undef, "No default CGI object");

                       is(shift, "a" x 100001 . "data", "data chunk");
                       is_deeply(\%ENV, \%params, "Env set");
                   },
                   Params => sub {
                       ++$hit;
                       if (7 <= $hit && $hit <= 10) {
                           pass("Good params");
                       } else {
                           is($hit, 10, "Param");
                       }
                       isa_ok(shift, "WEC::FastCGI::Request");

                       isa_ok(shift, "Faker", "Class");
                       is($CGI::Q, undef, "No default CGI object");

                       is_deeply(\%ENV, {}, "No env yet");
                       my $params = parse_params(shift);
                       # is_deeply($params, \%params, "params chunk");
                       $fd = eval { fileno(STDIN) };
                       ok($@, "STDIN is tied without FILENO");
                       $fd = eval { fileno(STDOUT) };
                       ok($@, "STDOUT is tied without FILENO");
                       $fd = eval { fileno(STDERR) };
                       ok($@, "STDERR is tied without FILENO");
                   },
                   ParamsEof => sub {
                       is(++$hit, 11, "param eof");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       isa_ok(shift, "Faker", "Class");
                       is($CGI::Q, undef, "No default CGI object");

                       is_deeply(shift, \%params, "params chunk");
                       is_deeply(\%ENV, \%params, "Env set");
                   },
                   UnknownApplicationRecord => sub {
                       isa_ok(shift, "WEC::FastCGI::Request");
                       is(shift, 0, "Type 0");
                       is(shift, "Farm", "Right data");
                       is(++$hit, 2, "Unknown app record callback");
                   }
                   ]);
    {
        local $SIG{__WARN__} = sub { 
            diag("Unexpected warning: @_");
            fail("There should be no warning");
        };
        is(plain_loop(), "bat", "Right returncode from loop");
    }
    is($hit, 24, "Executed all parts");
    is($ENV{foo}, "bar", "Environment is restored");
    is($CGI::Q, undef, "No default CGI object");
    check_fd();
    check_objects();

    # Again sending stuff all over the place, but everything a bit different
    %params = (foo => 5, bar => 6);
    $c = make_pair([
               Connect => sub {
                   $hit++;
                   my $conn = shift;
                   my $r = $conn->request;
                   $r->stdin("a ");
                   $r->stdin_flush;
                   $r->stdin("stdin");
                   $r->data("a ");
                   $r->data_flush;
                   $r->data("data");

                   $r->params_close(\%params);
                   eval { $r->params_close(\%params) };
                   like($@, qr/already closed/i, "Can redo params");
                   eval { $r->params_flush };
                   like($@, qr/already closed/i, "Can flush closed params");
                   eval { $r->params_close };
                   like($@, qr/already closed/i, "Can redo params_close");

                   $r->data_close;
                   eval { $r->data("foo") };
                   like($@, qr/already closed/i, "Can redo data");
                   eval { $r->data_flush };
                   like($@, qr/already closed/i, "Can flush closed data");
                   eval { $r->data_close };
                   like($@, qr/already closed/i, "Can redo data_close");

                   $r->stdin_close;
                   eval { $r->stdin("foo") };
                   like($@, qr/already closed/i, "Can redo stdin");
                   eval { $r->stdin_flush };
                   like($@, qr/already closed/i, "Can flush closed stdin");
                   eval { $r->stdin_close };
                   like($@, qr/already closed/i, "Can redo stdin_close");
               },
               RequestEnd => sub {
                   $hit++;
                   is(shift->id, 1, "Got end for request 1");
                   is(shift, 0, "Second arg is application status");
                   is(shift, 0, "Third arg is protocol status");
                   $server->abort;
                   $c->close;
                   unloop("bat");
               },
               Stdout => sub {
                   is(++$hit, 11, "Tenth STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
                   is(shift, "a stdout");
               },
               StdoutEof => sub {
                   is(++$hit, 12, "Eleventh close STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
               },
               Stderr => sub {
                   isa_ok(shift, "WEC::FastCGI::Request");
                   if (++$hit == 10) {
                       is(shift, "a ", "Eigth stderr");
                   } elsif ($hit == 13) {
                       is(shift, "stderr", "Twelfth stderr");
                   } else {
                       is($hit, 13, "Unexpected stderr");
                   }
               },
               StderrEof => sub {
                   is(++$hit, 14, "Thirteenth close STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
               }
               ], [CgiClass	=> "Faker",
                   FakeEnv	=> 0,
                   TieStdin	=> 0,
                   TieStdout	=> 0,
                   TieStderr	=> 0,
                   Stdin => sub {
                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       if (++$hit == 2) {
                           is(shift, "a ", "stdin chunk");
                       } elsif ($hit == 8) {
                           is(shift, "stdin", "stdin chunk");
                       } else {
                           is($hit, 8, "Expected stdin");
                       }
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                       $fd = eval { fileno(STDIN) };
                       is($@, "", "STDIN is not tied");
                       $fd = eval { fileno(STDOUT) };
                       is($@, "", "STDOUT is not tied");
                       $fd = eval { fileno(STDERR) };
                       is($@, "", "STDERR is not tied");
                   },
                   StdinEof => sub {
                       is(++$hit, 9, "Eigth data eof");
                       my $r = shift;
                       isa_ok($r, "WEC::FastCGI::Request");
                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");
                       is(shift, "a stdin", "stdin chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                       $fd = eval { fileno(STDIN) };
                       is($@, "", "STDIN is not tied");
                       $fd = eval { fileno(STDOUT) };
                       is($@, "", "STDOUT is not tied");
                       $fd = eval { fileno(STDERR) };
                       is($@, "", "STDERR is not tied");

                       $r->stdout("a ");
                       $r->stdout("std");
                       $r->stdout("out");
                       $r->stderr("a ");
                       $r->stdout_close;
                       eval { $r->stdout("foo") };
                       ok($@, "Can redo stdout");
                       eval { $r->stdout_close };
                       ok($@, "Can redo stdout_close");
                       $r->stderr("stderr");
                       $r->stderr_close;
                       eval { $r->stderr("foo") };
                       ok($@, "Can redo stderr");
                       eval { $r->stderr_close };
                       ok($@, "Can redo stderr_close");
                   },
                   Data => sub {
                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       if (++$hit == 3) {
                           is(shift, "a ", "data chunk");
                       } elsif ($hit == 6) {
                           is(shift, "data", "data chunk");
                       } else {
                           is($hit, 6, "Expected data");
                       }
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   DataEof => sub {
                       is(++$hit, 7, "Sixth data eof");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is(shift, "a data", "data chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   Params => sub {
                       is(++$hit, 4, "Third a param");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                       my $params = shift;
                       $params eq "\x03\x01bar6\x03\x01foo5" ||
                           $params eq "\x03\x01foo5\x03\x01bar6" ?
                           pass("params chunk") : fail("params chunk");
                       $fd = eval { fileno(STDIN) };
                       is($@, "", "STDIN is not tied");
                       $fd = eval { fileno(STDOUT) };
                       is($@, "", "STDOUT is not tied");
                       $fd = eval { fileno(STDERR) };
                       is($@, "", "STDERR is not tied");
                   },
                   ParamsEof => sub {
                       is(++$hit, 5, "Fourth param eof");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is_deeply(shift, \%params, "params chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   ]);
    is(loop(), "bat", "Right returncode from loop");
    is($hit, 15, "Executed all parts");
    is($ENV{foo}, "bar", "Environment is restored");
    is($CGI::Q, undef, "No default CGI object");
    check_fd();
    check_objects();

    # Carefully checking streams semantics
    my $final_env = { fe => "hg",
                      "m" x 128 => "o" x 128,
                      "x" => "yz" };
    $c = make_pair([
               Connect => sub {
                   is(++$hit, 1, "Connected");
                   my $conn = shift;
                   my $r = $conn->request;
                   $r->stdin("ab");	$r->stdin_flush;
                   $r->data("cd");	$r->data_flush;
                   $r->params("e" x 127 => "g" x 127); $r->params_flush;

                   $r->stdin("ij");	$r->stdin_flush;
                   $r->data("kl");	$r->data_flush;
                   $r->params("m" x 128 => "o" x 128); $r->params_flush;
               },
               RequestEnd => sub {
                   $hit++;
                   is(shift->id, 1, "Got end for request 1");
                   is(shift, 0, "Second arg is application status");
                   is(shift, 0, "Third arg is protocol status");
                   $server->abort;
                   $c->close;
                   unloop("baf");
               },
               Stdout => sub {
                   $hit++;
                   if ($hit == 8) {
                       is($_[1], "pq", "Right Stdout");
                       $_[1] = "qp3";
                   } elsif ($hit == 17) {
                       # Stdout after stderr because stderr is autoflush
                       is($_[1], "AB", "Right stdout");
                   } else {
                       diag("Stdout hit $hit");
                       fail("Stdout hit $hit");
                   }
               },
               StdoutEof => sub {
                   is(++$hit, 18, "Stdout ended");
                   is($_[1], "qp3AB", "right stdout buffer");
               },
               Stderr => sub {
                   $hit++;
                   if ($hit == 9) {
                       my $r = $_[0];
                       is($_[1], "rs", "Right Stderr");
                       $_[1] = "sr4";
                       $r->data("vw");  $r->data_close;
                       $r->params("xy" => "z" x 129); $r->params_close;
                       $r->stdin("tu"); $r->stdin_close;
                   } elsif ($hit == 16) {
                       # Stderr before stdout because stderr is autoflush
                       is($_[1], "CD", "Right STDERR");
                   } else {
                       fail("Stderr hit $hit");
                   }
               },
               StderrEof => sub {
                   is(++$hit, 19, "Stderr ended");
                   is($_[1], "sr4CD", "right stderr buffer");
               }
               ], [CgiClass	=> "Faker",
                   Stdin => sub {
                       $hit++;
                       if ($hit == 2) {
                           is(<STDIN>, undef, "No stdin arrived yet");
                           is($_[2], "ab", "But first buffer is here");
                           $_[2] = "ba1";
                           is_deeply(\%ENV, {}, "no %ENV yet");
                       } elsif ($hit == 5) {
                           is(<STDIN>, "ba1", "Modified buffer arrived");
                           is(<STDIN>, undef, "Consumed by read");
                           is($_[2], "ij", "New data is available");
                           is_deeply(\%ENV, {}, "no %ENV yet");
                       } elsif ($hit == 14) {
                           is(<STDIN>, "ij", "Unmodified buffer arrived");
                           is(<STDIN>, undef, "Consumed by read");
                           is($_[2], "tu", "New data is available");
                           $_[2] = "vroom\nZoem\n";
                           is_deeply(\%ENV, $final_env, "%ENV is set up");
                       } else {
                           fail("Stdin hit $hit");
                       }
                   },
                   StdinEof => sub {
                       is(++$hit, 15);
                       is ($_[2], "vroom\nZoem\n", "Final stdin arrived");
                       is(<STDIN>, "vroom\n", "Good STDIN parsing");
                       is(<STDIN>, "Zoem\n", "Good STDIN parsing");
                       is(<STDIN>, undef, "STDIN eof");
                       $_[2] = "Waf";
                       is(<STDIN>, "Waf", "STDIN can be controlled");
                       is_deeply(\%ENV, $final_env, "%ENV is set up");
                       print STDOUT "AB";
                       print STDERR "CD";
                   },
                   Data => sub {
                       $hit++;
                       is_deeply(\%ENV, {}, "no %ENV yet");
                       if ($hit == 3) {
                           is($_[2], "cd", "But first buffer is here");
                           $_[2] = "dc";
                       } elsif ($hit == 6) {
                           is($_[2], "kl", "New data is available");
                       } elsif ($hit == 10) {
                           is($_[2], "vw", "New data is available");
                           $_[2] = "Goud\nVis\n";
                       } else {
                           fail("Data hit $hit");
                       }
                   },
                   DataEof => sub {
                       is(++$hit, 11);
                       is($_[2], "dcklGoud\nVis\n", "Final data arrived");
                       is_deeply(\%ENV, {}, "no %ENV yet");
                   },
                   Params => sub {
                       $hit++;
                       is_deeply(\%ENV, {}, "no %ENV yet");
                       if ($hit == 4) {
                           is($_[2],
                              "\x7f\x7f" . "e" x 127 . "g" x 127,
                              "But first buffer is here");
                           $_[2] = "\x02\x02fehg";
                       } elsif ($hit == 7) {
                           is($_[2],
                              "\x80\x00\x00\x80"x2 . "m" x 128 . "o" x 128,
                              "New params is available");
                           my $r = $_[0];
                           $r->stdout("pq");
                           $r->stdout_flush;
                           $r->stderr("rs");
                           $r->stderr_flush;
                       } elsif ($hit == 12) {
                           is($_[2], "\x02\x80\x00\x00\x81xy" . "z" x 129);
                           $_[2] = "\x01\x02xyz";
                       } else {
                           fail("Stdin hit $hit");
                       }
                   },
                   ParamsEof => sub {
                       is(++$hit, 13, "Parameters ended");
                       is_deeply($_[2], $final_env, "Parameters arrived");
                       is_deeply(\%ENV, $final_env, "Parameters arrived");
                   },
                   ]);
    is(loop(), "baf", "Right returncode from loop");
    is($hit, 20, "Executed all parts");
    is($ENV{foo}, "bar", "Environment is restored");
    check_fd();
    check_objects();

    # Yet another way of sending stuff all over the place
    %params = (bar => 9);
    $c = make_pair([
               Connect => sub {
                   $hit++;
                   my $conn = shift;
                   my $r = $conn->request(FILTER(),
                                          \%params, "a stdin", "a data");
                   eval { $r->params_close(\%params) };
                   ok($@, "Can't redo params");
                   eval { $r->data("foo") };
                   ok($@, "Can't redo data");
                   eval { $r->data_close };
                   ok($@, "Can't redo data_close");
                   eval { $r->stdin("foo") };
                   ok($@, "Can't redo stdin");
                   eval { $r->stdin_close };
                   ok($@, "Can't redo stdin_close");
               },
               RequestEnd => sub {
                   $hit++;
                   is(shift->id, 1, "Got end for request 1");
                   is(shift, 0, "Second arg is application status");
                   is(shift, 0, "Third arg is protocol status");
                   $server->abort;
                   $c->close;
                   unloop("bat");
               },
               Stdout => sub {
                   is(++$hit, 10, "Ninth STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
                   is(shift, "a stdout");
               },
               StdoutEof => sub {
                   is(++$hit, 11, "Tenth close STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
               },
               Stderr => sub {
                   isa_ok(shift, "WEC::FastCGI::Request");
                   if (++$hit == 8) {
                       is(shift, "a ", "Seventh stderr");
                   } elsif ($hit == 9) {
                       is(shift, "stderr", "Eigth stderr");
                   } else {
                       is($hit, 9, "Unexpected stderr");
                   }
               },
               StderrEof => sub {
                   is(++$hit, 12, "Eleventh close STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
               }
               ], [CgiClass	=> "Faker",
                   FakeEnv	=> 0,
                   Stdin => sub {
                       is(++$hit, 4, "Third data eof");
                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is(shift, "a stdin", "stdin chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   StdinEof => sub {
                       is(++$hit, 5, "Fourth data eof");

                       my $r = shift;
                       isa_ok($r, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is(shift, "a stdin", "stdin chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");

                       $r->stdout("a ");
                       $r->stdout("std");
                       $r->stdout("out");
                       $r->stderr("a ");
                       $r->stderr("stderr");
                   },
                   Data => sub {
                       is(++$hit, 6, "Fifth data");
                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is(shift, "a data", "data chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   DataEof => sub {
                       is(++$hit, 7, "Sixth data eof");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is(shift, "a data", "data chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   Params => sub {
                       is(++$hit, 2, "First a param");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                       is(shift,
                          "\x0e\x01CONTENT_LENGTH7" .
                          "\x10\x01FCGI_DATA_LENGTH6" .
                          "\x03\x01bar9", "Expected params");
                   },
                   ParamsEof => sub {
                       is(++$hit, 3, "Second param eof");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is_deeply(shift, {%params,
                                         FCGI_DATA_LENGTH => 6,
                                         CONTENT_LENGTH	  => 7,
                                     },
                                 "params chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   ]);
    is(loop(), "bat", "Right returncode from loop");
    is($hit, 13, "Executed all parts");
    is($ENV{foo}, "bar", "Environment is restored");
    is($CGI::Q, undef, "No default CGI object");
    check_fd();
    check_objects();

    # Plain request
    %params = (bar => 9);
    $c = make_pair([
               Connect => sub {
                   $hit++;
                   my $conn = shift;
                   my $r = $conn->request(RESPONDER(),
                                          \%params, "a stdin", "a data");
               },
               RequestEnd => sub {
                   $hit++;
                   is(shift->id, 1, "Got end for request 1");
                   is(shift, 0, "Second arg is application status");
                   is(shift, 0, "Third arg is protocol status");
                   $server->abort;
                   $c->close;
                   unloop("bat");
               },
               Stdout => sub {
                   is(++$hit, 8, "Seventh STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
                   is(shift, "a stdout");
               },
               StdoutEof => sub {
                   is(++$hit, 9, "Eigth close STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
               },
               Stderr => sub {
                   isa_ok(shift, "WEC::FastCGI::Request");
                   if (++$hit == 6) {
                       is(shift, "a ", "Fifth stderr");
                   } elsif ($hit == 7) {
                       is(shift, "stderr", "Sixth stderr");
                   } else {
                       is($hit, 7, "Unexpected stderr");
                   }
               },
               StderrEof => sub { is(++$hit, 10, "Ninth close STDOUT") }
               ], [CgiClass	=> "Faker",
                   FakeEnv	=> 0,
                   Stdin => sub {
                       is(++$hit, 4, "Third data eof");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is(shift, "a stdin", "stdin chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   StdinEof => sub {
                       is(++$hit, 5, "Fourth data eof");

                       my $r = shift;
                       isa_ok($r, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is(shift, "a stdin", "stdin chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");

                       $r->stdout("a ");
                       $r->stdout("std");
                       $r->stdout("out");
                       $r->stderr("a ");
                       $r->stderr("stderr");
                   },
                   Data => sub { die "data" },
                   DataEof => sub { die "DataEof" },
                   Params => sub {
                       is(++$hit, 2, "First a param");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                       is (shift, "\x0e\x01CONTENT_LENGTH7\x03\x01bar9",
                           "params chunk");
                   },
                   ParamsEof => sub {
                       is(++$hit, 3, "Second param eof");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is_deeply(shift, {%params,
                                         CONTENT_LENGTH => 7,
                                     }, "params chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   ]);
    is(loop(), "bat", "Right returncode from loop");
    is($hit, 11, "Executed all parts");
    is($ENV{foo}, "bar", "Environment is restored");
    is($CGI::Q, undef, "No default CGI object");
    check_fd();
    check_objects();

    # Authorizer request
    %params = (foo => 8, bar => 9);
    $c = make_pair([
               Connect => sub {
                   $hit++;
                   my $conn = shift;
                   my $r = $conn->request(AUTHORIZER(),
                                          \%params, "a stdin", "a data");
               },
               RequestEnd => sub {
                   $hit++;
                   is(shift->id, 1, "Got end for request 1");
                   is(shift, 0, "Second arg is application status");
                   is(shift, 0, "Third arg is protocol status");
                   $server->abort;
                   $c->close;
                   unloop("bat");
               },
               Stdout => sub {
                   is(++$hit, 6, "Fifth STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
                   is(shift, "a stdout");
               },
               StdoutEof => sub {
                   is(++$hit, 7, "Sixth close STDOUT");
                   isa_ok(shift, "WEC::FastCGI::Request");
               },
               Stderr => sub {
                   isa_ok(shift, "WEC::FastCGI::Request");
                   if (++$hit == 4) {
                       is(shift, "a ", "Third stderr");
                   } elsif ($hit == 5) {
                       is(shift, "stderr", "Fourth stderr");
                   } else {
                       is($hit, 5, "Unexpected stderr");
                   }
               },
               StderrEof => sub { is(++$hit, 8, "Seventh close STDOUT") }
               ], [CgiClass	=> "Faker",
                   FakeEnv	=> 0,
                   Stdin	=> sub { die "stdin" },
                   StdinEof	=> sub { die "StdinEof" },
                   Data		=> sub { die "data" },
                   DataEof	=> sub { die "DataEof" },
                   Params	=> sub {
                       is(++$hit, 2, "First a param");

                       my $r = shift;
                       isa_ok($r, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                       my $params = shift;
                       $params eq "\x03\x01bar9\x03\x01foo8" ||
                           $params eq "\x03\x01foo8\x03\x01bar9" ?
                           pass("params chunk") : fail("params chunk");
                       $r->stdout("a ");
                       $r->stdout("std");
                       $r->stdout("out");
                       $r->stderr("a ");
                       $r->stderr("stderr");
                   },
                   ParamsEof => sub {
                       is(++$hit, 3, "Second param eof");

                       isa_ok(shift, "WEC::FastCGI::Request");

                       my $o = shift;
                       isa_ok($o, "Faker", "Class");
                       is($CGI::Q, $o, "Right default CGI object");

                       is_deeply(shift, \%params, "params chunk");
                       is_deeply($ENV{foo}, "bar", "Env is preserved");
                   },
                   ]);
    is(loop(), "bat", "Right returncode from loop");
    is($hit, 9, "Executed all parts");
    is($ENV{foo}, "bar", "Environment is restored");
    is($CGI::Q, undef, "No default CGI object");
    check_fd();
    check_objects();

    # Speed test of one request per connection
    %params = (foo => 8, bar => 9);
    my $now;
    $c = make_pair([
               RequestEnd => sub {
                   $hit++;
                   # A real app would get all these
                   my $r = shift;
                   my $app = shift;
                   my $proto = shift;
                   $r->stdout;
                   $c->close;

                   if ($now >= time) {
                       $c = $client->connect;
                       $c->request(\%params, "tttt a stdin");
                   } else {
                       my $ms = sprintf("%.1f", (time()-$now+1)*1000/$hit);
                       diag("$type: Connection per request roundtrip: $ms ms");
                       is($c->flows, 0, "All client flows are cleaned up");
                       $server->abort;
                       unloop("bat");
                   }
               }], [StdinEof	=> sub {
                   print <STDIN>;
               }]);
    $now=1+int(time);
    1 while $now > time;
    $now = time()+1;
    $c->request(\%params, "ssss a stdin");
    is(loop(), "bat", "Right returncode from loop");

    # Speed test of all requests on one connection
    %params = (foo => 8, bar => 9);
    $c = make_pair([
               Connect => sub {
                   $now=1+int(time);
                   1 while $now > time;
                   $now = time()+1;
                   shift->request(\%params, "a stdin") },
               RequestEnd => sub {
                   $hit++;
                   # A real app would get all these
                   my $r = shift;
                   my $app = shift;
                   my $proto = shift;
                   # Fake stdout fetch
                   $r->stdout;

                   if ($now >= time) {
                       $c->request(\%params, "a stdin");
                   } else {
                       my $ms = sprintf("%.1f", (time()-$now+1)*1000/$hit);
                       diag("$type: All on one connection  roundtrip: $ms ms");
                       is($c->flows, 1, "All client flows except current are cleaned up");
                       $c->close;
                       $server->abort;
                       unloop("bat");
                   }
               }], [StdinEof	=> sub {
                   print <STDIN>;
               }]);
    is(loop(), "bat", "Right returncode from loop");
    check_fd();
    check_objects();

    # Early "end" while still have stuff pending
    %params = (foo => 5, bar => 6);
    $c = make_pair([
               Connect => sub {
                   $hit++;
                   my $conn = shift;
                   my $r = $conn->request;
                   $r->stdin("a stdin");
                   $r->data("a data");
                   $r->params_close(\%params);
                   $r->data_close;
                   $r->stdin_close;
               },
               RequestEnd => sub {
                   is (++$hit, 9, "Eight result");
                   is(shift->id, 1, "Got end for request 1");
                   is(shift, 8, "Second arg is application status");
                   is(shift, 9, "Third arg is protocol status");
                   $server->abort;
                   $c->close;
                   unloop("bat");
               },
               Stdout => sub {
                   if (++$hit == 3) {
                   } elsif ($hit == 6) {
                   } else {
                       fail("Unexpected stdout");
                   }
               },
               StdoutEof => sub {
                   is(++$hit, 7, "Ninth close STDOUT");
               },
               Stderr => sub {
                   isa_ok(shift, "WEC::FastCGI::Request");
                   if (++$hit == 4) {
                       is(shift, "a ", "Second stderr");
                   } elsif ($hit == 5) {
                       is(shift, "stderr", "Third stderr");
                   } else {
                       fail("Unexpected stderr");
                   }
               },
               StderrEof => sub {
                   is(++$hit, 8, "Eleventh close STDOUT");
               }
               ], [CgiClass	=> "Faker",
                   Stdin	=> sub { die "stdin" },
                   StdinEof	=> sub { die "StdinEof" },
                   Data	=> sub { die "data" },
                   DataEof	=> sub { die "DataEof" },
                   Params	=> sub {
                       is(++$hit, 2, "First a param");
                       my $r = shift;
                       $r->stdout("a ");
                       $r->stdout("std");
                       $r->stdout_flush;
                       $r->stderr("a ");
                       $r->stderr_flush;
                       $r->stdout("out");
                       $r->stderr("stderr");
                       $r->end(8, 9);
                       eval { $r->stdout("a") };
                       ok($@, "Can't send more on stdout");
                       eval { $r->stdout_flush };
                       ok($@, "Can't flush stdout");
                       eval { $r->stdout_close };
                       ok($@, "Can't reclose stdout");
                       eval { $r->stderr("a") };
                       ok($@, "Can't send more on stderr");
                       eval { $r->stderr_flush };
                       ok($@, "Can't flush stderr");
                       eval { $r->stderr_close };
                       ok($@, "Can't reclose stderr");
                   },
                   ParamsEof => sub { die "ParamsEof" },
                   ]);
    is(loop(), "bat", "Right returncode from loop");
    is($hit, 9, "Executed all parts");
    check_fd();
    check_objects();

    # A real CGI simulation
    %params = (foo => 5, bar => 6);
    $c = make_pair([
               Connect => sub {
                   $hit++;
                   shift->request({
                       REQUEST_METHOD	=> "GET",
                       QUERY_STRING	=> query_string(\%params),
                   }, "");
               },
               RequestEnd => sub {
                   is (++$hit, 2, "Eight result");
                   my $r = shift;
                   is($r->id, 1, "Got end for request 1");
                   is(shift, 0, "Second arg is application status");
                   is(shift, 0, "Third arg is protocol status");
                   my $q = CGI->new;
                   my $str =
                       $q->header() .
                       $q->start_html("A Simple Example") .
                       $q->h1("A Simple Example") .
                       join("", map("$_ => $params{$_}", sort keys %params)) .
                       $q->end_html();
                   is($r->stdout, $str, "Expected output");
                   $server->abort;
                   $c->close;
                   unloop("bat");
               },
               ], [StdinEof	=> sub {
                   shift;

                   my $q = shift;
                   is($q, $CGI::Q, "Default CGI is set");
                   isa_ok($q, "WEC::FastCGI", "Good default class");

                   print($q->header,
                         $q->start_html("A Simple Example"),
                         $q->h1("A Simple Example"),
                         map("$_ => " . $q->param($_), sort $q->param()),
                         $q->end_html);
               }]);
    is(loop(), "bat", "Right returncode from loop");
    is($hit, 2, "Executed all parts");
}
check_fd();
check_objects();

1;

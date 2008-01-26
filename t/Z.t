use warnings;
use strict;
use Test::More tests => 1;
pass("dummy");

diag("\nroundtrip = the complete action:\n -Client marshals request, sends it\n -Server receives and unmarshals\n  constructs CGI object\n  create, marshal and send an answer\n  tear down CGI object\n -Client receives and unmarshals answer");


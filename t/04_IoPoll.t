# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 04_IoPoll.t'
use warnings;
use strict;

use Test::More "no_plan";

is($WEC::kernel_type, undef, "No event class set");
use_ok('WEC', qw(IO::Poll));
is($WEC::kernel_type, "WEC::IO::Poll", "Right Event class set");
use_ok("t::TestKernel");

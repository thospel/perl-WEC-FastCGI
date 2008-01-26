# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl IoSelect.t'

use warnings;
use strict;

use Test::More "no_plan";

is($WEC::kernel_type, undef, "No event class set");
use_ok('WEC', qw(IO::Select));
is($WEC::kernel_type, "WEC::IO::Select", "Right Event class set");
use_ok("t::TestKernel");

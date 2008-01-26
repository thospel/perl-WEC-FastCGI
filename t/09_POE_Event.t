# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl POE.t'

use warnings;
use strict;

use Test::More;

unless (eval { require Event }) {
        plan skip_all => "Can't find the Event module";
        exit;
}
unless (eval { require POE }) {
        plan skip_all => "Can't find the POE module";
        exit;
}
plan "no_plan";

is($WEC::kernel_type, undef, "No event class set");
use_ok('WEC');
require POE::Kernel;
my @poe_type;
for (keys %INC) {
    push @poe_type, $1 if
        m!^POE/(?:XS/)?Loop/(.+)\.pm\z! && $1 ne "PerlSignals";
}
ok($WEC::kernel_type eq "WEC::POE" && @poe_type == 1 && $poe_type[0] eq "Event",
        "Right Event class set");
SKIP: {
    # skip "Session cleanup bug", 597;
    use_ok("t::TestKernel");
};

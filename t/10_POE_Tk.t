# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl POE.t'

use warnings;
use strict;

use Test::More;

unless (eval { require Tk }) {
        plan skip_all => "Can't find the Tk module";
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
my %bad = map {$_ => 1} qw(PerlSignals TkCommon TkActiveState);
for (keys %INC) {
    push @poe_type, $1 if
        m!^POE/(?:XS/)?Loop/(.+)\.pm\z! && !$bad{$1};
}
ok($WEC::kernel_type eq "WEC::POE" && @poe_type == 1 && $poe_type[0] eq "Tk",
        "Right Event class set");
use_ok("t::TestKernel");

sub prepare_loop {
    no warnings "once";
    $POE::Kernel::poe_main_window->geometry("+10+10");
}

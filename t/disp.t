#!/usr/local/lib/perl -w

use strict;

use Test;
use XML::Filter::Dispatcher;
use XML::SAX::ParserFactory;
use UNIVERSAL;

my $have_test_diff = eval "use Test::Differences; 1";

my @log;

sub rule($) {
    my $rule = shift;
    return ( $rule => sub {
            my ( $self ) = shift;
            my ( $foo ) = @_;
            push @log, $rule . "_" . ( $foo->{Name} || $foo->{Data} || "" );
        }
    );
}

sub d {
    my $doc = shift;
    @log = ();
    my $options = @_ && ref $_[-1] ? pop : {};

    my $d = XML::Filter::Dispatcher->new(
        Rules => [ map rule $_, @_ ],
        %$options,
    );
    $d->as_png( "| cat > foo.png ; ee foo.png" ) if $options->{Graph};
    my $p = XML::SAX::ParserFactory->parser( Handler => $d );
    $p->parse_string( $doc );
    $d;  ## So caller can do ->as_png() easily :)
}

my $abcd    = "<a>s<b>t<c>u<d id='1' name='n1'>v</d><d id='2'>w</d>x</c>y</b>z</a>";
my $abcdBcd = "<a><b><c><d/><d/></c></b><B><c><d/><d/></c></B></a>";


my @tests = (
sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/" );
    eq_or_diff( \@log, [ "/_" ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/child::a" );
    eq_or_diff( \@log, [ "/child::a_a" ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/a" );
    eq_or_diff( \@log, [ "/a_a" ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/a/b/c" );
    eq_or_diff( \@log, [ "/a/b/c_c" ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/a/b/c/d" );
    eq_or_diff( \@log, [ qw( /a/b/c/d_d /a/b/c/d_d ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/child::*" );
    eq_or_diff( \@log, [ "/child::*_a" ] );
},

##
## //descendant-or-self::node()
##
sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/descendant-or-self::node()" );
    eq_or_diff( \@log, [ qw(
        /descendant-or-self::node()_
        /descendant-or-self::node()_a
        /descendant-or-self::node()_b
        /descendant-or-self::node()_c
        /descendant-or-self::node()_d
        /descendant-or-self::node()_d
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/descendant-or-self::node()/node()" );
    eq_or_diff( \@log, [ qw(
        /descendant-or-self::node()/node()_a
        /descendant-or-self::node()/node()_b
        /descendant-or-self::node()/node()_c
        /descendant-or-self::node()/node()_d
        /descendant-or-self::node()/node()_d
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "//node()" );
    eq_or_diff( \@log, [ qw(
        //node()_a
        //node()_b
        //node()_c
        //node()_d
        //node()_d
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/descendant-or-self::node()/a" );
    eq_or_diff( \@log, [ qw(
        /descendant-or-self::node()/a_a
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "//a" );
    eq_or_diff( \@log, [ qw(
        //a_a
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/descendant-or-self::node()/b" );
    eq_or_diff( \@log, [ qw(
        /descendant-or-self::node()/b_b
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "//b" );
    eq_or_diff( \@log, [ qw(
        //b_b
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/descendant-or-self::node()/d" );
    eq_or_diff( \@log, [ qw(
        /descendant-or-self::node()/d_d
        /descendant-or-self::node()/d_d
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "//d" );
    eq_or_diff( \@log, [ qw(
        //d_d
        //d_d
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcdBcd, "/a/B//d" );
    eq_or_diff( \@log, [ qw(
        /a/B//d_d
        /a/B//d_d
    ) ] );
},

##
## self::node()
##
sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/self::node()" );
    eq_or_diff( \@log, [ qw(
        /self::node()_
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/self::node()/a" );
    eq_or_diff( \@log, [ qw(
        /self::node()/a_a
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "/./a" );
    eq_or_diff( \@log, [ qw(
        /./a_a
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "//./a" );
    eq_or_diff( \@log, [ qw(
        //./a_a
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "//./d" );
    eq_or_diff( \@log, [ qw(
        //./d_d
        //./d_d
    ) ] );
},

##
## attribute::
##
sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "//attribute::id" );
    eq_or_diff( \@log, [ qw(
        //attribute::id_id
        //attribute::id_id
    ) ] );
},

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "//attribute::*", );
    eq_or_diff( [ sort @log ], [ qw(
        //attribute::*_id
        //attribute::*_id
        //attribute::*_name
    ) ] );
},

##
## text()
##

sub {
    skip "Needs Test::Differences to test", 1 unless $have_test_diff;
    d( $abcd, "//attribute::id" );
    eq_or_diff( \@log, [ qw(
        //attribute::id_id
        //attribute::id_id
    ) ] );
},

);

plan tests => scalar @tests;

$_->() for @tests;

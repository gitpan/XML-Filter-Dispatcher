#!/usr/local/lib/perl -w

use strict;

use Test;
use XML::Filter::Dispatcher::Parser;
use UNIVERSAL;

use Data::Dumper;

$Data::Dumper::Indent = 1;
$Data::Dumper::Quotekeys = 0;
$Data::Dumper::Terse = 1;

my @tests = (
sub {
    my $ast = XML::Filter::Dispatcher::Parser->parse( "/a/b/c" ) ;
    ok 1;
},

sub {
    my $ast = XML::Filter::Dispatcher::Parser->parse( "/node()" ) ;
#    $ast->as_png( "| cat > foo.png ; ee foo.png" );
    ok 1;
},

sub {
    my $ast = XML::Filter::Dispatcher::Parser->parse(
        "/descendant-or-self::node()"
    ) ;
    ok 1;
},

);

plan tests => scalar @tests;

$_->() for @tests;

#!/usr/local/lib/perl -w

use strict;

use Test;
use XML::SAX::PurePerl;
use XML::Filter::Dispatcher qw( :all );
use XML::SAX::Writer;

use UNIVERSAL;

my( $doc_root_out, $foo_out );

my $d = XML::Filter::Dispatcher->new(
    Rules => [
        "/"   => XML::SAX::Writer->new( Output => \$doc_root_out ),
        "foo" => XML::SAX::Writer->new( Output => \$foo_out ),
    ],
);

my $p = XML::SAX::PurePerl->new( Handler => $d );

$p->parse_string( "<root><subroot><foo><bar/></foo><foo><bar/></foo></subroot></root>" );

sub my_ok {
    my ( $got, $expected ) = @_;

    @_ = ( 1 )
        if $got =~ $expected;

    goto &ok;
}

my @tests = (
sub { my_ok $doc_root_out, qr{<root><subroot\s*/></root>}          },
sub { my_ok $foo_out, qr{<foo><bar\s*/></foo><foo><bar\s*/></foo>} },
);

plan tests => scalar @tests;

$_->() for @tests;


#!/usr/local/lib/perl -w

use strict;

use Test;
use XML::SAX::PurePerl;
use XML::Filter::Dispatcher qw( :all );
use XML::SAX::Writer;

use UNIVERSAL;

my( $doc_root_out, $foo_out );

## XML::SAX::Writer clears the output string every start_document() as
## of this writing.  Not sure it always will, but it might.
sub finalize{}
sub output {
   $foo_out .= $_[1];
}

my $d = XML::Filter::Dispatcher->new(
    Rules => [
        "/"   => XML::SAX::Writer->new( Output => \$doc_root_out ),
        "foo" => XML::SAX::Writer->new( Output => bless \$a, "main" ),
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


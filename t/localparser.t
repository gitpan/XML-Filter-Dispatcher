#!/usr/local/lib/perl -w

use strict;

use Test;
use XML::SAX::ParserFactory;
use XML::SAX::Writer;

my $out;
my $w = XML::SAX::Writer->new( Output => \$out );
my $p = XML::SAX::ParserFactory->parser( Handler => $w );

use XML::SAX::PurePerl; $p = XML::SAX::PurePerl->new( Handler => $w );

my $warned;
sub w {
    return if $warned++;

    warn "    *** Your XML parser (", ref $p, ") seems to be broken\n";
    warn "    *** This will cause miscellaneous failures in the test suite\n";
}


my @tests = (
sub {
    $out = "";
    my $doc = "<!--A--><a/>";
    $p->parse_string( $doc );
    if ( $out =~ m{\A<!--A--><a\s*/>\Z} ) {
        ok 1;
    }
    else {
        w;
        ok $out, $doc, "comment before roo elt.";
    }
},

sub {
    $out = "";
    my $doc = "<?A?><a/>";
    $p->parse_string( $doc );
    if ( $out =~ m{\A<\?A\s*\?><a\s*/>\Z} ) {
        ok 1;
    }
    else {
        w;
        ok $out, $doc, "comment before roo elt.";
    }
},

);

plan tests => scalar @tests;

$_->() for @tests;

#!/usr/local/lib/perl -w

use strict;

use Test;
use XML::Filter::Dispatcher::Parser;
use UNIVERSAL;

my $has_test_diff;
BEGIN {
    $has_test_diff = eval "use Test::Differences; 1";
    eval "sub eq_or_diff {}" unless $has_test_diff;
}

sub t_lex($) {
    my ( $s ) = @_ ;

    my $p =  {
        USER => {
            Input => $s,
        },
    };

    my @result;
    while (1) {
        my @token_pair = XML::Filter::Dispatcher::Parser::lex( $p );
        last unless length $token_pair[0];
        push @result, \@token_pair;
    }

    return \@result;
}


my @tests = (
##
## A quick probe to see if it works at all...
##
sub {
    t_lex "a";
    ok 1;
},

##
## Test the disambiguation rules (not fully, but just to see if we get
## the expected result from a simple matching pattern; if we wanted to
## be complete, we would have to try to fool the thing).
##
sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;

    eq_or_diff t_lex "a*", [
        [ QNAME    => "a" ],
        [ MULTIPLY => "*" ],
    ];
},

( 
    map {
        my $op_name = $_;
        sub {
            return skip "Need Test::Differences", 1 unless $has_test_diff;

            eq_or_diff t_lex "a $op_name", [
                [ QNAME       => "a"      ],
                [ uc $op_name => $op_name ],
            ];
        };
    } qw( and or mod div )
),

sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;

    eq_or_diff t_lex "a (", [
        [ FUNCTION_NAME => "a" ],
        [ LPAR          => "(" ],
    ];
},

( 
    map {
        my $node_type = $_;
        my $exp_token = $_ eq "processing-instruction" ? "PI" : uc $_;

        sub {
            return skip "Need Test::Differences", 1 unless $has_test_diff;

            eq_or_diff t_lex "$node_type (", [
                [ $exp_token  => $node_type ],
                [ LPAR        => "("        ],
            ];
        };
    } qw( text comment processing-instruction node )
),

( 
    map {
        my $axis_name = $_;

        sub {
            return skip "Need Test::Differences", 1 unless $has_test_diff;

            eq_or_diff t_lex "$axis_name ::", [
                [ AXIS_NAME   => $axis_name ],
                [ COLON_COLON => "::"       ],
            ];
        };
    } qw(
        ancestor
        ancestor-or-self
        attribute
        child
        descendant
        descendant-or-self
        following
        following-sibling
        namespace
        parent
        preceding
        preceding-sibling
        self
    )
),

##
## Non-disambiguation tokens
##
sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;
    eq_or_diff t_lex "abc", [
        [ QNAME => "abc" ],
    ];
},

sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;
    eq_or_diff t_lex "abc:def", [
        [ QNAME => "abc:def" ],
    ];
},

sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;
    eq_or_diff t_lex "abc:*", [
        [ NAME_COLON_STAR => "abc:*" ],
    ];
},

sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;
    eq_or_diff t_lex "'foo\"'", [
        [ LITERAL => "'foo\"'" ],
    ];
},

sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;
    eq_or_diff t_lex '"foo\'"', [
        [ LITERAL => '"foo\'"' ],
    ];
},

(
    map {
        my $number = $_;
        sub {
            return skip "Need Test::Differences", 1 unless $has_test_diff;
            eq_or_diff t_lex $number, [
                [ NUMBER => $number ],
            ];
        },
    } 0..20, qw ( 1.1 1000.1000 .100 )
),

sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;
    eq_or_diff t_lex '$foo', [
        [ DOLLAR_QNAME => '$foo' ],
    ];
},

sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;
    eq_or_diff t_lex '$foo:bar', [
        [ DOLLAR_QNAME => '$foo:bar' ],
    ];
},

(
    map {
        my ( $token, $val ) = /([A-Z_]+)(.*)/;
        sub {
            return skip "Need Test::Differences", 1 unless $has_test_diff;
            eq_or_diff t_lex $val, [
                [ $token => $val ],
            ];
        },
    } qw(
        DOT.
        DOT_DOT..
        AT@
        STAR*
        LPAR(
        RPAR)
        LSQB[
        RSQB]
        COLON_COLON::
        SLASH/
        SLASH_SLASH//
        VBAR|
        PLUS+
        MINUS-
        EQUALS=
        GT>
        LT<
        GTE>=
        LTE<=
    ),
    "COMMA,"
),

##
## Now a very few "real" cases
##
sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;
    eq_or_diff t_lex "   /   a   /   b   ", [
        [ SLASH => "/" ],
        [ QNAME => "a" ],
        [ SLASH => "/" ],
        [ QNAME => "b" ],
    ];
},

sub {
    return skip "Need Test::Differences", 1 unless $has_test_diff;
    eq_or_diff t_lex "   //   a   /   b   ", [
        [ SLASH_SLASH => "//" ],
        [ QNAME       => "a"  ],
        [ SLASH       => "/"  ],
        [ QNAME       => "b"  ],
    ];
},


);

plan tests => scalar @tests;

$_->() for @tests;

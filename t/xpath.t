#!/usr/local/lib/perl -w

use strict;

use Carp;
use Test;
use XML::Filter::Dispatcher qw( :all );
use XML::SAX::PurePerl;   ## Cannot use ParserFactory; LibXML 1.31 is broken.
use UNIVERSAL;

my $have_test_diff = eval "use Test::Differences; 1";

my $a = QB->new( "<a/>" );
my @nodes_in_a = ( "", "a" );

my $abcd    = QB->new( "<!--R--><?RR?><a>s<!--S--><?SS?><b>t<!--T--><?TT?><c>u<d id='1' name='n1'>v</d><d id='2'>w</d>x</c>y</b>z</a><!--Z1--><?Z1Z1?>" );

my $nodes_in_abcd = 25;  ## Including the doc node :)
my @nodes_in_abcd =          ( "", qw( R RR a s S SS b t T TT c u d name id v d id w x y z Z1 Z1Z1 ) );
my @non_attr_nodes_in_abcd = ( "", qw( R RR a s S SS b t T TT c u d         v d    w x y z Z1 Z1Z1 ) );
my @non_doc_non_attr_nodes_in_abcd =
                             (     qw( R RR a s S SS b t T TT c u d         v d    w x y z Z1 Z1Z1 ) );
my @end_nodes_in_abcd         = ( qw( d d c b a ), "" );
my @non_doc_end_nodes_in_abcd = ( qw( d d c b a )     );

my $abcdBcd = QB->new( "<a><b><c><d/><d/></c></b><B><c><d/><d/></c></B></a>" );

my $abc123  = QB->new( "<a>1<b>2<c id='10'>3</c><c id='20'>3</c>2</b>1</a>" );

my $var = QB->new( "<a><b/></a>" );
my @nodes_in_var = ( "", qw( a b ) );

sub result_list {
    my $prefix = "";
    $prefix = shift() . "_" unless ref $_[0];
    my $suffix = "";
    $suffix = "_" . pop unless ref $_[-1];

    return [ map "$prefix$_$suffix", @{$_[0]} ]
}

my @log;

my $fold_constants;

sub rule($) {
    my $rule = shift;
    return ( $rule => sub {
            my ( $self ) = shift;
            my ( $foo ) = @_;
            my $xr = xresult;
            push @log, join( "",
                ( $foo->{Name}
                    || ( $foo->{Target} || "" ) . ( $foo->{Data} || "" )
                ),
                defined $xr
                    ? ( "_", ref $xr ? $$xr : $xr )
                    : (),
            );
        }
    );
}

sub d {
    my $qb = shift;
    my $rule = shift;

    my $options = @_ && ref( $_[-1] ) eq "HASH" ? pop : {};

    $options->{FoldConstants} = $fold_constants;

    my $expect = result_list @_;

    unless ( $have_test_diff ) {
        @_ = ( "Need Test::Differences to test", 1 );
        goto &skip;
    }

    my $d = XML::Filter::Dispatcher->new(
        Rules => [ rule $rule ],
        Vars => {
            foo => [ boolean => "bar" ],
        },
        %$options,
    );

    @log = ();
    $qb->playback( $d );
    @_ = ( \@log, $expect, $rule );
    goto &eq_or_diff;

}


## NOTE: if you try this at home, it is *not* unsupported.
sub XFD::Function::oops { 
    my $code = "Carp::confess( 'operator not shorted!' )";
    return bless \$code, "boolean";
}


## Laid out for wide terminals, sorry.  This code is too tabular to do otherwise

my @tests = (
## Numbers and string literals
sub { d $a,     '0',                                               []                                },
## Note: we don't do '-0' in Perl...
sub { d $a,     '-0',                                              [],                               },
sub { d $a,    '10',                                               \@nodes_in_a,         '10'        },
sub { d $a,    '-10',                                              \@nodes_in_a,         '-10'       },
sub { d $a,    '""',                                               [],                               },
sub { d $a,    '"string"',                                         \@nodes_in_a,         'string'    },

## Functions

sub { d $a,    'concat(boolean(0),"P")',                           \@nodes_in_a,         'falseP'    },
sub { d $a,    'concat(boolean(false()),"P")',                     \@nodes_in_a,         'falseP'    },
sub { d $a,    'concat(boolean(""),"P")',                          \@nodes_in_a,         'falseP'    },
sub { d $a,    'boolean(1)',                                       \@nodes_in_a,         'true'      },
sub { d $a,    'boolean(true())',                                  \@nodes_in_a,         'true'      },
sub { d $a,    'boolean("0")',                                     \@nodes_in_a,         'true'      },
sub { d $a,    'boolean("false")',                                 \@nodes_in_a,         'true'      },

sub { d $a,    'concat(ceiling(0),"P")',                           \@nodes_in_a,         '0P'       },
sub { d $a,    'concat(ceiling(0.5),"P")',                         \@nodes_in_a,         '1P'       },
sub { d $a,    'concat(ceiling(0.999),"P")',                       \@nodes_in_a,         '1P'       },
sub { d $a,    'concat(ceiling(-0.999),"P")',                      \@nodes_in_a,         '0P'       },

sub { d $a,    'concat("a","b","c","d")',                          \@nodes_in_a,         'abcd'      },
sub { d $a,    'concat(1,2.3)',                                    \@nodes_in_a,         '12.3'      },
sub { d $a,    'concat(true(),false())',                           \@nodes_in_a,         'truefalse' },

sub { d $a,    'contains("ab","a")',                               \@nodes_in_a,         'true'      },
sub { d $a,    'contains("ab","b")',                               \@nodes_in_a,         'true'      },

# tested below as a predicate
#sub { d $abcd, 'is-end-event()',                                      \@end_nodes_in_abcd,  'true'      },

sub { d $a,    'string(false())',                                  \@nodes_in_a,         'false'     },

sub { d $a,    'concat(floor(0),"P")',                             \@nodes_in_a,         '0P'        },
sub { d $a,    'concat(floor(0.5),"P")',                           \@nodes_in_a,         '0P'        },
sub { d $a,    'concat(floor(0.999),"P")',                         \@nodes_in_a,         '0P'        },
sub { d $a,    'concat(floor(-0.999),"P")',                        \@nodes_in_a,         '-1P'       },

sub { d $a,    "normalize-space(' \t\r\na \t\r\nb \t\r\n')",       \@nodes_in_a,         'a b'       },

sub { d $a,    'not(0)',                                           \@nodes_in_a,         'true'      },
sub { d $a,    'concat(not(1),"P")',                               \@nodes_in_a,         'falseP'    },

sub { d $a,    'not(0)',                                           \@nodes_in_a,         'true'      },

sub { d $a,    'number(1)',                                        \@nodes_in_a,         '1'         },
sub { d $a,    'number(true())',                                   \@nodes_in_a,         '1'         },
sub { d $a,    'number(" 1 ")',                                    \@nodes_in_a,         '1'         },

sub { d $a,    'concat(round(0),"P")',                             \@nodes_in_a,         '0P'        },
sub { d $a,    'concat(round(0.5),"P")',                           \@nodes_in_a,         '1P'        },
sub { d $a,    'concat(round(0.999),"P")',                         \@nodes_in_a,         '1P'        },
sub { d $a,    'concat(round(-0.999),"P")',                        \@nodes_in_a,         '-1P'       },

sub { d $a,    "normalize-space(' \t\r\na \t\r\nb \t\r\n')",       \@nodes_in_a,         'a b'       },
sub { d $a,    'true()',                                           \@nodes_in_a,         'true'      },

sub { d $a,    'starts-with("ab","a")',                            \@nodes_in_a,         'true'      },
sub { d $a,    'concat(starts-with("ab","b"),1)',                  \@nodes_in_a,         'false1'    },

# tested below as a predicate
#sub { d $abcd, 'is-start-event()',                                    \@nodes_in_abcd,      'true'      },

sub { d $a,    'string("a")',                                      \@nodes_in_a,         'a'         },
sub { d $a,    'string(true())',                                   \@nodes_in_a,         'true'      },
sub { d $a,    'string(01)',                                       \@nodes_in_a,         '1'         },

sub { d $a,    'string-length("ab")',                              \@nodes_in_a,         '2'         },

sub { d $a,    'substring("ab",0)',                                \@nodes_in_a,         'ab'        },
sub { d $a,    'substring("ab",1)',                                \@nodes_in_a,         'ab'        },
sub { d $a,    'substring("ab",2)',                                \@nodes_in_a,         'b'         },
sub { d $a,    'concat(substring("ab",3),1)',                      \@nodes_in_a,         '1'         },
sub { d $a,    'substring("12345",2,3)',                           \@nodes_in_a,         '234'       },
sub { d $a,    'substring("12345",2)',                             \@nodes_in_a,         '2345'      },
sub { d $a,    'substring("12345",1.5,2.6)',                       \@nodes_in_a,         '234'       },
sub { d $a,    'substring("12345",0,3)',                           \@nodes_in_a,         '12'        },
# Waiting for div:
#sub { d $a,    'concat(substring("12345",0 div 0,3),"P")',         \@nodes_in_a,         'P'         },
#sub { d $a,    'concat(substring("12345",1,0 div 0),"P")',         \@nodes_in_a,         'P'         },
#sub { d $a,    'substring("12345",-42,1 div 0)',                   \@nodes_in_a,         '12345'     },
#sub { d $a,    'concat(substring("12345",-1 div 0,1 div 0),"P")',  \@nodes_in_a,         'P'         },

sub { d $a,    'substring-after("ab","a")',                        \@nodes_in_a,         'b'         },
sub { d $a,    'concat(substring-after("ab","b"),1)',              \@nodes_in_a,         '1'         },
sub { d $a,    'concat(substring-after("ab","c"),1)',              \@nodes_in_a,         '1'         },
sub { d $a,    'concat(substring-after("ab",""),1)',               \@nodes_in_a,         'ab1'       },
sub { d $a,    'substring-after("1999/04/01","19")',               \@nodes_in_a,         '99/04/01'  },

sub { d $a,    'substring-before("ab","b")',                       \@nodes_in_a,         'a'         },
sub { d $a,    'substring-before("1999/04/01","/")',               \@nodes_in_a,         '1999'      },
sub { d $a,    'concat(substring-before("ab","a"),1)',             \@nodes_in_a,         '1'         },
sub { d $a,    'concat(substring-before("ab","c"),1)',             \@nodes_in_a,         '1'         },
sub { d $a,    'concat(substring-before("ab",""),1)',              \@nodes_in_a,         '1'         },

sub { d $a,    'translate("bar","abc","ABC")',                     \@nodes_in_a,         'BAr'       },
sub { d $a,    'translate("--aaa--","abc-","ABC")',                \@nodes_in_a,         'AAA'       },

## Operators (other than union)
sub { d $a,    'concat( 0 or 0, "P" )',                            \@nodes_in_a,         'falseP'    },
sub { d $a,    '0 or 1',                                           \@nodes_in_a,         'true'      },
sub { d $a,    '1 or 0',                                           \@nodes_in_a,         'true'      },
sub { d $a,    '1 or 1',                                           \@nodes_in_a,         'true'      },
sub { d $a,    '1 or oops()',                                      \@nodes_in_a,         'true'      },

sub { d $a,    'concat( 0 and 0, "P" )',                           \@nodes_in_a,         'falseP'    },
sub { d $a,    'concat( 0 and 1, "P" )',                           \@nodes_in_a,         'falseP'    },
sub { d $a,    'concat( 1 and 0, "P" )',                           \@nodes_in_a,         'falseP'    },
sub { d $a,    '1 and 1',                                          \@nodes_in_a,         'true'      },
sub { d $a,    'concat( 0 and oops(), "P" )',                      \@nodes_in_a,         'falseP'    },

sub { d $a,    '0 and 1 or 1',                                     \@nodes_in_a,         'true'      },
sub { d $a,    '1 or 1 and 0',                                     \@nodes_in_a,         'true'      },

sub { d $a,    'concat( true() = false(), "P" )',                  \@nodes_in_a,         'falseP'    },
sub { d $a,    'true() = true()',                                  \@nodes_in_a,         'true'      },
sub { d $a,    '1 = 1',                                            \@nodes_in_a,         'true'      },
sub { d $a,    '"a" = "a"',                                        \@nodes_in_a,         'true'      },
sub { d $a,    '1 = " 1 "',                                        \@nodes_in_a,         'true'      },
sub { d $a,    'true() = 1',                                       \@nodes_in_a,         'true'      },
sub { d $a,    'false() = 0',                                      \@nodes_in_a,         'true'      },
sub { d $a,    'true() = "a"',                                     \@nodes_in_a,         'true'      },
sub { d $a,    'false() = ""',                                     \@nodes_in_a,         'true'      },

sub { d $a,    'concat( true() != true(), "P" )',                  \@nodes_in_a,         'falseP'    },
sub { d $a,    'true() != false()',                                \@nodes_in_a,         'true'      },
sub { d $a,    '1 != 0',                                           \@nodes_in_a,         'true'      },
sub { d $a,    '"a" != "b"',                                       \@nodes_in_a,         'true'      },
sub { d $a,    '1 != " 0 "',                                       \@nodes_in_a,         'true'      },
sub { d $a,    'true() != 0',                                      \@nodes_in_a,         'true'      },
sub { d $a,    'false() != 1',                                     \@nodes_in_a,         'true'      },
sub { d $a,    'true() != ""',                                     \@nodes_in_a,         'true'      },
sub { d $a,    'false() != "a"',                                   \@nodes_in_a,         'true'      },

sub { d $a,    'concat( true() < true(), "P" )',                   \@nodes_in_a,         'falseP'    },
sub { d $a,    'concat( true() < false(), "P" )',                  \@nodes_in_a,         'falseP'    },
sub { d $a,    'false() < true()',                                 \@nodes_in_a,         'true'      },
sub { d $a,    '0 < 1',                                            \@nodes_in_a,         'true'      },
sub { d $a,    '"a" < "b"',                                        \@nodes_in_a,         'true'      },
sub { d $a,    '0 < " 1 "',                                        \@nodes_in_a,         'true'      },

sub { d $a,    'true() <= true()',                                 \@nodes_in_a,         'true'      },
sub { d $a,    'concat( true() <= false(), "P" )',                 \@nodes_in_a,         'falseP'    },
sub { d $a,    'false() <= true()',                                \@nodes_in_a,         'true'      },
sub { d $a,    '0 <= 1',                                           \@nodes_in_a,         'true'      },
sub { d $a,    '"a" <= "b"',                                       \@nodes_in_a,         'true'      },
sub { d $a,    '0 <= " 1 "',                                       \@nodes_in_a,         'true'      },

sub { d $a,    'concat( true() > true(), "P" )',                   \@nodes_in_a,         'falseP'    },
sub { d $a,    'concat( false() > true(), "P" )',                  \@nodes_in_a,         'falseP'    },
sub { d $a,    'true() > false()',                                 \@nodes_in_a,         'true'      },
sub { d $a,    '1 > 0',                                            \@nodes_in_a,         'true'      },
sub { d $a,    '"b" > "a"',                                        \@nodes_in_a,         'true'      },
sub { d $a,    '1 > " 0 "',                                        \@nodes_in_a,         'true'      },
sub { d $a,    'concat( 3 > 2 > 1, "P" )',                         \@nodes_in_a,         'falseP'    },

sub { d $a,    'true() >= true()',                                 \@nodes_in_a,         'true'      },
sub { d $a,    'concat( false() >= true(), "P" )',                 \@nodes_in_a,         'falseP'    },
sub { d $a,    'true() >= false()',                                \@nodes_in_a,         'true'      },
sub { d $a,    '1 >= 0',                                           \@nodes_in_a,         'true'      },
sub { d $a,    '"b" >= "a"',                                       \@nodes_in_a,         'true'      },
sub { d $a,    '1 >= " 0 "',                                       \@nodes_in_a,         'true'      },

sub { d $a,    '4 + 1',                                            \@nodes_in_a,         '5'         },
sub { d $a,    '4 - 1',                                            \@nodes_in_a,         '3'         },
sub { d $a,    '4 * 1',                                            \@nodes_in_a,         '4'         },
sub { d $a,    '4 div 2',                                          \@nodes_in_a,         '2'         },
sub { d $a,    '5 mod 2',                                          \@nodes_in_a,         '1'         },

sub { d $a,    '( 1 )',                                            \@nodes_in_a,         '1'         },
sub { d $a,    '- ( 1 )',                                          \@nodes_in_a,         '-1'        },

##
## Location paths
##
sub { d $abcd, '/',                                                [ '' ]                            },
sub { d $abcd, '/.',                                               [ '' ]                            },
sub { d $abcd, '/child::a',                                        [ 'a' ]                           },
sub { d $abcd, '/a',                                               [ 'a' ]                           },
sub { d $abcd, 'a',                                                [ 'a' ]                           },
sub { d $abcd, './a',                                              [ 'a' ]                           },
sub { d $abcd, '.',                                                \@nodes_in_abcd                   },
sub { d $abcd, 'b',                                                [ 'b' ]                           },
sub { d $abcd, './b',                                              [ 'b' ]                           },
sub { d $abcd, '//b',                                              [ 'b' ]                           },
sub { d $abcd, './/b',                                             [ 'b' ]                           },
sub { d $abcd, '/a/b/c',                                           [ 'c' ]                           },
sub { d $abcd, '/a/b/c/d',                                         [ 'd', 'd' ]                      },
sub { d $abcd, '/child::*',                                        [ 'a' ]                           },
sub { d $abcd, '/*/child::*',                                      [ 'b' ]                           },

##
## //descendant-or-self::node()
##
sub { d $abcd, '/descendant-or-self::node()',                      \@non_attr_nodes_in_abcd          },

sub { d $abcd, '/descendant-or-self::node()/node()',               [ @non_attr_nodes_in_abcd[ 1..$#non_attr_nodes_in_abcd ] ] },
sub { d $abcd, '//node()',                                         [ @non_attr_nodes_in_abcd[ 1..$#non_attr_nodes_in_abcd ] ] },
sub { d $abcd, '/descendant-or-self::node()/a',                    [ 'a' ]                           },
sub { d $abcd, '//a',                                              [ 'a' ]                           },
sub { d $abcd, '/descendant-or-self::node()/b',                    [ 'b' ]                           },
sub { d $abcd, '//b',                                              [ 'b' ]                           },
sub { d $abcd, '/descendant-or-self::node()/d',                    [ 'd', 'd' ]                      },
sub { d $abcd, '//d',                                              [ 'd', 'd' ]                      },
sub { d $abcdBcd, '/a/B//d',                                       [ 'd', 'd' ]                      },

## TODO: fix grammar to like ////
#sub { d $abcd, '////node()',                                                                 [ @non_attr_nodes_in_abcd[ 1..$#non_attr_nodes_in_abcd ] ] },
sub { d $abcd, '/descendant-or-self::node()/descendant-or-self::node()/node()',              [ @non_attr_nodes_in_abcd[ 1..$#non_attr_nodes_in_abcd ] ] },

sub { d $abcd, '/self::node()',                                    [ '' ]                            },
sub { d $abcd, '/self::node()/a',                                  [ 'a' ]                           },
sub { d $abcd, '/./a',                                             [ 'a' ]                           },
sub { d $abcd, '//./a',                                            [ 'a' ]                           },
sub { d $abcd, '//./d',                                            [ 'd', 'd' ]                      },

sub { d $abcd, '//attribute::id',                                  [ 'id', 'id' ]                    },
sub { d $abcd, '//@id',                                            [ 'id', 'id' ]                    },
sub { d $abcd, '//@id',                                            [ 'id', 'id' ]                    },
sub { d $abcd, '//attribute::*',                                   [ 'name', 'id', 'id' ]            },
sub { d $abcd, '//@*',                                             [ 'name', 'id', 'id' ]            },

## Node tests (other than node())
sub { d $abcd, '//text()',                                         [qw( s t u v w x y z )]           },
sub { d $abcd, '//comment()',                                      [qw( R S T Z1 )]                  },
sub { d $abcd, '//processing-instruction()',                       [qw( RR SS TT Z1Z1 )]             },

## Union: |
sub { d $abcd, '//a|//b',                                          [ 'a', 'b' ]                      },
sub { d $abcd, '//a|//a',                                          [ 'a' ]                           },
sub { d $abcd, '//a|//a|//a',                                      [ 'a' ]                           },

## Predicates
sub { d $a,    'a[1]',                                             [ 'a' ]                           },
sub { d $a,    'a[0]',                                             []                                },
sub { d $abcd, 'node()[is-start-event()]',                         \@non_doc_non_attr_nodes_in_abcd  },
sub { d $abcd, 'node()[is-end-event()]',                           \@non_doc_end_nodes_in_abcd       },
sub { d $abcd, './self::node()[is-start-event()]',                 \@nodes_in_abcd                   },
sub { d $abcd, './self::node()[is-end-event()]',                   \@end_nodes_in_abcd               },
sub { d $a,    'a[is-start-event() or is-end-event()]',            [ 'a', 'a' ]                      },
sub { d $abcd, 'd[@id]',                                           [ 'd', 'd' ],                     },
sub { d $abcd, 'd[@id=1]',                                         [ 'd' ],                          },

## Functions that take node sets (and thus require precursors)
sub { d $abcd, 'string(/a)',                                       [ '' ],                'stuvwxyz' },
sub { d $abcd, 'string(/a/b)',                                     [ '' ],                'tuvwxy'   },
sub { d $abcd, 'string(/a/b/c/d)',                                 [ '_v', '_w' ],                   },
sub { d $abcd, 'string(d)',                                        [ 'c_v', 'c_w' ],                 },
sub { d $abcd, 'string(@id)',                                      [ 'd_1', 'd_2' ],                 },
sub { d $abcd, 'concat( @id, "")',                                 [ 'd_1', 'd_2' ],                 },

## boolean(/path) is useless, but it's legal.
sub { d $abcd, 'boolean(/a)',                                      [ '' ],                'true'     },
sub { d $abcd, 'boolean(/a/b)',                                    [ '' ],                'true'     },
sub { d $abcd, 'boolean(/a/b/c/d)',                                [ '', '' ],            'true'     },
sub { d $abcd, 'boolean(d)',                                       [ 'c', 'c' ],          'true'     },
sub { d $abcd, 'boolean(@id)',                                     [ 'd', 'd' ],          'true'     },
sub { d $abcd, 'not(d)',                                           [],                               },

sub { d $abc123, 'number(/a)',                                     [ '' ],                '123321'   },
sub { d $abc123, 'number(/a/b)',                                   [ '' ],                '2332'     },
sub { d $abc123, 'number(c)',                                      [ 'b', 'b' ],          '3'        },
sub { d $abc123, 'number(@id)',                                    [ 'c_10', 'c_20' ],               },
sub { d $abc123, '- @id',                                          [ 'c_-10', 'c_-20' ],             },

## Multiple precursors
sub { d $abcd, 'concat( @id, @id )',                               [ 'd_11', 'd_22' ],               },
## TODO??? sub { d $abcd, 'concat( @id, ":", @name )',                        [ 'id_1:n1' ],                    },
sub { d $abcd, 'string(a | b)',                                    [ 'a_tuvwxy', '_stuvwxyz' ]       },
sub { d $abcd, 'string( ./a or ./b )',                             [ 'a_true', '_true' ]             },
sub { d $abcd, 'concat( string(a), ":", string(a) )',              [ '_stuvwxyz:stuvwxyz' ],         },
sub { d $abcd, 'concat( string(a), ":", string(@id) )',            [ '_stuvwxyz:' ],                 },

## Variable references
sub { d $var,   'concat( $foo, "!" )',                           \@nodes_in_var,        'true!'      },

);

plan tests => 2 * @tests;

for ( @tests ) {
    $fold_constants = 0;
    $_->();
    $fold_constants = 1;
    $_->();
}

## This quick little buffering filter is used to save us the overhead
## of a parse for each test.  This saves me sanity (since I run the test
## suite a lot), allows me to see which tests are noticably slower in
## case something pathalogical happens, and keeps admins from getting the
## impression that this is a slow package based on test suite speed.
package QB;
use vars qw( $AUTOLOAD );

sub new {
    my $self = bless [], shift;
    my $p = XML::SAX::PurePerl->new( Handler => $self );
    $p->parse_string( shift );
    return $self;
}

sub DESTROY;

sub AUTOLOAD {
    my $self = shift;
    $AUTOLOAD =~ s/.*://;
    if ( $AUTOLOAD eq "start_element" ) {
        ## Older (and mebbe newer :) X::S::PurePerls reuse the same
        ## hash in end_element but delete the Attributes, so we need
        ## to copy.  And I can't copy everything because some other
        ## overly magical thing dies, haven't tracked down beyond seeing
        ## signs that it's XML::SAX::DocumentLocator::NEXTKEY(/usr/local/lib/perl5/site_perl/5.6.1/XML/SAX/DocumentLocator.pm:72)
        ## but I hear that's fixed in CVS :).
        push @$self, [ $AUTOLOAD, [ { %{$_[0]} } ] ];
    }
    else {
        push @$self, [ $AUTOLOAD, [ $_[0] ] ];
    }
}

sub playback {
    my $self = shift;
    my $h = shift;
    for ( @$self ) {
        my $m = $_->[0];
        no strict "refs";
        $h->$m( @{$_->[1]} );
    }
}

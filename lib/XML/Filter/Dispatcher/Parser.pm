####################################################################
#
#    This file was generated using Parse::Yapp version 1.02.
#
#        Don't edit this file, use source file instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
####################################################################
package XML::Filter::Dispatcher::Parser;
use vars qw ( @ISA );
use strict;

@ISA= qw ( Parse::Yapp::Driver );
#Included Parse/Yapp/Driver.pm file----------------------------------------
{
#
# Module Parse::Yapp::Driver
#
# This module is part of the Parse::Yapp package available on your
# nearest CPAN
#
# Any use of this module in a standalone parser make the included
# text under the same copyright as the Parse::Yapp module itself.
#
# This notice should remain unchanged.
#
# (c) Copyright 1998-1999 Francois Desarmenien, all rights reserved.
# (see the pod text in Parse::Yapp module for use and distribution rights)
#

package Parse::Yapp::Driver;

require 5.004;

use strict;

use vars qw ( $VERSION $COMPATIBLE $FILENAME );

$VERSION = '1.02';
$COMPATIBLE = '0.07';
$FILENAME=__FILE__;

use Carp;

#Known parameters, all starting with YY (leading YY will be discarded)
my(%params)=(YYLEX => 'CODE', 'YYERROR' => 'CODE', YYVERSION => '',
			 YYRULES => 'ARRAY', YYSTATES => 'ARRAY', YYDEBUG => '');
#Mandatory parameters
my(@params)=('LEX','RULES','STATES');

sub new {
    my($class)=shift;
	my($errst,$nberr,$token,$value,$check,$dotpos);
    my($self)={ ERROR => \&_Error,
				ERRST => \$errst,
                NBERR => \$nberr,
				TOKEN => \$token,
				VALUE => \$value,
				DOTPOS => \$dotpos,
				STACK => [],
				DEBUG => 0,
				CHECK => \$check };

	_CheckParams( [], \%params, \@_, $self );

		exists($$self{VERSION})
	and	$$self{VERSION} < $COMPATIBLE
	and	croak "Yapp driver version $VERSION ".
			  "incompatible with version $$self{VERSION}:\n".
			  "Please recompile parser module.";

        ref($class)
    and $class=ref($class);

    bless($self,$class);
}

sub YYParse {
    my($self)=shift;
    my($retval);

	_CheckParams( \@params, \%params, \@_, $self );

	if($$self{DEBUG}) {
		_DBLoad();
		$retval = eval '$self->_DBParse()';#Do not create stab entry on compile
        $@ and die $@;
	}
	else {
		$retval = $self->_Parse();
	}
    $retval
}

sub YYData {
	my($self)=shift;

		exists($$self{USER})
	or	$$self{USER}={};

	$$self{USER};
	
}

sub YYErrok {
	my($self)=shift;

	${$$self{ERRST}}=0;
    undef;
}

sub YYNberr {
	my($self)=shift;

	${$$self{NBERR}};
}

sub YYRecovering {
	my($self)=shift;

	${$$self{ERRST}} != 0;
}

sub YYAbort {
	my($self)=shift;

	${$$self{CHECK}}='ABORT';
    undef;
}

sub YYAccept {
	my($self)=shift;

	${$$self{CHECK}}='ACCEPT';
    undef;
}

sub YYError {
	my($self)=shift;

	${$$self{CHECK}}='ERROR';
    undef;
}

sub YYSemval {
	my($self)=shift;
	my($index)= $_[0] - ${$$self{DOTPOS}} - 1;

		$index < 0
	and	-$index <= @{$$self{STACK}}
	and	return $$self{STACK}[$index][1];

	undef;	#Invalid index
}

sub YYCurtok {
	my($self)=shift;

        @_
    and ${$$self{TOKEN}}=$_[0];
    ${$$self{TOKEN}};
}

sub YYCurval {
	my($self)=shift;

        @_
    and ${$$self{VALUE}}=$_[0];
    ${$$self{VALUE}};
}

sub YYExpect {
    my($self)=shift;

    keys %{$self->{STATES}[$self->{STACK}[-1][0]]{ACTIONS}}
}

sub YYLexer {
    my($self)=shift;

	$$self{LEX};
}


#################
# Private stuff #
#################


sub _CheckParams {
	my($mandatory,$checklist,$inarray,$outhash)=@_;
	my($prm,$value);
	my($prmlst)={};

	while(($prm,$value)=splice(@$inarray,0,2)) {
        $prm=uc($prm);
			exists($$checklist{$prm})
		or	croak("Unknow parameter '$prm'");
			ref($value) eq $$checklist{$prm}
		or	croak("Invalid value for parameter '$prm'");
        $prm=unpack('@2A*',$prm);
		$$outhash{$prm}=$value;
	}
	for (@$mandatory) {
			exists($$outhash{$_})
		or	croak("Missing mandatory parameter '".lc($_)."'");
	}
}

sub _Error {
	print "Parse error.\n";
}

sub _DBLoad {
	{
		no strict 'refs';

			exists(${__PACKAGE__.'::'}{_DBParse})#Already loaded ?
		and	return;
	}
	my($fname)=__FILE__;
	my(@drv);
	open(DRV,"<$fname") or die "Report this as a BUG: Cannot open $fname";
	while(<DRV>) {
                	/^\s*sub\s+_Parse\s*{\s*$/ .. /^\s*}\s*#\s*_Parse\s*$/
        	and     do {
                	s/^#DBG>//;
                	push(@drv,$_);
        	}
	}
	close(DRV);

	$drv[0]=~s/_P/_DBP/;
	eval join('',@drv);
}

#Note that for loading debugging version of the driver,
#this file will be parsed from 'sub _Parse' up to '}#_Parse' inclusive.
#So, DO NOT remove comment at end of sub !!!
sub _Parse {
    my($self)=shift;

	my($rules,$states,$lex,$error)
     = @$self{ 'RULES', 'STATES', 'LEX', 'ERROR' };
	my($errstatus,$nberror,$token,$value,$stack,$check,$dotpos)
     = @$self{ 'ERRST', 'NBERR', 'TOKEN', 'VALUE', 'STACK', 'CHECK', 'DOTPOS' };

#DBG>	my($debug)=$$self{DEBUG};
#DBG>	my($dbgerror)=0;

#DBG>	my($ShowCurToken) = sub {
#DBG>		my($tok)='>';
#DBG>		for (split('',$$token)) {
#DBG>			$tok.=		(ord($_) < 32 or ord($_) > 126)
#DBG>					?	sprintf('<%02X>',ord($_))
#DBG>					:	$_;
#DBG>		}
#DBG>		$tok.='<';
#DBG>	};

	$$errstatus=0;
	$$nberror=0;
	($$token,$$value)=(undef,undef);
	@$stack=( [ 0, undef ] );
	$$check='';

    while(1) {
        my($actions,$act,$stateno);

        $stateno=$$stack[-1][0];
        $actions=$$states[$stateno];

#DBG>	print STDERR ('-' x 40),"\n";
#DBG>		$debug & 0x2
#DBG>	and	print STDERR "In state $stateno:\n";
#DBG>		$debug & 0x08
#DBG>	and	print STDERR "Stack:[".
#DBG>					 join(',',map { $$_[0] } @$stack).
#DBG>					 "]\n";


        if  (exists($$actions{ACTIONS})) {

				defined($$token)
            or	do {
				($$token,$$value)=&$lex($self);
#DBG>				$debug & 0x01
#DBG>			and	print STDERR "Need token. Got ".&$ShowCurToken."\n";
			};

            $act=   exists($$actions{ACTIONS}{$$token})
                    ?   $$actions{ACTIONS}{$$token}
                    :   exists($$actions{DEFAULT})
                        ?   $$actions{DEFAULT}
                        :   undef;
        }
        else {
            $act=$$actions{DEFAULT};
#DBG>			$debug & 0x01
#DBG>		and	print STDERR "Don't need token.\n";
        }

            defined($act)
        and do {

                $act > 0
            and do {        #shift

#DBG>				$debug & 0x04
#DBG>			and	print STDERR "Shift and go to state $act.\n";

					$$errstatus
				and	do {
					--$$errstatus;

#DBG>					$debug & 0x10
#DBG>				and	$dbgerror
#DBG>				and	$$errstatus == 0
#DBG>				and	do {
#DBG>					print STDERR "**End of Error recovery.\n";
#DBG>					$dbgerror=0;
#DBG>				};
				};


                push(@$stack,[ $act, $$value ]);

					$$token ne ''	#Don't eat the eof
				and	$$token=$$value=undef;
                next;
            };

            #reduce
            my($lhs,$len,$code,@sempar,$semval);
            ($lhs,$len,$code)=@{$$rules[-$act]};

#DBG>			$debug & 0x04
#DBG>		and	$act
#DBG>		and	print STDERR "Reduce using rule ".-$act." ($lhs,$len): ";

                $act
            or  $self->YYAccept();

            $$dotpos=$len;

                unpack('A1',$lhs) eq '@'    #In line rule
            and do {
                    $lhs =~ /^\@[0-9]+\-([0-9]+)$/
                or  die "In line rule name '$lhs' ill formed: ".
                        "report it as a BUG.\n";
                $$dotpos = $1;
            };

            @sempar =       $$dotpos
                        ?   map { $$_[1] } @$stack[ -$$dotpos .. -1 ]
                        :   ();

            $semval = $code ? &$code( $self, @sempar )
                            : @sempar ? $sempar[0] : undef;

            splice(@$stack,-$len,$len);

                $$check eq 'ACCEPT'
            and do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Accept.\n";

				return($semval);
			};

                $$check eq 'ABORT'
            and	do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Abort.\n";

				return(undef);

			};

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Back to state $$stack[-1][0], then ";

                $$check eq 'ERROR'
            or  do {
#DBG>				$debug & 0x04
#DBG>			and	print STDERR 
#DBG>				    "go to state $$states[$$stack[-1][0]]{GOTOS}{$lhs}.\n";

#DBG>				$debug & 0x10
#DBG>			and	$dbgerror
#DBG>			and	$$errstatus == 0
#DBG>			and	do {
#DBG>				print STDERR "**End of Error recovery.\n";
#DBG>				$dbgerror=0;
#DBG>			};

			    push(@$stack,
                     [ $$states[$$stack[-1][0]]{GOTOS}{$lhs}, $semval ]);
                $$check='';
                next;
            };

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Forced Error recovery.\n";

            $$check='';

        };

        #Error
            $$errstatus
        or   do {

            $$errstatus = 1;
            &$error($self);
                $$errstatus # if 0, then YYErrok has been called
            or  next;       # so continue parsing

#DBG>			$debug & 0x10
#DBG>		and	do {
#DBG>			print STDERR "**Entering Error recovery.\n";
#DBG>			++$dbgerror;
#DBG>		};

            ++$$nberror;

        };

			$$errstatus == 3	#The next token is not valid: discard it
		and	do {
				$$token eq ''	# End of input: no hope
			and	do {
#DBG>				$debug & 0x10
#DBG>			and	print STDERR "**At eof: aborting.\n";
				return(undef);
			};

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Dicard invalid token ".&$ShowCurToken.".\n";

			$$token=$$value=undef;
		};

        $$errstatus=3;

		while(	  @$stack
			  and (		not exists($$states[$$stack[-1][0]]{ACTIONS})
			        or  not exists($$states[$$stack[-1][0]]{ACTIONS}{error})
					or	$$states[$$stack[-1][0]]{ACTIONS}{error} <= 0)) {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Pop state $$stack[-1][0].\n";

			pop(@$stack);
		}

			@$stack
		or	do {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**No state left on stack: aborting.\n";

			return(undef);
		};

		#shift the error token

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Shift \$error token and go to state ".
#DBG>						 $$states[$$stack[-1][0]]{ACTIONS}{error}.
#DBG>						 ".\n";

		push(@$stack, [ $$states[$$stack[-1][0]]{ACTIONS}{error}, undef ]);

    }

    #never reached
	croak("Error in driver logic. Please, report it as a BUG");

}#_Parse
#DO NOT remove comment

1;

}
#End of include--------------------------------------------------


#line 3 "xfdxpath.yp"

    use Carp;
    use UNIVERSAL;
    use XML::Filter::Dispatcher::Ops;

    sub _no {
        my $p = shift;
#        push @{$p->{USER}->{NONONO}}, join(
die join(
            "",
            "XPath construct not supported: ",
            join( " ", map
                defined $_
                    ? ref $_
                        ? do {
                            my $f = ref $_;
                            $f =~ s/^XFD:://;
                            $f;
                        }
                        : $_
                    : "<undef>" ,
                @_
            ),
            " (grammar rule at ",
            (caller)[1],
            ", line ",
            (caller)[2],
            ")"
        );

        return ();
    }

    sub _step {
        my ( $p, $axis, $node_test, @predicates ) = @_;

        shift @predicates if @predicates && ! defined $predicates[0];

        warn "axis not an op: '$axis'" if defined $axis && !ref $axis;
        warn "node test not an op: '$node_test'" if defined $node_test && !ref $node_test;

        my @ops;
        push @ops, $axis if defined $axis && ref $axis;
        push @ops, $node_test if $node_test && ref $node_test;
        push @ops, @predicates;

        for ( 0..$#ops-1 ) {
            $ops[$_]->set_next( $ops[$_+1] );
        }
            
        return $ops[0];
    }


sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    my($self)=$class->SUPER::new( yyversion => '1.02',
                                  yystates =>
[
	{#State 0
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'and_expr' => 9,
			'equality_expr' => 10,
			'additive_expr' => 12,
			'location_path' => 13,
			'step' => 15,
			'relational_expr' => 16,
			'function_call' => 17,
			'multiplicative_expr' => 18,
			'or_expr' => 19,
			'unary_expr' => 22,
			'expr' => 25,
			'path_expr' => 26,
			'absolute_location_path' => 29,
			'axis' => 28
		}
	},
	{#State 1
		ACTIONS => {
			'VBAR' => 30
		},
		DEFAULT => -25
	},
	{#State 2
		DEFAULT => -53
	},
	{#State 3
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -34
	},
	{#State 4
		DEFAULT => -48,
		GOTOS => {
			'predicates' => 33
		}
	},
	{#State 5
		DEFAULT => -50
	},
	{#State 6
		ACTIONS => {
			'COLON_COLON' => 34
		}
	},
	{#State 7
		DEFAULT => -44
	},
	{#State 8
		DEFAULT => -43
	},
	{#State 9
		ACTIONS => {
			'AND' => 35,
			'AMP' => 36,
			'AMP_AMP' => 37
		},
		DEFAULT => -2
	},
	{#State 10
		ACTIONS => {
			'EQUALS' => 38,
			'BANG_EQUALS' => 40,
			'EQUALS_EQUALS' => 39
		},
		DEFAULT => -5
	},
	{#State 11
		ACTIONS => {
			'AXIS_NAME' => 6,
			'STAR' => -45,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'PI' => -45,
			'TEXT' => -45,
			'COMMENT' => -45,
			'NAME_COLON_STAR' => -45,
			'QNAME' => -45,
			'AT' => 24,
			'NODE' => -45
		},
		DEFAULT => -36,
		GOTOS => {
			'relative_location_path' => 41,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 12
		ACTIONS => {
			'PLUS' => 42,
			'MINUS' => 43
		},
		DEFAULT => -13
	},
	{#State 13
		DEFAULT => -29
	},
	{#State 14
		ACTIONS => {
			'LPAR' => 44
		}
	},
	{#State 15
		DEFAULT => -39
	},
	{#State 16
		ACTIONS => {
			'LT' => 47,
			'GT' => 45,
			'LTE' => 48,
			'GTE' => 46
		},
		DEFAULT => -9
	},
	{#State 17
		DEFAULT => -54
	},
	{#State 18
		ACTIONS => {
			'MULTIPLY' => 49,
			'MOD' => 50,
			'DIV' => 51
		},
		DEFAULT => -18
	},
	{#State 19
		ACTIONS => {
			'VBAR_VBAR' => 52,
			'OR' => 53
		},
		DEFAULT => -1
	},
	{#State 20
		DEFAULT => -52
	},
	{#State 21
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 24
		},
		DEFAULT => -45,
		GOTOS => {
			'relative_location_path' => 54,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 22
		DEFAULT => -21
	},
	{#State 23
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'function_call' => 17,
			'unary_expr' => 55,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 24
		DEFAULT => -47
	},
	{#State 25
		ACTIONS => {
			'' => 56
		}
	},
	{#State 26
		DEFAULT => -27
	},
	{#State 27
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'and_expr' => 9,
			'equality_expr' => 10,
			'additive_expr' => 12,
			'location_path' => 13,
			'step' => 15,
			'relational_expr' => 16,
			'function_call' => 17,
			'multiplicative_expr' => 18,
			'or_expr' => 19,
			'unary_expr' => 22,
			'path_expr' => 26,
			'expr' => 57,
			'absolute_location_path' => 29,
			'axis' => 28
		}
	},
	{#State 28
		ACTIONS => {
			'QNAME' => 64,
			'NODE' => 65,
			'COMMENT' => 62,
			'PI' => 61,
			'TEXT' => 60,
			'STAR' => 59,
			'NAME_COLON_STAR' => 63
		},
		GOTOS => {
			'node_test' => 58
		}
	},
	{#State 29
		DEFAULT => -35
	},
	{#State 30
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'relative_location_path' => 3,
			'path_expr' => 66,
			'primary_expr' => 4,
			'function_call' => 17,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 31
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 24
		},
		DEFAULT => -45,
		GOTOS => {
			'axis' => 28,
			'step' => 67
		}
	},
	{#State 32
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 24
		},
		DEFAULT => -45,
		GOTOS => {
			'axis' => 28,
			'step' => 68
		}
	},
	{#State 33
		ACTIONS => {
			'SLASH' => 69,
			'SLASH_SLASH' => 71,
			'LSQB' => 72
		},
		DEFAULT => -31,
		GOTOS => {
			'segment' => 70
		}
	},
	{#State 34
		DEFAULT => -46
	},
	{#State 35
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'equality_expr' => 73,
			'path_expr' => 26,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 36
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'equality_expr' => 74,
			'path_expr' => 26,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 37
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'equality_expr' => 75,
			'path_expr' => 26,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 38
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 76,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'path_expr' => 26,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 39
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 77,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'path_expr' => 26,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 40
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 78,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'path_expr' => 26,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 41
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -37
	},
	{#State 42
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 79,
			'function_call' => 17,
			'unary_expr' => 22,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 43
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 80,
			'function_call' => 17,
			'unary_expr' => 22,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 44
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'RPAR' => -56,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'and_expr' => 9,
			'args' => 81,
			'equality_expr' => 10,
			'additive_expr' => 12,
			'location_path' => 13,
			'step' => 15,
			'opt_args' => 82,
			'relational_expr' => 16,
			'function_call' => 17,
			'multiplicative_expr' => 18,
			'or_expr' => 19,
			'unary_expr' => 22,
			'expr' => 83,
			'path_expr' => 26,
			'absolute_location_path' => 29,
			'axis' => 28
		}
	},
	{#State 45
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'additive_expr' => 84,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 46
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'additive_expr' => 85,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 47
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'additive_expr' => 86,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 48
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'additive_expr' => 87,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 49
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'function_call' => 17,
			'unary_expr' => 88,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 50
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'function_call' => 17,
			'unary_expr' => 89,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 51
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'function_call' => 17,
			'unary_expr' => 90,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 52
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'and_expr' => 91,
			'unary_expr' => 22,
			'equality_expr' => 10,
			'path_expr' => 26,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 53
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'and_expr' => 92,
			'unary_expr' => 22,
			'equality_expr' => 10,
			'path_expr' => 26,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 54
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -38
	},
	{#State 55
		DEFAULT => -26
	},
	{#State 56
		DEFAULT => -0
	},
	{#State 57
		ACTIONS => {
			'RPAR' => 93
		}
	},
	{#State 58
		DEFAULT => -48,
		GOTOS => {
			'predicates' => 94
		}
	},
	{#State 59
		DEFAULT => -61
	},
	{#State 60
		ACTIONS => {
			'LPAR' => 95
		}
	},
	{#State 61
		ACTIONS => {
			'LPAR' => 96
		}
	},
	{#State 62
		ACTIONS => {
			'LPAR' => 97
		}
	},
	{#State 63
		DEFAULT => -62
	},
	{#State 64
		DEFAULT => -60
	},
	{#State 65
		ACTIONS => {
			'LPAR' => 98
		}
	},
	{#State 66
		DEFAULT => -28
	},
	{#State 67
		DEFAULT => -40
	},
	{#State 68
		DEFAULT => -41
	},
	{#State 69
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 24
		},
		DEFAULT => -45,
		GOTOS => {
			'relative_location_path' => 99,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 70
		DEFAULT => -30
	},
	{#State 71
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 24
		},
		DEFAULT => -45,
		GOTOS => {
			'relative_location_path' => 100,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 72
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'and_expr' => 9,
			'equality_expr' => 10,
			'additive_expr' => 12,
			'location_path' => 13,
			'step' => 15,
			'relational_expr' => 16,
			'function_call' => 17,
			'multiplicative_expr' => 18,
			'or_expr' => 19,
			'unary_expr' => 22,
			'path_expr' => 26,
			'expr' => 101,
			'absolute_location_path' => 29,
			'axis' => 28
		}
	},
	{#State 73
		ACTIONS => {
			'EQUALS' => 38,
			'BANG_EQUALS' => 40,
			'EQUALS_EQUALS' => 39
		},
		DEFAULT => -6
	},
	{#State 74
		ACTIONS => {
			'EQUALS' => 38,
			'BANG_EQUALS' => 40,
			'EQUALS_EQUALS' => 39
		},
		DEFAULT => -8
	},
	{#State 75
		ACTIONS => {
			'EQUALS' => 38,
			'BANG_EQUALS' => 40,
			'EQUALS_EQUALS' => 39
		},
		DEFAULT => -7
	},
	{#State 76
		ACTIONS => {
			'LT' => 47,
			'GT' => 45,
			'LTE' => 48,
			'GTE' => 46
		},
		DEFAULT => -10
	},
	{#State 77
		ACTIONS => {
			'LT' => 47,
			'GT' => 45,
			'LTE' => 48,
			'GTE' => 46
		},
		DEFAULT => -12
	},
	{#State 78
		ACTIONS => {
			'LT' => 47,
			'GT' => 45,
			'LTE' => 48,
			'GTE' => 46
		},
		DEFAULT => -11
	},
	{#State 79
		ACTIONS => {
			'MULTIPLY' => 49,
			'MOD' => 50,
			'DIV' => 51
		},
		DEFAULT => -19
	},
	{#State 80
		ACTIONS => {
			'MULTIPLY' => 49,
			'MOD' => 50,
			'DIV' => 51
		},
		DEFAULT => -20
	},
	{#State 81
		ACTIONS => {
			'COMMA' => 102
		},
		DEFAULT => -57
	},
	{#State 82
		ACTIONS => {
			'RPAR' => 103
		}
	},
	{#State 83
		DEFAULT => -58
	},
	{#State 84
		ACTIONS => {
			'PLUS' => 42,
			'MINUS' => 43
		},
		DEFAULT => -15
	},
	{#State 85
		ACTIONS => {
			'PLUS' => 42,
			'MINUS' => 43
		},
		DEFAULT => -17
	},
	{#State 86
		ACTIONS => {
			'PLUS' => 42,
			'MINUS' => 43
		},
		DEFAULT => -14
	},
	{#State 87
		ACTIONS => {
			'PLUS' => 42,
			'MINUS' => 43
		},
		DEFAULT => -16
	},
	{#State 88
		DEFAULT => -22
	},
	{#State 89
		DEFAULT => -24
	},
	{#State 90
		DEFAULT => -23
	},
	{#State 91
		ACTIONS => {
			'AND' => 35,
			'AMP' => 36,
			'AMP_AMP' => 37
		},
		DEFAULT => -4
	},
	{#State 92
		ACTIONS => {
			'AND' => 35,
			'AMP' => 36,
			'AMP_AMP' => 37
		},
		DEFAULT => -3
	},
	{#State 93
		DEFAULT => -51
	},
	{#State 94
		ACTIONS => {
			'LSQB' => 72
		},
		DEFAULT => -42
	},
	{#State 95
		ACTIONS => {
			'RPAR' => 104
		}
	},
	{#State 96
		ACTIONS => {
			'LITERAL' => 105
		},
		DEFAULT => -67,
		GOTOS => {
			'opt_literal' => 106
		}
	},
	{#State 97
		ACTIONS => {
			'RPAR' => 107
		}
	},
	{#State 98
		ACTIONS => {
			'RPAR' => 108
		}
	},
	{#State 99
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -32
	},
	{#State 100
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -33
	},
	{#State 101
		ACTIONS => {
			'RSQB' => 109
		}
	},
	{#State 102
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			'MINUS' => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'and_expr' => 9,
			'equality_expr' => 10,
			'additive_expr' => 12,
			'location_path' => 13,
			'step' => 15,
			'relational_expr' => 16,
			'function_call' => 17,
			'multiplicative_expr' => 18,
			'or_expr' => 19,
			'unary_expr' => 22,
			'path_expr' => 26,
			'expr' => 110,
			'absolute_location_path' => 29,
			'axis' => 28
		}
	},
	{#State 103
		DEFAULT => -55
	},
	{#State 104
		DEFAULT => -65
	},
	{#State 105
		DEFAULT => -68
	},
	{#State 106
		ACTIONS => {
			'RPAR' => 111
		}
	},
	{#State 107
		DEFAULT => -64
	},
	{#State 108
		DEFAULT => -66
	},
	{#State 109
		DEFAULT => -49
	},
	{#State 110
		DEFAULT => -59
	},
	{#State 111
		DEFAULT => -63
	}
],
                                  yyrules  =>
[
	[#Rule 0
		 '$start', 2, undef
	],
	[#Rule 1
		 'expr', 1, undef
	],
	[#Rule 2
		 'or_expr', 1, undef
	],
	[#Rule 3
		 'or_expr', 3,
sub
#line 111 "xfdxpath.yp"
{ XFD::or [ @_[1,3] ] }
	],
	[#Rule 4
		 'or_expr', 3,
sub
#line 112 "xfdxpath.yp"
{
        die "XPath uses 'or' instead of Perl's '||'\n";
    }
	],
	[#Rule 5
		 'and_expr', 1, undef
	],
	[#Rule 6
		 'and_expr', 3,
sub
#line 119 "xfdxpath.yp"
{ XFD::and [ @_[1,3] ] }
	],
	[#Rule 7
		 'and_expr', 3,
sub
#line 120 "xfdxpath.yp"
{
        die "XPath uses 'and' instead of Perl's '&&'\n";
    }
	],
	[#Rule 8
		 'and_expr', 3,
sub
#line 123 "xfdxpath.yp"
{
        die "XPath uses 'and' instead of Perl's '&'\n";
    }
	],
	[#Rule 9
		 'equality_expr', 1, undef
	],
	[#Rule 10
		 'equality_expr', 3,
sub
#line 130 "xfdxpath.yp"
{ XFD::equals [ @_[1,3] ] }
	],
	[#Rule 11
		 'equality_expr', 3,
sub
#line 131 "xfdxpath.yp"
{ XFD::not_equals [ @_[1,3] ] }
	],
	[#Rule 12
		 'equality_expr', 3,
sub
#line 132 "xfdxpath.yp"
{ 
        die "XPath uses '=' instead of Perl's '=='\n";
    }
	],
	[#Rule 13
		 'relational_expr', 1, undef
	],
	[#Rule 14
		 'relational_expr', 3,
sub
#line 139 "xfdxpath.yp"
{ XFD::lt  [ @_[1,3] ] }
	],
	[#Rule 15
		 'relational_expr', 3,
sub
#line 140 "xfdxpath.yp"
{ XFD::gt  [ @_[1,3] ] }
	],
	[#Rule 16
		 'relational_expr', 3,
sub
#line 141 "xfdxpath.yp"
{ XFD::lte [ @_[1,3] ] }
	],
	[#Rule 17
		 'relational_expr', 3,
sub
#line 142 "xfdxpath.yp"
{ XFD::gte [ @_[1,3] ] }
	],
	[#Rule 18
		 'additive_expr', 1, undef
	],
	[#Rule 19
		 'additive_expr', 3,
sub
#line 147 "xfdxpath.yp"
{ XFD::addition    [ @_[1,3] ] }
	],
	[#Rule 20
		 'additive_expr', 3,
sub
#line 148 "xfdxpath.yp"
{ XFD::subtraction [ @_[1,3] ] }
	],
	[#Rule 21
		 'multiplicative_expr', 1, undef
	],
	[#Rule 22
		 'multiplicative_expr', 3,
sub
#line 153 "xfdxpath.yp"
{ XFD::multiplication[ @_[1,3] ] }
	],
	[#Rule 23
		 'multiplicative_expr', 3,
sub
#line 154 "xfdxpath.yp"
{ XFD::division      [ @_[1,3] ] }
	],
	[#Rule 24
		 'multiplicative_expr', 3,
sub
#line 155 "xfdxpath.yp"
{ XFD::modulus       [ @_[1,3] ] }
	],
	[#Rule 25
		 'unary_expr', 1, undef
	],
	[#Rule 26
		 'unary_expr', 2,
sub
#line 160 "xfdxpath.yp"
{ XFD::negation $_[2] }
	],
	[#Rule 27
		 'union_expr', 1, undef
	],
	[#Rule 28
		 'union_expr', 3,
sub
#line 165 "xfdxpath.yp"
{
        my $union;
        if ( $_[1]->isa( "XFD::union" ) ) {
            $_[1]->add( $_[3] );
            $union = $_[1];
        }
        else {
            $union = XFD::union->new( @_[1,3] )
        }
        $union;
    }
	],
	[#Rule 29
		 'path_expr', 1, undef
	],
	[#Rule 30
		 'path_expr', 3,
sub
#line 180 "xfdxpath.yp"
{
        if ( defined $_[2] ) {
            die "This XPath implementation has no node sets, (expression)[predicate] is not supported\n";
        }
        if ( defined $_[3] ) {
            die "This XPath implementation has no node sets, (expression)/path is not supported\n";
        }
        $_[1];
    }
	],
	[#Rule 31
		 'segment', 0, undef
	],
	[#Rule 32
		 'segment', 2, undef
	],
	[#Rule 33
		 'segment', 2, undef
	],
	[#Rule 34
		 'location_path', 1, undef
	],
	[#Rule 35
		 'location_path', 1, undef
	],
	[#Rule 36
		 'absolute_location_path', 1,
sub
#line 203 "xfdxpath.yp"
{ XFD::doc_node->new }
	],
	[#Rule 37
		 'absolute_location_path', 2,
sub
#line 204 "xfdxpath.yp"
{
        my $op = XFD::doc_node->new( @_[0..1] );
        $op->set_next( $_[2] );
        $op;
    }
	],
	[#Rule 38
		 'absolute_location_path', 2,
sub
#line 209 "xfdxpath.yp"
{ 
        ## /descendant-or-self::node()/relative_location_path
        my $op = XFD::doc_node->new( @_[0..1] );
        my $step = _step(
            $_[0],
            XFD::Axis::descendant_or_self->new,
            XFD::NodeType::node->new,
        );
        $op->set_next( $step );
        $step->set_next( $_[2] );
        $op;
    }
	],
	[#Rule 39
		 'relative_location_path', 1, undef
	],
	[#Rule 40
		 'relative_location_path', 3,
sub
#line 224 "xfdxpath.yp"
{ $_[1]->set_next( $_[3] ) ; $_[1] }
	],
	[#Rule 41
		 'relative_location_path', 3,
sub
#line 227 "xfdxpath.yp"
{
        my $step = _step(
            $_[0],
            XFD::Axis::descendant_or_self->new,
            XFD::NodeType::node->new,
        );
        $_[1]->set_next( $step );
        $step->set_next( $_[3] );
        $_[1];
    }
	],
	[#Rule 42
		 'step', 3,
sub
#line 240 "xfdxpath.yp"
{ _step( @_ ) }
	],
	[#Rule 43
		 'step', 1,
sub
#line 241 "xfdxpath.yp"
{ XFD::self_node->new }
	],
	[#Rule 44
		 'step', 1,
sub
#line 242 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 45
		 'axis', 0,
sub
#line 246 "xfdxpath.yp"
{ XFD::Axis::child->new     }
	],
	[#Rule 46
		 'axis', 2,
sub
#line 247 "xfdxpath.yp"
{ XFD::axis( $_[1] )        }
	],
	[#Rule 47
		 'axis', 1,
sub
#line 248 "xfdxpath.yp"
{ XFD::Axis::attribute->new }
	],
	[#Rule 48
		 'predicates', 0, undef
	],
	[#Rule 49
		 'predicates', 4,
sub
#line 253 "xfdxpath.yp"
{
        my $p = XFD::predicate->new( $_[3] );
        if ( defined $_[1] ) {
            $_[1]->set_next( $p );
            return $_[1];
        }
        return $p;
    }
	],
	[#Rule 50
		 'primary_expr', 1,
sub
#line 264 "xfdxpath.yp"
{ XFD::get_var $_[1] }
	],
	[#Rule 51
		 'primary_expr', 3,
sub
#line 265 "xfdxpath.yp"
{ XFD::parens $_[2] }
	],
	[#Rule 52
		 'primary_expr', 1, undef
	],
	[#Rule 53
		 'primary_expr', 1, undef
	],
	[#Rule 54
		 'primary_expr', 1, undef
	],
	[#Rule 55
		 'function_call', 4,
sub
#line 272 "xfdxpath.yp"
{ XFD::function( @_[1,3] ) }
	],
	[#Rule 56
		 'opt_args', 0,
sub
#line 276 "xfdxpath.yp"
{ [] }
	],
	[#Rule 57
		 'opt_args', 1, undef
	],
	[#Rule 58
		 'args', 1,
sub
#line 281 "xfdxpath.yp"
{
        [ $_[1] ];
    }
	],
	[#Rule 59
		 'args', 3,
sub
#line 284 "xfdxpath.yp"
{
        push @{$_[1]}, $_[3];
        $_[1];
    }
	],
	[#Rule 60
		 'node_test', 1,
sub
#line 291 "xfdxpath.yp"
{ XFD::node_name->new( $_[1] ) }
	],
	[#Rule 61
		 'node_test', 1,
sub
#line 292 "xfdxpath.yp"
{ XFD::any_node_name->new; }
	],
	[#Rule 62
		 'node_test', 1,
sub
#line 293 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 63
		 'node_test', 4,
sub
#line 294 "xfdxpath.yp"
{ XFD::NodeType::processing_instruction
                                                           ->new( $_[0] ) }
	],
	[#Rule 64
		 'node_test', 3,
sub
#line 296 "xfdxpath.yp"
{ XFD::NodeType::comment->new( $_[0] ) }
	],
	[#Rule 65
		 'node_test', 3,
sub
#line 297 "xfdxpath.yp"
{ XFD::NodeType::text   ->new( $_[0] ) }
	],
	[#Rule 66
		 'node_test', 3,
sub
#line 298 "xfdxpath.yp"
{ XFD::NodeType::node   ->new( $_[0] ) }
	],
	[#Rule 67
		 'opt_literal', 0, undef
	],
	[#Rule 68
		 'opt_literal', 1,
sub
#line 303 "xfdxpath.yp"
{ _no @_; }
	]
],
                                  @_);
    bless($self,$class);
}

#line 306 "xfdxpath.yp"


=head1

XML::Filter::Dispatcher::Parser - Parses the XPath subset used by ...::Dispatcher

=head1 SYNOPSIS

   use XML::Filter::Dispatcher::Parser;

   my $result = XML::Filter::Dispatcher::Parser->parse( $xpath );

=head1 DESCRIPTION

Some notes on the parsing and evaluation:

=over

=item *

Result Objects

The result expressions alway return true or false.  For XPath
expressions that would normally return a node-set, the result is true if
the current SAX event would build a node that would be in the node set.
No floating point or string return objects are supported (this may
change).

=item *

Context

The XPath context node is the document root (theoretically; in reality
there is none).  The variables are the Dispatcher's data members, and
the function library is XXX.

Not sure what to do about the context position, but the context size is
of necessity undefined.

The namespace mapping will be added in when I grok the NamespaceHelper.

=back

=cut

use Carp;

my %tokens = (qw(
    .           DOT
    ..          DOT_DOT
    @           AT
    *           STAR
    (           LPAR
    )           RPAR
    [           LSQB
    ]           RSQB
    ::          COLON_COLON
    /           SLASH
    //          SLASH_SLASH
    |           VBAR
    +           PLUS
    -           MINUS
    =           EQUALS
    !=          BANG_EQUALS
    >           GT
    <           LT
    >=          GTE
    <=          LTE

    ==          EQUALS_EQUALS
    ||          VBAR_VBAR
    &&          AMP_AMP
    &           AMP
),
    "," =>      "COMMA"
);

my $simple_tokens =
    join "|",
        map
            quotemeta,
            reverse
                sort {
                    length $a <=> length $b
                } keys %tokens;

my $NCName = "(?:[a-zA-Z_][a-zA-Z0-9_.-]*)"; ## TODO: comb. chars & Extenders

my %NodeType = qw(
    node                   NODE
    text                   TEXT
    comment                COMMENT
    processing-instruction PI
);

my $NodeType = "(?:" .
    join( "|", map quotemeta, sort {length $a <=> length $b} keys %NodeType ) .
    ")";

my $AxisName = "(?:" .  join( "|", split /\n/, <<AXIS_LIST_END ) . ")" ;
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
AXIS_LIST_END

my %preceding_tokens = map { ( $_ => undef ) } ( qw(
    @ :: [
    and or mod div
    *
    / // | + - = != < <= > >=

    == & && ||
    ),
    "(", ","
) ;

sub debugging () { 0 }

sub lex {
    my ( $p ) = @_;

    ## Optimization notes: we aren't parsing War and Peace here, so
    ## readability over performance.

    my $d = $p->{USER};
    my $input = \$d->{Input};

    ## This needs to be more contextual, only recognizing axis/function-name
    if ( ( pos( $$input ) || 0 ) == length $$input ) {
        $d->{LastToken} = undef;
        return ( '', undef );
    }

    my ( $token, $val ) ;
    ## First do the disambiguation rules:

    ## If there is a preceding token and the preceding token is not
    ## one of "@", "::", "(", "[", "," or an Operator,
    if ( defined $d->{LastToken}
        && ! exists $preceding_tokens{$d->{LastToken}}
    ) {
        ## a * must be recognized as a MultiplyOperator
        if ( $$input =~ /\G\s*\*/gc ) {
            ( $token, $val ) = ( MULTIPLY => "*" );
        }
        ## an NCName must be recognized as an OperatorName.
        elsif ( $$input =~ /\G\s*($NCName)/gc ) {
            die "Expected and, or, mod or div, got '$1'"
                unless 0 <= index "and|or|mod|div", $1;
            ( $token, $val ) = ( uc $1, $1 );
        }
    }

    ## NOTE: \s is only an approximation for ExprWhitespace
    unless ( defined $token ) {
        $$input =~ m{\G\s*(?:
            ## If the character following an NCName (possibly after
            ## intervening ExprWhitespace) is (, then the token must be
            ## recognized as a NodeType or a FunctionName.

            ($NCName)\s*(?=\()

            ## If the two characters following an NCName (possibly after
            ## intervening ExprWhitespace) are ::, then the token must be
            ## recognized as an AxisName

            |($NCName)\s*(?=::)

            |($NCName:\*)                           #NAME_COLON_STAR
            |((?:$NCName:)?$NCName)                 #QNAME
            |('[^']*'|"[^"]*")                      #LITERAL
            |(-?\d+(?:\.\d+)?|\.\d+)                #NUMBER
            |\$((?:$NCName:)?$NCName)               #DOLLAR_QNAME
            |($simple_tokens)
        )\s*}gcx;

        ( $token, $val ) =
            defined $1  ? (
                exists $NodeType{$1}
                    ? ( $NodeType{$1}, $1 )
                    : ( FUNCTION_NAME => $1 )
            ) :
        
            defined $2  ? (
                0 <= index( $AxisName, $2 )
                    ? ( AXIS_NAME => $2 )
                    : die "Expected an Axis Name, got '$2' at ",
                        pos $p->{USER}->{Input},
                        "\n"
            ) :
            defined  $3 ? ( NAME_COLON_STAR  =>  $3 ) :
            defined  $4 ? ( QNAME            =>  $4 ) :
            defined  $5 ? ( LITERAL          =>  do {
                    my $s = substr( $5, 1, -1 );
                    $s =~ s/([\\'])/\\$1/g;
                    bless \"'$s'", "string constant";
                }
            ) :
            defined  $6 ? ( NUMBER           =>  bless \"$6", "number constant" ) :
            defined  $7 ? ( DOLLAR_QNAME     =>  $7 ) :
            defined  $8 ? ( $tokens{$8}      =>  $8 ) :
            die "Failed to parse '$$input' at ",
                pos $$input,
                "\n";

        ## the parser needs to know whether an path expression is being
        ## parsed in a predicate or not so it can deal with paths
        ## using immediate code in predicates instead of converting them
        ## to incremental code run as precursors.
        if ( $p->{USER}->{ExitedPredicate} ) {
            --$XFD::predicate_depth;
        }
        $p->{USER}->{ExitedPredicate} = $token eq "RSQB";

        if ( $token eq "LSQB" ) {
            ++$XFD::predicate_depth;
        }
    }

    $d->{LastToken} = $val;

    if ( debugging ) {
        warn
            "'",
            $$input,
            "' (",
            pos $$input,
            "):",
            join( " => ", map defined $_ ? $_ : "<undef>", $token, $val ),
            "\n";
    }

    return ( $token, $val );
}


sub error {
    my ( $p ) = @_;
    print "Couldn't parse '$p->{USER}->{Input}' at position ", pos $p->{USER}->{Input}, "\n";
}

sub parse {
    my $self = shift;
    my ( $xpath, $action_code, $options ) = @_;

    $XFD::has_start_or_end = 0;

    my $p = XML::Filter::Dispatcher::Parser->new(
        yylex   => \&lex,
        yyerror => \&error,
        ( $options->{Debug} || 0 ) > 5
            ? ( yydebug => 0x1D )
            : (),
    );

    local $XFD::fold_constants =
        defined $options->{FoldConstants}
            ? $options->{FoldConstants}
            : 1;

    local $XFD::self_curriers = 0;
    local $XFD::rule_number   = $options->{RuleNumber};
    local $XFD::predicate_depth = 0;

    %{$p->{USER}} = %$options if $options;
    $p->{USER}->{ExitedPredicate} = 0;
    $p->{USER}->{Input} = $xpath;
    my $r = $p->YYParse;

    if ( $p->{USER}->{ExitedPredicate} ) {
        --$XFD::predicate_depth;
        $p->{USER}->{ExitedPredicate} = 0;
    }

    die map "$_\n", @{$p->{USER}->{NONONO}}
        if $p->{USER}->{NONONO} ;

    return undef unless defined $r;

    die "grammar returned '$r', needed a ref\n"
        unless ref $r;

    die "predicate_depth not zero: $XFD::predicate_depth"
        if $XFD::predicate_depth;

    package XFD;
    if ( $r->isa( "XFD::PathTest" ) ) {
        ## It's a relative location path, so 
        ## convert it to code as-is.
        $r = _rel2abs( $r )->as_incr_code( action \$action_code );
    }
    else {
        ## It's not a location path opcode, it's an expression,
        ## so make it get evaluated for every node 
        $r = ${ expr_eval $r, action \$action_code };
        $r = ${ all_nodes_or_self \$r }
            unless @XFD::precursors;
    }

    return () unless defined $r && length $r;

    ## Ok, looks like we have an expression that might be true sometimes,
    ## assemble it and any precursors in to a list of test subs.
    return XFD::expression_as_incr_code $r;

}

1 ;

1;

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


#line 9 "xfdxpath.yp"

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
        my @ops = grep $_, @_;

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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'or_expr' => 18,
			'and_expr' => 9,
			'unary_expr' => 21,
			'equality_expr' => 10,
			'path_expr' => 25,
			'expr' => 24,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 1
		ACTIONS => {
			'VBAR' => 29
		},
		DEFAULT => -25
	},
	{#State 2
		DEFAULT => -53
	},
	{#State 3
		ACTIONS => {
			'SLASH' => 30,
			'SLASH_SLASH' => 31
		},
		DEFAULT => -34
	},
	{#State 4
		DEFAULT => -48,
		GOTOS => {
			'predicates' => 32
		}
	},
	{#State 5
		DEFAULT => -50
	},
	{#State 6
		ACTIONS => {
			'COLON_COLON' => 33
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
			'AND' => 34,
			'AMP' => 35,
			'AMP_AMP' => 36
		},
		DEFAULT => -2
	},
	{#State 10
		ACTIONS => {
			'EQUALS' => 37,
			'BANG_EQUALS' => 39,
			'EQUALS_EQUALS' => 38
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
			'AT' => 23,
			'NODE' => -45
		},
		DEFAULT => -36,
		GOTOS => {
			'relative_location_path' => 40,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 12
		ACTIONS => {
			'PLUS' => 41,
			'MINUS' => 42
		},
		DEFAULT => -13
	},
	{#State 13
		DEFAULT => -29
	},
	{#State 14
		ACTIONS => {
			'LPAR' => 43
		}
	},
	{#State 15
		DEFAULT => -39
	},
	{#State 16
		ACTIONS => {
			'LT' => 46,
			'GT' => 44,
			'LTE' => 47,
			'GTE' => 45
		},
		DEFAULT => -9
	},
	{#State 17
		ACTIONS => {
			'MULTIPLY' => 48,
			'MOD' => 49,
			'DIV' => 50
		},
		DEFAULT => -18
	},
	{#State 18
		ACTIONS => {
			'VBAR_VBAR' => 51,
			'OR' => 52
		},
		DEFAULT => -1
	},
	{#State 19
		DEFAULT => -52
	},
	{#State 20
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 23
		},
		DEFAULT => -45,
		GOTOS => {
			'relative_location_path' => 53,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 21
		DEFAULT => -21
	},
	{#State 22
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'unary_expr' => 54,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 23
		DEFAULT => -47
	},
	{#State 24
		ACTIONS => {
			'' => 55
		}
	},
	{#State 25
		DEFAULT => -27
	},
	{#State 26
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'or_expr' => 18,
			'and_expr' => 9,
			'unary_expr' => 21,
			'equality_expr' => 10,
			'expr' => 56,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 27
		ACTIONS => {
			'QNAME' => 63,
			'NODE' => 64,
			'COMMENT' => 61,
			'PI' => 60,
			'TEXT' => 59,
			'STAR' => 58,
			'NAME_COLON_STAR' => 62
		},
		GOTOS => {
			'node_test' => 57
		}
	},
	{#State 28
		DEFAULT => -35
	},
	{#State 29
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'relative_location_path' => 3,
			'path_expr' => 65,
			'primary_expr' => 4,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 30
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 23
		},
		DEFAULT => -45,
		GOTOS => {
			'axis' => 27,
			'step' => 66
		}
	},
	{#State 31
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 23
		},
		DEFAULT => -45,
		GOTOS => {
			'axis' => 27,
			'step' => 67
		}
	},
	{#State 32
		ACTIONS => {
			'SLASH' => 68,
			'SLASH_SLASH' => 70,
			'LSQB' => 71
		},
		DEFAULT => -31,
		GOTOS => {
			'segment' => 69
		}
	},
	{#State 33
		DEFAULT => -46
	},
	{#State 34
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'equality_expr' => 72,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'equality_expr' => 73,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'equality_expr' => 74,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 75,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 76,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 77,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 40
		ACTIONS => {
			'SLASH' => 30,
			'SLASH_SLASH' => 31
		},
		DEFAULT => -37
	},
	{#State 41
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 78,
			'unary_expr' => 21,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 79,
			'unary_expr' => 21,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'RPAR' => -55,
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'and_expr' => 9,
			'args' => 80,
			'equality_expr' => 10,
			'additive_expr' => 12,
			'location_path' => 13,
			'step' => 15,
			'opt_args' => 81,
			'relational_expr' => 16,
			'multiplicative_expr' => 17,
			'or_expr' => 18,
			'unary_expr' => 21,
			'expr' => 82,
			'path_expr' => 25,
			'absolute_location_path' => 28,
			'axis' => 27
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'additive_expr' => 83,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'additive_expr' => 84,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'additive_expr' => 85,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'unary_expr' => 21,
			'additive_expr' => 86,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'unary_expr' => 87,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'unary_expr' => 88,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'unary_expr' => 89,
			'path_expr' => 25,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'and_expr' => 90,
			'unary_expr' => 21,
			'equality_expr' => 10,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
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
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'and_expr' => 91,
			'unary_expr' => 21,
			'equality_expr' => 10,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 53
		ACTIONS => {
			'SLASH' => 30,
			'SLASH_SLASH' => 31
		},
		DEFAULT => -38
	},
	{#State 54
		DEFAULT => -26
	},
	{#State 55
		DEFAULT => -0
	},
	{#State 56
		ACTIONS => {
			'RPAR' => 92
		}
	},
	{#State 57
		DEFAULT => -48,
		GOTOS => {
			'predicates' => 93
		}
	},
	{#State 58
		DEFAULT => -60
	},
	{#State 59
		ACTIONS => {
			'LPAR' => 94
		}
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
		DEFAULT => -61
	},
	{#State 63
		DEFAULT => -59
	},
	{#State 64
		ACTIONS => {
			'LPAR' => 97
		}
	},
	{#State 65
		DEFAULT => -28
	},
	{#State 66
		DEFAULT => -40
	},
	{#State 67
		DEFAULT => -41
	},
	{#State 68
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 23
		},
		DEFAULT => -45,
		GOTOS => {
			'relative_location_path' => 98,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 69
		DEFAULT => -30
	},
	{#State 70
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 23
		},
		DEFAULT => -45,
		GOTOS => {
			'relative_location_path' => 99,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 71
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'or_expr' => 18,
			'and_expr' => 9,
			'unary_expr' => 21,
			'equality_expr' => 10,
			'expr' => 100,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 72
		ACTIONS => {
			'EQUALS' => 37,
			'BANG_EQUALS' => 39,
			'EQUALS_EQUALS' => 38
		},
		DEFAULT => -6
	},
	{#State 73
		ACTIONS => {
			'EQUALS' => 37,
			'BANG_EQUALS' => 39,
			'EQUALS_EQUALS' => 38
		},
		DEFAULT => -8
	},
	{#State 74
		ACTIONS => {
			'EQUALS' => 37,
			'BANG_EQUALS' => 39,
			'EQUALS_EQUALS' => 38
		},
		DEFAULT => -7
	},
	{#State 75
		ACTIONS => {
			'LT' => 46,
			'GT' => 44,
			'LTE' => 47,
			'GTE' => 45
		},
		DEFAULT => -10
	},
	{#State 76
		ACTIONS => {
			'LT' => 46,
			'GT' => 44,
			'LTE' => 47,
			'GTE' => 45
		},
		DEFAULT => -12
	},
	{#State 77
		ACTIONS => {
			'LT' => 46,
			'GT' => 44,
			'LTE' => 47,
			'GTE' => 45
		},
		DEFAULT => -11
	},
	{#State 78
		ACTIONS => {
			'MULTIPLY' => 48,
			'MOD' => 49,
			'DIV' => 50
		},
		DEFAULT => -19
	},
	{#State 79
		ACTIONS => {
			'MULTIPLY' => 48,
			'MOD' => 49,
			'DIV' => 50
		},
		DEFAULT => -20
	},
	{#State 80
		ACTIONS => {
			'COMMA' => 101
		},
		DEFAULT => -56
	},
	{#State 81
		ACTIONS => {
			'RPAR' => 102
		}
	},
	{#State 82
		DEFAULT => -57
	},
	{#State 83
		ACTIONS => {
			'PLUS' => 41,
			'MINUS' => 42
		},
		DEFAULT => -15
	},
	{#State 84
		ACTIONS => {
			'PLUS' => 41,
			'MINUS' => 42
		},
		DEFAULT => -17
	},
	{#State 85
		ACTIONS => {
			'PLUS' => 41,
			'MINUS' => 42
		},
		DEFAULT => -14
	},
	{#State 86
		ACTIONS => {
			'PLUS' => 41,
			'MINUS' => 42
		},
		DEFAULT => -16
	},
	{#State 87
		DEFAULT => -22
	},
	{#State 88
		DEFAULT => -24
	},
	{#State 89
		DEFAULT => -23
	},
	{#State 90
		ACTIONS => {
			'AND' => 34,
			'AMP' => 35,
			'AMP_AMP' => 36
		},
		DEFAULT => -4
	},
	{#State 91
		ACTIONS => {
			'AND' => 34,
			'AMP' => 35,
			'AMP_AMP' => 36
		},
		DEFAULT => -3
	},
	{#State 92
		DEFAULT => -51
	},
	{#State 93
		ACTIONS => {
			'LSQB' => 71
		},
		DEFAULT => -42
	},
	{#State 94
		ACTIONS => {
			'RPAR' => 103
		}
	},
	{#State 95
		ACTIONS => {
			'LITERAL' => 104
		},
		DEFAULT => -66,
		GOTOS => {
			'opt_literal' => 105
		}
	},
	{#State 96
		ACTIONS => {
			'RPAR' => 106
		}
	},
	{#State 97
		ACTIONS => {
			'RPAR' => 107
		}
	},
	{#State 98
		ACTIONS => {
			'SLASH' => 30,
			'SLASH_SLASH' => 31
		},
		DEFAULT => -32
	},
	{#State 99
		ACTIONS => {
			'SLASH' => 30,
			'SLASH_SLASH' => 31
		},
		DEFAULT => -33
	},
	{#State 100
		ACTIONS => {
			'RSQB' => 108
		}
	},
	{#State 101
		ACTIONS => {
			'NUMBER' => 2,
			'AXIS_NAME' => 6,
			'DOLLAR_QNAME' => 5,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'SLASH' => 11,
			'FUNCTION_NAME' => 14,
			'LITERAL' => 19,
			'SLASH_SLASH' => 20,
			'MINUS' => 22,
			'AT' => 23,
			'LPAR' => 26
		},
		DEFAULT => -45,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 17,
			'or_expr' => 18,
			'and_expr' => 9,
			'unary_expr' => 21,
			'equality_expr' => 10,
			'expr' => 109,
			'path_expr' => 25,
			'additive_expr' => 12,
			'location_path' => 13,
			'absolute_location_path' => 28,
			'axis' => 27,
			'step' => 15
		}
	},
	{#State 102
		DEFAULT => -54
	},
	{#State 103
		DEFAULT => -64
	},
	{#State 104
		DEFAULT => -67
	},
	{#State 105
		ACTIONS => {
			'RPAR' => 110
		}
	},
	{#State 106
		DEFAULT => -63
	},
	{#State 107
		DEFAULT => -65
	},
	{#State 108
		DEFAULT => -49
	},
	{#State 109
		DEFAULT => -58
	},
	{#State 110
		DEFAULT => -62
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
#line 107 "xfdxpath.yp"
{ XFD::Operator::or->new( @_[1,3] ) }
	],
	[#Rule 4
		 'or_expr', 3,
sub
#line 108 "xfdxpath.yp"
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
#line 115 "xfdxpath.yp"
{ XFD::Operator::and->new( @_[1,3] ) }
	],
	[#Rule 7
		 'and_expr', 3,
sub
#line 116 "xfdxpath.yp"
{
        die "XPath uses 'and' instead of Perl's '&&'\n";
    }
	],
	[#Rule 8
		 'and_expr', 3,
sub
#line 119 "xfdxpath.yp"
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
#line 126 "xfdxpath.yp"
{ XFD::relational_op equals     => @_[1,3] }
	],
	[#Rule 11
		 'equality_expr', 3,
sub
#line 127 "xfdxpath.yp"
{ XFD::relational_op not_equals => @_[1,3] }
	],
	[#Rule 12
		 'equality_expr', 3,
sub
#line 128 "xfdxpath.yp"
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
#line 135 "xfdxpath.yp"
{ XFD::relational_op lt  => @_[1,3] }
	],
	[#Rule 15
		 'relational_expr', 3,
sub
#line 136 "xfdxpath.yp"
{ XFD::relational_op gt  => @_[1,3] }
	],
	[#Rule 16
		 'relational_expr', 3,
sub
#line 137 "xfdxpath.yp"
{ XFD::relational_op lte => @_[1,3] }
	],
	[#Rule 17
		 'relational_expr', 3,
sub
#line 138 "xfdxpath.yp"
{ XFD::relational_op gte => @_[1,3] }
	],
	[#Rule 18
		 'additive_expr', 1, undef
	],
	[#Rule 19
		 'additive_expr', 3,
sub
#line 143 "xfdxpath.yp"
{ XFD::math_op addition    => @_[1,3] }
	],
	[#Rule 20
		 'additive_expr', 3,
sub
#line 144 "xfdxpath.yp"
{ XFD::math_op subtraction => @_[1,3] }
	],
	[#Rule 21
		 'multiplicative_expr', 1, undef
	],
	[#Rule 22
		 'multiplicative_expr', 3,
sub
#line 149 "xfdxpath.yp"
{ XFD::math_op multiplication => @_[1,3] }
	],
	[#Rule 23
		 'multiplicative_expr', 3,
sub
#line 150 "xfdxpath.yp"
{ XFD::math_op division       => @_[1,3] }
	],
	[#Rule 24
		 'multiplicative_expr', 3,
sub
#line 151 "xfdxpath.yp"
{ XFD::math_op modulus        => @_[1,3] }
	],
	[#Rule 25
		 'unary_expr', 1, undef
	],
	[#Rule 26
		 'unary_expr', 2,
sub
#line 156 "xfdxpath.yp"
{ XFD::Negation->new( $_[2] ) }
	],
	[#Rule 27
		 'union_expr', 1, undef
	],
	[#Rule 28
		 'union_expr', 3,
sub
#line 161 "xfdxpath.yp"
{
        for ( $_[1], $_[3] ) {
            next if $_->can( "set_next" );
            $_ = ref $_;
            s/^XFD:://;
            die "Can't use a $_ in a union, perhaps you want || instead of |\n";
        }

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
#line 183 "xfdxpath.yp"
{
        return $_[1] unless defined $_[2] || defined $_[3];

        my $expr = $_[1];
        $expr = $expr->[0] if $expr->isa( "XFD::Parens" );

        ## TODO: Cope with nodesets passed in vars or
        ## returned from functions.
        die "node-set is required before a predicate or '/' (variables and functions returning nodesets are not (yet) supported)"
            unless $expr->isa( "XFD::PathTest" );

        $expr->set_next( $_[2] ) if defined $_[2];
        $expr->set_next( $_[3] ) if defined $_[3];
        $expr;
    }
	],
	[#Rule 31
		 'segment', 0, undef
	],
	[#Rule 32
		 'segment', 2,
sub
#line 202 "xfdxpath.yp"
{ $_[2] }
	],
	[#Rule 33
		 'segment', 2,
sub
#line 203 "xfdxpath.yp"
{
        my $op = XFD::Axis::descendant_or_self->new;
        $op->set_next( $_[2] );
        $op;
    }
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
#line 215 "xfdxpath.yp"
{ XFD::doc_node->new }
	],
	[#Rule 37
		 'absolute_location_path', 2,
sub
#line 216 "xfdxpath.yp"
{
        my $op = XFD::doc_node->new;
        $op->set_next( $_[2] );
        $op;
    }
	],
	[#Rule 38
		 'absolute_location_path', 2,
sub
#line 221 "xfdxpath.yp"
{ 
        ## /descendant-or-self::node()/relative_location_path
        my $op = XFD::doc_node->new;
        my $step = _step(
            XFD::Axis::descendant_or_self->new,
            XFD::EventType::node->new,
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
#line 235 "xfdxpath.yp"
{ $_[1]->set_next( $_[3] ) ; $_[1] }
	],
	[#Rule 41
		 'relative_location_path', 3,
sub
#line 238 "xfdxpath.yp"
{
        my $step = _step(
            XFD::Axis::descendant_or_self->new,
            XFD::EventType::node->new,
        );
        $_[1]->set_next( $step );
        $step->set_next( $_[3] );
        $_[1];
    }
	],
	[#Rule 42
		 'step', 3,
sub
#line 250 "xfdxpath.yp"
{ _step( @_[1..$#_] ) }
	],
	[#Rule 43
		 'step', 1,
sub
#line 251 "xfdxpath.yp"
{ XFD::self_node->new }
	],
	[#Rule 44
		 'step', 1,
sub
#line 252 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 45
		 'axis', 0,
sub
#line 256 "xfdxpath.yp"
{ XFD::Axis::child->new     }
	],
	[#Rule 46
		 'axis', 2,
sub
#line 257 "xfdxpath.yp"
{ XFD::axis( $_[1] )        }
	],
	[#Rule 47
		 'axis', 1,
sub
#line 258 "xfdxpath.yp"
{ XFD::Axis::attribute->new }
	],
	[#Rule 48
		 'predicates', 0, undef
	],
	[#Rule 49
		 'predicates', 4,
sub
#line 263 "xfdxpath.yp"
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
#line 274 "xfdxpath.yp"
{ XFD::VariableReference->new( $_[1] ) }
	],
	[#Rule 51
		 'primary_expr', 3,
sub
#line 275 "xfdxpath.yp"
{ XFD::Parens->new( $_[2] ) }
	],
	[#Rule 52
		 'primary_expr', 1, undef
	],
	[#Rule 53
		 'primary_expr', 1, undef
	],
	[#Rule 54
		 'primary_expr', 4,
sub
#line 278 "xfdxpath.yp"
{ XFD::function( @_[1,3] ) }
	],
	[#Rule 55
		 'opt_args', 0,
sub
#line 282 "xfdxpath.yp"
{ [] }
	],
	[#Rule 56
		 'opt_args', 1, undef
	],
	[#Rule 57
		 'args', 1,
sub
#line 287 "xfdxpath.yp"
{ [ $_[1] ] }
	],
	[#Rule 58
		 'args', 3,
sub
#line 288 "xfdxpath.yp"
{
        push @{$_[1]}, $_[3];
        $_[1];
    }
	],
	[#Rule 59
		 'node_test', 1,
sub
#line 295 "xfdxpath.yp"
{ XFD::node_name->new( $_[1] ) }
	],
	[#Rule 60
		 'node_test', 1,
sub
#line 296 "xfdxpath.yp"
{ XFD::EventType::principal_event_type->new; }
	],
	[#Rule 61
		 'node_test', 1,
sub
#line 297 "xfdxpath.yp"
{ XFD::namespace_test->new( $_[1] ) }
	],
	[#Rule 62
		 'node_test', 4,
sub
#line 298 "xfdxpath.yp"
{ XFD::EventType::processing_instruction
                                                           ->new( $_[2] ) }
	],
	[#Rule 63
		 'node_test', 3,
sub
#line 300 "xfdxpath.yp"
{ XFD::EventType::comment      ->new }
	],
	[#Rule 64
		 'node_test', 3,
sub
#line 301 "xfdxpath.yp"
{ XFD::EventType::text         ->new }
	],
	[#Rule 65
		 'node_test', 3,
sub
#line 302 "xfdxpath.yp"
{ XFD::EventType::node         ->new }
	],
	[#Rule 66
		 'opt_literal', 0, undef
	],
	[#Rule 67
		 'opt_literal', 1,
sub
#line 307 "xfdxpath.yp"
{ _no @_; }
	]
],
                                  @_);
    bless($self,$class);
}

#line 310 "xfdxpath.yp"


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

my %EventType = qw(
    node                   NODE
    text                   TEXT
    comment                COMMENT
    processing-instruction PI
);

my $EventType = "(?:" .
    join( "|", map quotemeta, sort {length $a <=> length $b} keys %EventType ) .
    ")";

my $AxisName = "(?:" .  join( "|", split /\n+/, <<AXIS_LIST_END ) . ")" ;
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
end
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
            die "Expected and, or, mod or div, got '$1'\n"
                unless 0 <= index "and|or|mod|div", $1;
            ( $token, $val ) = ( uc $1, $1 );
        }
    }

    ## NOTE: \s is only an approximation for ExprWhitespace
    unless ( defined $token ) {
        $$input =~ m{\G\s*(?:
            ## If the character following an NCName (possibly after
            ## intervening ExprWhitespace) is (, then the token must be
            ## recognized as a EventType or a FunctionName.
            ($NCName)\s*(?=\()

            ## If the two characters following an NCName (possibly after
            ## intervening ExprWhitespace) are ::, then the token must be
            ## recognized as an AxisName
            |($NCName)\s*(?=::)

            ## Otherwise, it's just a normal lexer.
            |($NCName:\*)                           #NAME_COLON_STAR
            |((?:$NCName:)?$NCName)                 #QNAME
            |('[^']*'|"[^"]*")                      #LITERAL
            |(-?\d+(?:\.\d+)?|\.\d+)                #NUMBER
            |\$((?:$NCName:)?$NCName)               #DOLLAR_QNAME
            |($simple_tokens)
        )\s*}gcx;

        ( $token, $val ) =
            defined $1  ? (
                exists $EventType{$1}
                    ? ( $EventType{$1}, $1 )
                    : ( FUNCTION_NAME => $1 )
            ) :
        
            defined $2 ? ( AXIS_NAME        =>  $2 ) :
            defined $3 ? ( NAME_COLON_STAR  =>  $3 ) :
            defined $4 ? ( QNAME            =>  $4 ) :
            defined $5 ? ( LITERAL          =>  do {
                    my $s = substr( $5, 1, -1 );
                    $s =~ s/([\\'])/\\$1/g;
                    XFD::StringConstant->new( $s );
                }
            ) :
            defined $6 ? ( NUMBER           =>
                XFD::NumericConstant->new( "$6")
            ) :
            defined $7 ? ( DOLLAR_QNAME     =>  $7 ) :
            defined $8 ? ( $tokens{$8}      =>  $8 ) :
            die "Failed to parse '$$input' at ",
                pos $$input,
                "\n";
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

## _parse is an internal, reentrant entry point; it's used to parse rules
## and subrules.
sub _parse {
    my $self = shift;
    my ( $expr, $action_code ) = @_;

    my $options = $XFD::dispatcher;

    warn "Parsing '$expr'\n" if $options->{Debug};

    my $p = XML::Filter::Dispatcher::Parser->new(
        yylex   => \&lex,
        yyerror => \&error,
        ( $options->{Debug} || 0 ) > 5
            ? ( yydebug => 0x1D )
            : (),
    );

    %{$p->{USER}} = %$options if $options;
    $p->{USER}->{Input} = $expr;
    local $XFD::dispatcher->{ParseNestingDepth}
        = $XFD::dispatcher->{ParseNestingDepth} + 1;

    my $op_tree = eval {
        $p->YYParse;                ## <== the actual parse
    };

    die $@ if $@;

    die map "$_\n", @{$p->{USER}->{NONONO}}
        if $p->{USER}->{NONONO} ;

    return undef unless defined $op_tree;

    die "grammar returned '$op_tree', needed a ref\n"
        unless ref $op_tree;

    ## TODO: figure a way to allow a limited subset
    ## of EventPath patterns, kinda like allowing
    ## a pattern match against the generated Op tree,
    ## or alternate grammar files.  The former could
    ## give more helpful error messages, the latter
    ## could be more flexible because it would allow
    ## non-standard grammars.
    $op_tree = XFD::ExprEval->new( $op_tree )
        unless $op_tree->isa( "XFD::PathTest" );

    $op_tree->set_next( XFD::action( $action_code ) );

    return $op_tree;
}


sub parse {
    my $self = shift;
    local $XFD::dispatcher = shift;
    my ( $expr, $context ) = @_;

    $XFD::dispatcher->{ParseNestingDepth} = 0;
    $XFD::dispatcher->{OpTree} ||= XFD::union->new;

    my $op_tree = $self->_parse( @_ );

    if ( ( $XFD::dispatcher->{Debug} || 0 ) > 1 ) {
        open F, ">foo.png";
        print F $op_tree->as_graphviz->as_png;
        close F;
        system( "ee foo.png" );
    }

    my $rule = XFD::Rule->new( $expr );
    $rule->set_next( $op_tree );
    $XFD::dispatcher->{OpTree}->add( $rule );
}

1 ;

1;

####################################################################
#
#    This file was generated using Parse::Yapp version 1.05.
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
# (c) Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (see the pod text in Parse::Yapp module for use and distribution rights)
#

package Parse::Yapp::Driver;

require 5.004;

use strict;

use vars qw ( $VERSION $COMPATIBLE $FILENAME );

$VERSION = '1.05';
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

    use XML::Filter::Dispatcher::Ops;

    sub _no {
        my $p = shift;
        push @{$p->{USER}->{NONONO}}, join(
            "",
            "XPath construct not supported: ",
            join( "", map ref $_ ? do {
                my $f = ref $_;
                $f =~ s/^XFD:://;
                $f;
            } : $_, @_ ),
            " (grammar rule at ",
            (caller)[1],
            ", line ",
            (caller)[2],
            ")"
        );
    }


sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    my($self)=$class->SUPER::new( yyversion => '1.05',
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
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
		DEFAULT => -20
	},
	{#State 2
		DEFAULT => -48
	},
	{#State 3
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -29
	},
	{#State 4
		DEFAULT => -43,
		GOTOS => {
			'predicates' => 33
		}
	},
	{#State 5
		DEFAULT => -45
	},
	{#State 6
		ACTIONS => {
			'COLON_COLON' => 34
		}
	},
	{#State 7
		DEFAULT => -39
	},
	{#State 8
		DEFAULT => -38
	},
	{#State 9
		ACTIONS => {
			'AND' => 35
		},
		DEFAULT => -2
	},
	{#State 10
		ACTIONS => {
			'EQUALS' => 36
		},
		DEFAULT => -4
	},
	{#State 11
		ACTIONS => {
			'AXIS_NAME' => 6,
			'STAR' => -40,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'PI' => -40,
			'TEXT' => -40,
			'COMMENT' => -40,
			'NAME_COLON_STAR' => -40,
			'QNAME' => -40,
			'AT' => 24,
			'NODE' => -40
		},
		DEFAULT => -31,
		GOTOS => {
			'relative_location_path' => 37,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 12
		ACTIONS => {
			'MINUS' => 39,
			'PLUS' => 38
		},
		DEFAULT => -8
	},
	{#State 13
		DEFAULT => -24
	},
	{#State 14
		ACTIONS => {
			'LPAR' => 40
		}
	},
	{#State 15
		DEFAULT => -34
	},
	{#State 16
		ACTIONS => {
			'LT' => 43,
			'GT' => 41,
			'LTE' => 44,
			'GTE' => 42
		},
		DEFAULT => -6
	},
	{#State 17
		DEFAULT => -49
	},
	{#State 18
		ACTIONS => {
			'MOD' => 46,
			'MULTIPLY' => 45,
			'DIV' => 47
		},
		DEFAULT => -13
	},
	{#State 19
		ACTIONS => {
			'OR' => 48
		},
		DEFAULT => -1
	},
	{#State 20
		DEFAULT => -47
	},
	{#State 21
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 24
		},
		DEFAULT => -40,
		GOTOS => {
			'relative_location_path' => 49,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 22
		DEFAULT => -16
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'function_call' => 17,
			'unary_expr' => 50,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 24
		DEFAULT => -42
	},
	{#State 25
		ACTIONS => {
			'' => 51
		}
	},
	{#State 26
		DEFAULT => -22
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
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
			'expr' => 52,
			'absolute_location_path' => 29,
			'axis' => 28
		}
	},
	{#State 28
		ACTIONS => {
			'QNAME' => 59,
			'NODE' => 60,
			'COMMENT' => 57,
			'PI' => 56,
			'TEXT' => 55,
			'STAR' => 54,
			'NAME_COLON_STAR' => 58
		},
		GOTOS => {
			'node_test' => 53
		}
	},
	{#State 29
		DEFAULT => -30
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
		DEFAULT => -40,
		GOTOS => {
			'relative_location_path' => 3,
			'path_expr' => 61,
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
		DEFAULT => -40,
		GOTOS => {
			'axis' => 28,
			'step' => 62
		}
	},
	{#State 32
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 24
		},
		DEFAULT => -40,
		GOTOS => {
			'axis' => 28,
			'step' => 63
		}
	},
	{#State 33
		ACTIONS => {
			'SLASH' => 64,
			'SLASH_SLASH' => 66,
			'LSQB' => 67
		},
		DEFAULT => -26,
		GOTOS => {
			'segment' => 65
		}
	},
	{#State 34
		DEFAULT => -41
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'equality_expr' => 68,
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 69,
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
	{#State 37
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -32
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 70,
			'function_call' => 17,
			'unary_expr' => 22,
			'path_expr' => 26,
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 71,
			'function_call' => 17,
			'unary_expr' => 22,
			'path_expr' => 26,
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
			'RPAR' => -51,
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'and_expr' => 9,
			'args' => 72,
			'equality_expr' => 10,
			'additive_expr' => 12,
			'location_path' => 13,
			'step' => 15,
			'opt_args' => 73,
			'relational_expr' => 16,
			'function_call' => 17,
			'multiplicative_expr' => 18,
			'or_expr' => 19,
			'unary_expr' => 22,
			'expr' => 74,
			'path_expr' => 26,
			'absolute_location_path' => 29,
			'axis' => 28
		}
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
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'additive_expr' => 75,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
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
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'additive_expr' => 76,
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'additive_expr' => 77,
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
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'unary_expr' => 22,
			'additive_expr' => 78,
			'path_expr' => 26,
			'location_path' => 13,
			'absolute_location_path' => 29,
			'axis' => 28,
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
			'LITERAL' => 20,
			'SLASH_SLASH' => 21,
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'function_call' => 17,
			'unary_expr' => 79,
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'function_call' => 17,
			'unary_expr' => 80,
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'primary_expr' => 4,
			'function_call' => 17,
			'unary_expr' => 81,
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
		GOTOS => {
			'union_expr' => 1,
			'relative_location_path' => 3,
			'relational_expr' => 16,
			'primary_expr' => 4,
			'multiplicative_expr' => 18,
			'function_call' => 17,
			'and_expr' => 82,
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
	{#State 49
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -33
	},
	{#State 50
		DEFAULT => -21
	},
	{#State 51
		DEFAULT => -0
	},
	{#State 52
		ACTIONS => {
			'RPAR' => 83
		}
	},
	{#State 53
		DEFAULT => -43,
		GOTOS => {
			'predicates' => 84
		}
	},
	{#State 54
		DEFAULT => -56
	},
	{#State 55
		ACTIONS => {
			'LPAR' => 85
		}
	},
	{#State 56
		ACTIONS => {
			'LPAR' => 86
		}
	},
	{#State 57
		ACTIONS => {
			'LPAR' => 87
		}
	},
	{#State 58
		DEFAULT => -57
	},
	{#State 59
		DEFAULT => -55
	},
	{#State 60
		ACTIONS => {
			'LPAR' => 88
		}
	},
	{#State 61
		DEFAULT => -23
	},
	{#State 62
		DEFAULT => -35
	},
	{#State 63
		DEFAULT => -36
	},
	{#State 64
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 24
		},
		DEFAULT => -40,
		GOTOS => {
			'relative_location_path' => 89,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 65
		DEFAULT => -25
	},
	{#State 66
		ACTIONS => {
			'AXIS_NAME' => 6,
			'DOT' => 8,
			'DOT_DOT' => 7,
			'AT' => 24
		},
		DEFAULT => -40,
		GOTOS => {
			'relative_location_path' => 90,
			'axis' => 28,
			'step' => 15
		}
	},
	{#State 67
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
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
			'expr' => 91,
			'absolute_location_path' => 29,
			'axis' => 28
		}
	},
	{#State 68
		ACTIONS => {
			'EQUALS' => 36
		},
		DEFAULT => -5
	},
	{#State 69
		ACTIONS => {
			'LT' => 43,
			'GT' => 41,
			'LTE' => 44,
			'GTE' => 42
		},
		DEFAULT => -7
	},
	{#State 70
		ACTIONS => {
			'MOD' => 46,
			'MULTIPLY' => 45,
			'DIV' => 47
		},
		DEFAULT => -14
	},
	{#State 71
		ACTIONS => {
			'MOD' => 46,
			'MULTIPLY' => 45,
			'DIV' => 47
		},
		DEFAULT => -15
	},
	{#State 72
		ACTIONS => {
			'COMMA' => 92
		},
		DEFAULT => -52
	},
	{#State 73
		ACTIONS => {
			'RPAR' => 93
		}
	},
	{#State 74
		DEFAULT => -53
	},
	{#State 75
		ACTIONS => {
			'MINUS' => 39,
			'PLUS' => 38
		},
		DEFAULT => -10
	},
	{#State 76
		ACTIONS => {
			'MINUS' => 39,
			'PLUS' => 38
		},
		DEFAULT => -12
	},
	{#State 77
		ACTIONS => {
			'MINUS' => 39,
			'PLUS' => 38
		},
		DEFAULT => -9
	},
	{#State 78
		ACTIONS => {
			'MINUS' => 39,
			'PLUS' => 38
		},
		DEFAULT => -11
	},
	{#State 79
		DEFAULT => -17
	},
	{#State 80
		DEFAULT => -19
	},
	{#State 81
		DEFAULT => -18
	},
	{#State 82
		ACTIONS => {
			'AND' => 35
		},
		DEFAULT => -3
	},
	{#State 83
		DEFAULT => -46
	},
	{#State 84
		ACTIONS => {
			'LSQB' => 67
		},
		DEFAULT => -37
	},
	{#State 85
		ACTIONS => {
			'RPAR' => 94
		}
	},
	{#State 86
		ACTIONS => {
			'LITERAL' => 95
		},
		DEFAULT => -62,
		GOTOS => {
			'opt_literal' => 96
		}
	},
	{#State 87
		ACTIONS => {
			'RPAR' => 97
		}
	},
	{#State 88
		ACTIONS => {
			'RPAR' => 98
		}
	},
	{#State 89
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -27
	},
	{#State 90
		ACTIONS => {
			'SLASH' => 31,
			'SLASH_SLASH' => 32
		},
		DEFAULT => -28
	},
	{#State 91
		ACTIONS => {
			'RSQB' => 99
		}
	},
	{#State 92
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
			"-" => 23,
			'AT' => 24,
			'LPAR' => 27
		},
		DEFAULT => -40,
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
			'expr' => 100,
			'absolute_location_path' => 29,
			'axis' => 28
		}
	},
	{#State 93
		DEFAULT => -50
	},
	{#State 94
		DEFAULT => -60
	},
	{#State 95
		DEFAULT => -63
	},
	{#State 96
		ACTIONS => {
			'RPAR' => 101
		}
	},
	{#State 97
		DEFAULT => -59
	},
	{#State 98
		DEFAULT => -61
	},
	{#State 99
		DEFAULT => -44
	},
	{#State 100
		DEFAULT => -54
	},
	{#State 101
		DEFAULT => -58
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
#line 71 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 4
		 'and_expr', 1, undef
	],
	[#Rule 5
		 'and_expr', 3,
sub
#line 76 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 6
		 'equality_expr', 1, undef
	],
	[#Rule 7
		 'equality_expr', 3,
sub
#line 81 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 8
		 'relational_expr', 1, undef
	],
	[#Rule 9
		 'relational_expr', 3,
sub
#line 86 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 10
		 'relational_expr', 3,
sub
#line 87 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 11
		 'relational_expr', 3,
sub
#line 88 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 12
		 'relational_expr', 3,
sub
#line 89 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 13
		 'additive_expr', 1, undef
	],
	[#Rule 14
		 'additive_expr', 3,
sub
#line 94 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 15
		 'additive_expr', 3,
sub
#line 95 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 16
		 'multiplicative_expr', 1, undef
	],
	[#Rule 17
		 'multiplicative_expr', 3,
sub
#line 100 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 18
		 'multiplicative_expr', 3,
sub
#line 101 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 19
		 'multiplicative_expr', 3,
sub
#line 102 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 20
		 'unary_expr', 1, undef
	],
	[#Rule 21
		 'unary_expr', 2,
sub
#line 107 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 22
		 'union_expr', 1, undef
	],
	[#Rule 23
		 'union_expr', 3,
sub
#line 112 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 24
		 'path_expr', 1, undef
	],
	[#Rule 25
		 'path_expr', 3,
sub
#line 117 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 26
		 'segment', 0,
sub
#line 121 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 27
		 'segment', 2,
sub
#line 122 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 28
		 'segment', 2,
sub
#line 123 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 29
		 'location_path', 1,
sub
#line 127 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 30
		 'location_path', 1, undef
	],
	[#Rule 31
		 'absolute_location_path', 1,
sub
#line 132 "xfdxpath.yp"
{ XFD::doc_node->new( @_, undef ) }
	],
	[#Rule 32
		 'absolute_location_path', 2,
sub
#line 133 "xfdxpath.yp"
{ XFD::doc_node->new( @_ ) }
	],
	[#Rule 33
		 'absolute_location_path', 2,
sub
#line 134 "xfdxpath.yp"
{ 
      ## /descendant-or-self::node()/relative_location_path
      my $step = XFD::step->new(
          $_[0],
          "descendant-or-self",
          XFD::node_type->new( $_[0], "node" ),
          undef, ## predicates.
      );
      $step->set_next( $_[2] );
      XFD::doc_node->new( $_[0], $_[1], $step );
  }
	],
	[#Rule 34
		 'relative_location_path', 1, undef
	],
	[#Rule 35
		 'relative_location_path', 3,
sub
#line 148 "xfdxpath.yp"
{ $_[1]->set_next( $_[3] ) ; $_[1] }
	],
	[#Rule 36
		 'relative_location_path', 3,
sub
#line 149 "xfdxpath.yp"
{
      ## relative_location_path/descendant-or-self::node()/step
      my $step = XFD::step->new(
          $_[0],
          "descendant-or-self",
          XFD::node_type->new( $_[0], "node" ),
          undef, ## predicates.
      );
      $_[1]->set_next( $step );
      $step->set_next( $_[3] );
      $_[1];
  }
	],
	[#Rule 37
		 'step', 3,
sub
#line 164 "xfdxpath.yp"
{ XFD::step->new( @_ ) }
	],
	[#Rule 38
		 'step', 1,
sub
#line 165 "xfdxpath.yp"
{
      ## /self::node()
      XFD::step->new(
        $_[0],
        "self",
        XFD::node_type->new(
            $_[0],
            "node"
        ),
        undef
    );
  }
	],
	[#Rule 39
		 'step', 1,
sub
#line 177 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 40
		 'axis', 0, undef
	],
	[#Rule 41
		 'axis', 2, undef
	],
	[#Rule 42
		 'axis', 1,
sub
#line 183 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 43
		 'predicates', 0, undef
	],
	[#Rule 44
		 'predicates', 4,
sub
#line 188 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 45
		 'primary_expr', 1,
sub
#line 192 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 46
		 'primary_expr', 3,
sub
#line 193 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 47
		 'primary_expr', 1,
sub
#line 194 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 48
		 'primary_expr', 1,
sub
#line 195 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 49
		 'primary_expr', 1,
sub
#line 196 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 50
		 'function_call', 4,
sub
#line 200 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 51
		 'opt_args', 0,
sub
#line 204 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 52
		 'opt_args', 1,
sub
#line 205 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 53
		 'args', 1,
sub
#line 209 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 54
		 'args', 3,
sub
#line 210 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 55
		 'node_test', 1, undef
	],
	[#Rule 56
		 'node_test', 1, undef
	],
	[#Rule 57
		 'node_test', 1,
sub
#line 216 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 58
		 'node_test', 4,
sub
#line 217 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 59
		 'node_test', 3,
sub
#line 218 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 60
		 'node_test', 3,
sub
#line 219 "xfdxpath.yp"
{ _no @_; }
	],
	[#Rule 61
		 'node_test', 3,
sub
#line 220 "xfdxpath.yp"
{ XFD::node_type->new( $_[0], $_[1] ) }
	],
	[#Rule 62
		 'opt_literal', 0, undef
	],
	[#Rule 63
		 'opt_literal', 1,
sub
#line 225 "xfdxpath.yp"
{ _no @_; }
	]
],
                                  @_);
    bless($self,$class);
}

#line 228 "xfdxpath.yp"


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
    >           GT
    <           LT
    >=          GTE
    <=          LTE
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
            |(\d+(?:\.\d+)?|\.\d+)                  #NUMBER
            |(\$(?:$NCName:)?$NCName)               #DOLLAR_QNAME
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
            defined  $5 ? ( LITERAL          =>  $5 ) :
            defined  $6 ? ( NUMBER           =>  $6 ) :
            defined  $7 ? ( DOLLAR_QNAME     =>  $7 ) :
            defined  $8 ? ( $tokens{ $8}     =>  $8 ) :
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

sub parse {
    my $self = shift;
    my ( $xpath, $options ) = @_;

    my $p = XML::Filter::Dispatcher::Parser->new(
        yylex   => \&lex,
        yyerror => \&error,
        $options->{Debug}
            ? ( yydebug => 0x1D )
            : (),
    );
    %{$p->{USER}} = %$options if $options;
    $p->{USER}->{Input} = $xpath;
    my $r = $p->YYParse;

    croak join "\n", @{$p->{USER}->{NONONO}}
        if $p->{USER}->{NONONO} ;

    return $r;
}

1 ;

1;

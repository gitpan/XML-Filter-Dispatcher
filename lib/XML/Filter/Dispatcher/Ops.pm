package XML::Filter::Dispatcher::Ops;

=head1 NAME

XML::Filter::Dispatcher::Ops - The Syntax Tree

=head1 SYNOPSIS

    None.  Used by XML::Filter::Dispatcher.

=cut

## TODO: Replace XFD:: with XML::Filter::Dispatcher

package XFD;

use Carp;

use strict;

use vars ( 
    '$fold_constants',    ## Whether or not to...  Set by ::Parser
    '$rule_number',       ## What rule number this is.  Set by ::Parser.

    '$has_start_or_end',  ## set when a start() or end() is compiled
    '$anon_sub_count',    ## # of anon subs registered with current Dispatcher

## TODO: reman self_curriers to st like "only_run_once_check_enabled"
    '$self_curriers',     ## When an expression includes more than one
                          ## foo-or-self axes, it is likely that it
                          ## will curry more than once and the action
                          ## may fire for each time a test is curried.
                          ## So ".//a", for instance, gets curried once
                          ## in the "." (all-nodes-or-self::node())
                          ## and again in the "//" (descendant-or-self::node()),
                          ## so the "a" (child::a) tests end up being executed
                          ## twice.  $self_curriers is used to count the
                          ## number of times an -or-self or union occurs
                          ## and the action code gets wrapped with a
                          ## double-firing preventer in this case.
    '@precursors',        ## The precursors for this rule
    '$predicate_depth',   ## Incremented by the lexer to let us know if
                          ## we're in a predicate or not.  If we're in
                          ## a predicate, we need to evaluate path
                          ## expressions immediately, otherwise they
                          ## become precursors when used as function/oper
                          ## args or are left alone as incremental code
                          ## for the primary expression.
);

###############################################################################
##
## Boolean Singletons
##

## These are not used internally; 1 and 0 are.  These are used when passing
## boolean values in / out to Perl code, so they may be differentiated from
## numeric ones.
sub true()  { \"true"  }
sub false() { \"false" }


###############################################################################
##
## Helpers
##
sub _looks_numeric($)          { $_[0] =~ /^[ \t\r\n]*-?(?:\d+(?:\.\d+)?|\.\d+)[ \t\r\n]*$/ }

sub _looks_literal($)    { $_[0] =~ /^(?:'[^']*'|"[^"]*")(?!\n)\Z/       }

sub _indentomatic() { 1 }  ## Turning this off shaves a little parse time.
sub _indent         { $_[0] =~ s/^/    /mg; }

sub _is_rel_path($) {
    my $path = shift;

    return 0
        if ! $path->isa( "XFD::PathTest" )
            ## TODO: Think through these next three isa() tests;
            ## perhaps we should ## just flag relative vs. absolute
            ## paths when parsed.
            || $path->isa( "XFD::Axis::all_nodes_or_self" )   ## ./... paths
            || $path->isa( "XFD::doc_node" )                  ## /... paths
            || $path->isa( "XFD::Axis::descendant_or_self" )  ## //... paths

            || $path->isa( "XFD::union" ); ## (a|b), union called this already.

    return 1;
}


sub _rel2abs($) {
    ## Converts any XPathTest expression to an absolute one.  This is because
    ## "foo" is equivalent to "./foo" and "//foo" outside of predicates.
    my $path = shift;
    return $path unless _is_rel_path $path;

    ## It's a relative location path, so prefix it with ./ and
    ## convert it to code.
    my $op = XFD::Axis::all_nodes_or_self->new;
    $op->set_next( $path );
    return $op;
}

###############################################################################
##
## expression_as_incr_code

##
## Precursors
## ==========
##
## A precursor is something (so far always a location path sub-expr) that
## (usually) needs to be dealt with before a pattern can be evaluated.
## A precursor is known as "defined" if it's been evaluated and returned
## some result (number, string, boolean, or node).
##
## The only time a pattern can be fully evaluated in the face of undefined
## precursor is when the precursor is supposed to return a node and the
## precursor result is being turned in to a boolean.  Booleans accept
## empty node sets as false values.  Right now, all precursors happen to
## return node sets of 0 or 1 nodes.
##
## The precursor values are stored in $ctx because I'm afraid of leaky
## closure in older perl.  I haven't tested them in this case, but have been
## bit before.
sub expression_as_incr_code {
    my $main_expr_code = shift;

    _indent $main_expr_code if _indentomatic;
    
    my $main_expr_anon_sub_index = $anon_sub_count++;
    $main_expr_code = <<CODE_END;
sub {
    my ( \$d, \$ctx, \$child_ctx ) = \@_;

$main_expr_code}
CODE_END

    return ( $main_expr_code ) unless @precursors;

    $main_expr_code = <<CODE_END;
## The main expression for rule $rule_number, which only
## gets run if all precursors run and become defined.
\$self->{AnonSubs}->[$main_expr_anon_sub_index] ||= $main_expr_code;
CODE_END

    my $precursor_number = 0;

    ## NOTE: node->boolean conversions don't count because an undefined
    ## precursor converts to false().
    ## For other types, the type converters will not set a result unless
    ## they run, and will set a result if they do.  They should never set
    ## an undef result, so testing the precursors for definedness
    ## should be just fine.
    ##
    ## TODO: rethink that to be completly sure.
    my $precursor_gates = join " && ", map
                $precursors[$_]->{NeedType} eq "boolean"
                    ? ()
                    : "\$main_ctx->{Precursors}->[$rule_number]"
                      . " &&"
                      . " defined( \$main_ctx->{Precursors}->[$rule_number]->[$_] )",
            (0..$#precursors);

    my @precursor_codes;
    for my $precursor ( @precursors ) {
##$self_curriers = 10;
        $precursor_gates = "1" unless $precursor_gates;
        my $precursor_setting_code = <<CODE_END;
## rule $rule_number, precursor $precursor_number
\$main_ctx->{Precursors}->[$rule_number]->[$precursor_number] = \$d->{ExpressionResult};
if ( $precursor_gates ) {
    \$d->{AnonSubs}->[$main_expr_anon_sub_index]->( \$d, \$main_ctx, \$child_ctx );
} ## rule $rule_number, precursor $precursor_number
CODE_END

        my $converter_class = "XFD::node2$precursor->{NeedType}_converter";

        die "Can't convert a node set to '$precursor->{NeedType}'"
            . " (no $converter_class found)\n"
            unless $converter_class->can( "new" );

        my $converter= $converter_class->new;
        $converter->set_next( _rel2abs( $precursor->{Expr} ) );

        my $precursor_code = $converter->as_incr_code( \$precursor_setting_code );

        _indent $precursor_code if _indentomatic;

        push @precursor_codes, $precursor_code;
        ++$precursor_number;
    }

    @precursors = ();

    my $precursor_codes = join "", @precursor_codes;

    return join "", , <<CODE_END
## rule $rule_number
$main_expr_code

sub {  # precursors for rule $rule_number
    my ( \$d, \$ctx, \$child_ctx ) = \@_;
$precursor_codes} # precursors for rule $rule_number
CODE_END
}


###############################################################################
##
## PathTest base class
##
## This is used for all of the pieces of location paths axis tests, name
## and type tests, predicates.  It is also used by a few functions
## that act on the result of a location path (which is effectively a
## node set with just the current node in it).
##
sub XFD::PathTest::new {
    my $class = shift;
    return bless [ @_, undef ], $class;
}

sub _next() { -1 }

sub XFD::PathTest::set_next {
    my $self = shift;
    if ( $self->[_next] ) {
        $self->[_next]->set_next( @_ );
    }
    else {
        $self->[_next] = shift;
    }
}

## child:: and descendant-or-self:: axes need to curry to child nodes.
## These tests are the appropriate tests for child nodes.
my %child_curry_tests = qw( EltTests 1 CommentTests 1 PITests 1 TextTests 1 );

## No need for DocTests in this array; we never curry to a doc node event
## because doc node events never occur inside other nodes.
my @all_curry_tests = qw( EltTests CommentTests PITests TextTests AttrTests NSTests );

sub XFD::PathTest::curry_tests {
    ## we assume that there will *always* be a node test after an axis.
    ## This is a property of the grammar.
    my $self = shift;
    my $next = $self->[_next];
    die "$self does not have a next" unless defined $next;
    return $next->curry_tests;
}


## "Incremental code" gets evaluated SAX event by SAX event, currying it's
## results until future events are received if need be.
sub XFD::PathTest::as_incr_code {
    my $self = shift;
    my $action_code = shift;

    my ( $preamble, $postamble ) = split /<NEXT>/, $self->incr_code_template;
    return $preamble unless defined $postamble;

    my $code = defined $self->[_next]
        ? $self->[_next]->as_incr_code( $action_code )
        : $$action_code;

    if ( _indentomatic && $preamble =~ s/( *)(?!\n)\Z// ) {
        _indent $code for 1.. length( $1 ) / 4
    }
    return join $code, $preamble, $postamble;
}


## "Immediate code" gets evaluated as the expression is evaluated, so
## it must perform DOM walking (we have no DOM, other than non-children
## of elt nodes: attrs and namespace nodes).
sub XFD::PathTest::as_immed_code {
    my $self = shift;
    my $action_code = shift;
    my ( $preamble, $postamble ) = split /<NEXT>/, $self->immed_code_template;
    return $preamble unless defined $postamble;

    if ( _indentomatic && $preamble =~ s/( *)(?!\n)\Z// ) {
        $action_code = \"$$action_code";
        _indent $$action_code for 1.. length( $1 ) / 4
    }
    my $code = join $$action_code, $preamble, $postamble;

    return defined $self->[_next]
        ? $self->[_next]->as_immed_code( \$code )
        : $code;

}


sub XFD::PathTest::immed_code_template {
    my $self = shift;
    my $type = ref $self;
    $type =~ s/.*://;

    die "Paths containing $type tests not yet allowed in predicates\n";
}


###############################################################################
##
## Type Converters
##

## NOTE: these do not take/return blessed refs, since they tend to be used
## internally where the result type is known and reffing/dereffing would be
## inconvenient.

## NaN is not handled properly.

sub _boolean2number {
    my $boolean_expr = shift;

    ## Internally, booleans are numeric, force them to 0 or 1.
    return "$boolean_expr ? 1 : 0";
}

sub _boolean2string {
    my $boolean_expr = shift;

    ## Internally, booleans are numeric
    return "( $boolean_expr ? 'true' : 'false' )";
}

## "int" is used internally to get things rounded right.
sub _number2int {
    my $numeric_expr= shift;
    require POSIX;
    return "POSIX::floor( $numeric_expr + 0.5 )";
}

sub _number2string  {
    my $numeric_expr= shift;
    ## The 0+ is to force the scalar in to a numeric format, so to
    ## trim leading zeros.  This will have the side effect that any
    ## numbers that don't "fit" in to the local machine's notion of
    ## a floating point Perl scaler will be munged to the closest
    ## approximation, but hey...
    return
        "do { my \$n = $numeric_expr; ( \$n ne 'NaN' ? 0+\$n : \$n ) }";
}

sub _number2boolean {
    my $numeric_expr= shift;
    return
       "do { my \$n = $numeric_expr; ( \$n ne 'NaN' && \$n ) ? 1 : 0 }";
}

sub _string2boolean {
    my $string_expr= shift;
    return "( length $string_expr ? 1 : 0 )";
}

sub _string2number {
    my $string_expr= shift;

    ## The "0+" forces it to a number, hopefully it was a number
    ## the local machine's perl can represent accurately.
    return qq{do { my \$s = $string_expr; _looks_numeric \$s ? 0+\$s : die "can't convert '\$s' to a number in XPath expression"}};
}

## the any2..._rt are used at runtime when we have no idea at compile time
## about type.  So far, this only happens with variable refs.  Unlike the
## other converters, these are called at runtime.
sub _any2boolean_rt {
    my $any_value = shift;

    my $type = ref $any_value;
    $type =~ s/.*://;
    $type =~ s/ .*//;

    my $value = $$any_value;

    return $value if $type eq "boolean";
    return $value ? 1 : 0 if $type eq "number";
    return length( $value ) ? 1 : 0 if $type eq "string";

    die "Can't convert '$type' to boolean\n";
}

sub _any2boolean { "_any2boolean_rt( $_[0] )" }
##########
sub _any2string_rt {
    my $any_value = shift;

    my $type = ref $any_value;
    $type =~ s/.*://;
    $type =~ s/ .*//;

    my $value = $$any_value;

    return $any_value if $type eq "string";
    return 0+$any_value if $type eq "number";
    if ( $type eq "boolean" ) {
        return ref $value
            ? UNIVERSAL::isa( "XFD::true" )
                ? "true"
                : "false"
            : $value ? "true" : "false";
    }

    die "Can't convert '$type' to string\n";
}

sub _any2string { "_any2string_rt( $_[0] )" }
##########
sub _any2number_rt {
    my $any_value = shift;

    my $type = ref $any_value;
    $type =~ s/.*://;
    $type =~ s/ .*//;

    my $value = $$any_value;

    return 0+$any_value if $type eq "number";

    if ( $type eq "boolean" ) {
        return ref $value
            ? UNIVERSAL::isa( "XFD::true" ) ? 1 : 0
            : $value ? 1 : 0;
    }

    if ( $type eq "string" ) {
        return 0+$value if _looks_numeric $value;
        return "NaN";
    }

    die "Can't convert '$type' to number\n";
}

sub _any2number { "_any2number_rt( $_[0] )" }
################################################################################
##
## Compile-time constant folding
##
## By tracking what values and expressions are constant, we can use
## eval "" to evaluate things at compile time.
##
sub _eval_at_compile_time {
    my ( $type, $code ) = @_;

    return $code unless $fold_constants;

    my $out_code = eval $code;
    die "$@ in XPath compile-time execution of ($type) \"$code\"\n"
        if $@;

    ## Perl's bool ops ret. "" for F
    $out_code = "0"
        if $type eq "boolean" && !length $out_code;
    $out_code = $$out_code if ref $out_code;
    if ( $type eq "string" ) {
        $out_code =~ s/([\\'])/\\$1/g;
        $out_code = "'$out_code'";
    }

    #warn "compiled `$code` to `$out_code`";
    return $out_code;
}

########################################
##
## node2foo_converter classes
##
## These are PathTests because they need to monkey with the action code
## to do the required conversion and stuff the result in ExpressionResult
## so the precursor logic can pick it up and stuff it in the correct
## precursor value slot.
##
## These are only used for grabbing the results of precursors and
## turning them in to scalars so the main expression can run.
##
@XFD::Converter::ISA = qw( XFD::PathTest );

sub XFD::Converter::as_immed_code {
    ## We need to convert from the normal path test semantics to
    ## value-returning semantics.  So, unlike most PathTests'
    ## as_immed_code(), Converters' as_immed_code()s need to
    ## take the result of the path test and return it.  This ends
    ## up behaving a lot like how PathTest::as_incr_code(),
    ## except it calls _next->as_immed_code() on \$ctx.
    my $self = shift;

    my ( $preamble, $postamble ) = split /<NEXT>/, $self->immed_code_template;
    return $preamble unless defined $postamble;

    unless ( $self->[_next] ) {
        my $type = ref $self;
        $type =~ s/.*://;
        die "BUG: $type needs a next!\n"
    }

    my $code = $self->[_next]->as_immed_code( \"( \$ctx )" );

    if ( _indentomatic && $preamble =~ s/( *)(?!\n)\Z// ) {
        _indent $code for 1.. length( $1 ) / 4
    }

    return join $code, $preamble, $postamble;
}


@XFD::node2string_converter::ISA = qw( XFD::Converter );

sub XFD::node2string_converter::as_incr_code {
    my $self = shift;

    ## We need to run some code as the action, some of
    ## which calls the real action.  So, fool SUPER::as_incr_code
    ## in to thinking we're at the end of the chain for a
    ## moment and get it to build our action for us,
    ## then pass that down the chain to get the real
    ## action.
    my $fake_action_code = do {
        local $self->[_next] = undef;
        package XFD::node2string_converter;
        $self->SUPER::as_incr_code( @_ );
    };

    return $self->[_next]->as_incr_code( \$fake_action_code );
}

sub XFD::node2string_converter::incr_code_template {
    my $self = shift;

    ## This is actually passed down the chain as the action code,
    ## with the *real* action code already subbed in to the
    ## <NEXT>. (normally the next test's output would contain
    ## the action and go in <NEXT>).
    return <<CODE_END;
if ( \$ctx->{IsStartEvent} ) {
    if ( \$ctx->{NodeType} ne "attribute" ) {
        ## Flag the child context so all descendant
        ## text is accumulated.
        \$child_ctx->{Text} = "" unless exists \$child_ctx->{Text};
    }
}

if ( \$ctx->{IsEndEvent} ) {
    \$d->{ExpressionResult} = ( \$ctx->{NodeType} eq "attribute" )
        ? \$ctx->{Node}->{Value}
        : \$child_ctx->{Text};
    <NEXT>
}
CODE_END
}

## TODO: Someday we won't be able to assume the dern nodesets are
## in document order
## TODO: Don't assume that this is an attribute here.
sub XFD::node2string_converter::immed_code_template {
    return <<CODE_END;
do { ## node2string
    my \@ctxs = (
        <NEXT>
    );
    \@ctxs && exists \$ctxs[0]->{Node}->{Value}
        ? \$ctxs[0]->{Node}->{Value}
        : "";
} # node2string
CODE_END
}

##########
@XFD::node2number_converter::ISA = qw( XFD::Converter );

sub XFD::node2number_converter::new {
    my $self = shift->XFD::PathTest::new( @_ );
    $self->[_next] = XFD::node2string_converter->new;
    return $self;
}


sub XFD::node2number_converter::as_incr_code {
    my $self = shift;

    ## We need to run some code as the action, some of
    ## which calls the real action.  So, fool SUPER::as_incr_code
    ## in to thinking we're at the end of the chain for a
    ## moment and get it to build our action for us,
    ## then pass that down the chain to get the real
    ## action.
    my $fake_action_code = do {
        local $self->[_next] = undef;
        package XFD::node2number_converter;
        $self->SUPER::as_incr_code( @_ );
    };

    return $self->[_next]->as_incr_code( \$fake_action_code );
}

sub XFD::node2number_converter::incr_code_template {
    ## This is actually passed down to the node2string_converter
    ## as the action code with the real action code where <NEXT> is.
    ## Thuse the string conversion runs first
    ## and sets the ExpressionResult, then this bit runs and converts the
    ## result.
    return <<CODE_END;
{ ## node2number
    \$d->{ExpressionResult} = _looks_numeric \$d->{ExpressionResult}
        ? 0 + \$d->{ExpressionResult}
        : "NaN";  ## TODO: work better with Perl's "real" NaN support?
    <NEXT>
} # node2number
CODE_END
}


sub XFD::node2number_converter::immed_code_template {
    return <<CODE_END;
do { ## node2number
    my \$string = (
        <NEXT>
    );
    _looks_numeric \$string
        ? 0 + \$string
        : "NaN";  ## TODO: work better with Perl's "real" NaN support?
} # node2number
CODE_END
}


##########
@XFD::node2boolean_converter::ISA = qw( XFD::Converter );

sub XFD::node2boolean_converter::as_incr_code {
    my $self = shift;

    ## We need to run some code as the action, some of
    ## which calls the real action.  So, fool SUPER::as_incr_code
    ## in to thinking we're at the end of the chain for a
    ## moment and get it to build our action for us,
    ## then pass that down the chain to get the real
    ## action.
    my $fake_action_code = do {
        local $self->[_next] = undef;
        package XFD::node2boolean_converter;
        $self->SUPER::as_incr_code( @_ );
    };

    return $self->[_next]->as_incr_code( \$fake_action_code );
}

sub XFD::node2boolean_converter::incr_code_template {
    my $self = shift;

    ## This is actually passed down the chain as the action code,
    ## so the *real* action code goes in <NEXT>, then the result
    ## of this template is is passed off as the action.
    return <<CODE_END;
if ( \$ctx->{IsEndEvent} ) {
    ## It's got to be true if we reached here...
    \$d->{ExpressionResult} = 1;
    <NEXT>
}
CODE_END
}


sub XFD::node2boolean_converter::immed_code_template {
    return <<CODE_END;
(
    scalar( ## node2boolean
        <NEXT>
    ) ? 1 : 0
) # node2boolean
CODE_END
}


###############################################################################
##
## Misc
##


## When the root op is an expression or scalar, we need to save the
## result and fire the action iff the result is true.
##
sub expr_eval($$) {
    my $expr_code    = ${$_[0]};
    my ( $expr_returns, $is_constant ) = split / /, ref $_[0];

    my $next_code    = ${$_[1]};

    my $expr_test = "\$d->{ExpressionResult}";
    my @expr_test = ( bless \$expr_test, $expr_returns );

    ## TODO: factor the type conversion logic out of _munge_parms().
    _munge_parms( 1, 1, "expr_eval", \@expr_test, "boolean" );
    $expr_test = ${$expr_test[0]};

    if ( $expr_returns eq "boolean" ) {
        $expr_code = "$expr_code ? true : false";
        $expr_test = "$expr_test != false";
    }

    _indent $next_code if _indentomatic;

    return \<<CODE_END;
## expr_eval
\$d->{ExpressionResult} = $expr_code;  ## $expr_returns
if ( $expr_test ) {
$next_code}
CODE_END
}

##########
## TODO: Consider making already_ran an ARRAY in $d.
my $action_id = 0;
sub action {
    my $code = ${$_[0]};
    $code = "## NO ACTION (undef)\n" unless defined $code;
    _indent $code if _indentomatic;

    my $i = $action_id++;

    my @gates;
    push @gates, "\$ctx->{IsStartEvent} "
        unless $has_start_or_end;
    push @gates, "! \$XFD::already_ran{$i}++ "
        if $self_curriers > 1;

    my $gates = join "\n    && ", @gates;
    $gates .= "\n" if _indentomatic && @gates > 1;

    return @gates ? \<<GATED : \<<ALWAYS_DO;
## action
if ( $gates) {
$code} # action

GATED
{ # action
$code} # action

ALWAYS_DO
}

##########
###############################################################################
##
## Functions
##

## Boolean functions return 0 or 1, it's up to expr_eval (or any
## other code which passes these in or out to perl code) to convert
## these to true() or false().  Passing these in/out of this subsystem
## is far rarer than using them within it. Using 1 or 0 lets
## the optimization code and the generated use numeric or boolean
## Perl ops.

## When compiled, expressions and function calls are turned in to
## Perl code right in the grammar, with a vew exceptions like
## string( node-set ).

## The parser calls this
sub function {
    my ( $name, $args ) = @_;
    
    no strict 'refs';

    ## prevent ppl from spelling then "foo_bar"
    my $real_name = $name;
    my $had_underscores = $real_name =~ s/_/*NO-UNDERSCORES-PLEASE*/g;
    $real_name =~ s/-/_/g;

    unless ( defined &{"XFD::Function::$real_name"} ) {
        if ( $had_underscores ) {
            if ( $had_underscores ) {
                $real_name = $name;
                $real_name =~ s/-/_/g;
                if ( defined &{"XFD::Function::$real_name"} ) {
                    die
                "XPath function mispelled, use '-' instead of '_' in '$name'\n";
                }
            }
        }
        die "unknown XPath function '$name'\n";
    }

    return "XFD::Function::$real_name"->( $args );
}


## Helper subs
##
## NOTE: _munge_parms is where all the precursor magic is initiated.
##
sub _munge_parms {
    ## police parms and convert to appropriate type as needed.
    my ( $min, $max, $sub_name, $args, @types ) = @_;

    croak "$min > $max!!" if defined $max && $min > $max;

    my $msg;
    my $cnt = scalar @$args;
    if ( defined $max && $max == $min ) {
        $msg = "takes $min parameters" unless $cnt == $min;
    }
    elsif ( $cnt < $min ) {
        $msg = "takes at least $min parameters";
    }
    elsif ( defined $max && $cnt > $max ) {
        $msg = "takes at most $max parameters";
    }

    my @errors;
    push @errors, "$sub_name() $msg, got $cnt\n"
        if defined $msg;
    my $num = 1;
    my @is_constant;
    for ( @$args ) {
        my $out_type = $types[0];

        if ( $_->isa( "XFD::PathTest" ) ) {
            if (
                _is_rel_path $_
                && ( $predicate_depth
                    || $_->isa( "XFD::Axis::attribute" )
                )
            ) {
                ## Predicates need their paths evaluated in-place instead
                ## of incrementally.

                my $converter_class = "XFD::node2${out_type}_converter";

                die "Can't convert a node set to '${out_type}'"
                    . " (no $converter_class found)\n"
                    unless $converter_class->can( "new" );

                my $converter= $converter_class->new;
                $converter->set_next( $_ );

                my $expr_code = $converter->as_immed_code;
                _indent $expr_code if _indentomatic;
                $_ = bless \$expr_code, $out_type;
            }
            else {
                ## AHA! an expression we need to use as a precursor
                ## shove it off to the side along with the type we need.
                ## the type is important because the expression will
                ## return a node set of 0 or 1 nodes, and the boolean
                ## types treat undef as false; while number and string
                ## treat them as "can't calculate this" signals.

                ## Precursors need to be evaluated in the end_element
                ## event most often.
                $has_start_or_end = 1;

                push @precursors, {
                    NeedType => $out_type,
                    Expr     => $_,
                };
                my $precursor_number = $#precursors;
                my $code =
                    "\$ctx->{Precursors}->[$rule_number]->[$precursor_number]";

                $_ = bless \$code, $out_type;
            }
            push @is_constant, 0;
            next;
        }

        my ( $type, $is_constant ) = split /( .+)/, ref;
        push @is_constant, $is_constant;

        if ( $type ne $out_type ) {
            no strict "refs";
            my $cvt = "_${type}2$out_type";
            if ( defined &$cvt ) {
                my $code = $cvt->( $$_ );

                if ( $is_constant ) {
                    $code = _eval_at_compile_time $out_type, $code;
                    $out_type .= $is_constant;
                }

                $_ = bless \$code, $out_type;
            }
            else {
                push @errors,
                    "Can't convert ",
                    $type,
                    " to ",
                    $types[0],
                    " in ",
                    $sub_name,
                    "() parameter ",
                    $num,
                    "\n"
            }
        }
        ++$num;
        shift @types if @types > 1;
    }

    die @errors if @errors;
    return wantarray ? @is_constant : ! grep ! $_, @is_constant;
}



sub _takes_no_parms{
     unshift @_, 0, 0;
     goto &_munge_parms;
}

## Sometimes a function call can be run at compile-time.  This normally
## happens when all of it's inputs are constants.  _build_expr detects
## that and does it.  This will probably only be of great benefit for
## XPath exprs that are automatically compiled.
##
sub _build_expr {
    my $code_sub = pop;
    my $type     = pop;
    my $all_args_are_constant = _munge_parms @_;
    my $code = $code_sub->( map $$_, @{$_[3]} );
    if ( $all_args_are_constant ) {
        $code = _eval_at_compile_time $type, $code;
        $type .= " constant";
    }
    return bless \$code, $type;
}

##########
sub XFD::Function::boolean{
    _build_expr 1, 1, "boolean", shift, "boolean",
        "boolean", sub { $_[0] };
}
##########
sub XFD::Function::ceiling {
    require POSIX;
    _build_expr 1, 1, "ceiling", shift, "number",
        "number", sub {"POSIX::ceil( $_[0] )"};
}
##########
sub XFD::Function::concat {
    _build_expr 2, undef, "concat", shift, "string",
        "string", sub {
            "join( '', " . join( ", ", @_ ) . " )";
        };
}
##########
sub XFD::Function::contains {
    _build_expr 2, 2, "contains", shift, "string",
        "boolean", sub {
            "0 <= index( " . join( ", ", @_ ) . " )";
        };
}
##########
{
    my $end_code = "!\$ctx->{IsStartEvent}";

    sub XFD::Function::is_end_event {   ## An XFD-only (ie non-XPath) function
        _takes_no_parms "end_event", @_;
        $has_start_or_end = 1;
        return bless \$end_code, "boolean";
    }
}
##########
{
    my $false_code = "0";

    sub XFD::Function::false {
        _takes_no_parms "false", @_;
        return bless \$false_code, "boolean constant";
    }
}
##########
sub XFD::Function::floor {
    require POSIX;
    _build_expr 1, 1, "floor", shift, "number",
        "number", sub {"POSIX::floor( $_[0] )"};
}
##########
sub XFD::Function::normalize_space {
    ## We don't do the argless version because we can't for all nodes, since
    ## that would require keeping entire subtrees around.  We might be
    ## able to do it for attributes and leaf elements, throwing an error
    ## at runtime if the node is not a leaf node.
    my ( $args ) = @_;
    _munge_parms 1, 1, "normalize-space", $args, "string";

    my $code = "do { my \$s = ${$args->[0]}; \$s =~ s/^[ \\t\\r\\n]+//; \$s =~ s/[ \\t\\r\\n]+(?!\\n)\\Z//; \$s =~ s/[ \\t\\r\\n]+/ /g; \$s }";
    return bless \$code, "string";

}
##########
sub XFD::Function::not {
    _build_expr 1, 1, "not", shift, "boolean",
        "boolean", sub { "! $_[0]" }
}
##########
sub XFD::Function::number{
    _build_expr 1, 1, "number", shift, "number",
        "number", sub { $_[0] };
}
##########
sub XFD::Function::round {
    require POSIX;
    ## Expressly ignoring the -0 conditions in the spec.
    _build_expr 1, 1, "round", shift, "number",
        "number", sub {"POSIX::floor( $_[0] + 0.5 )"};
}
##########
{
    my $start_code = "\$ctx->{IsStartEvent}";

    sub XFD::Function::is_start_event {   ## An XFD-only (ie non-XPath) function
        _takes_no_parms "start_event", @_;
        $has_start_or_end = 1;
        return bless \$start_code, "boolean";
    }
}
##########
sub XFD::Function::starts_with {
    _build_expr 2, 2, "starts-with", shift, "string",
        "boolean", sub {
            "0 == index( " . join( ", ", @_ ) . " )";
        };
}
##########
sub XFD::Function::string{
    _build_expr 1, 1, "string", shift, "string",
        "string", sub { $_[0] };
}

##########
sub XFD::Function::string_length {
    ## We don't do string-length() because we can't for all nodes, since
    ## that would require keeping entire subtrees around.  We might be
    ## able to do it for attributes and leaf elements, throwing an error
    ## at runtime if the node is not a leaf node.
    _build_expr 1, 1, "string-length", shift, "string",
        "string", sub { "length( $_[0] )" };
}
##########
sub XFD::Function::substring {
    my ( $args ) = @_;
    my $all_constant = _munge_parms 2, 3, "substring", $args, "string", "int";
    my @args = map $$_, @$args;

    my @is_constant = map 0 <= index( ref, "constant" ), @$args;

    my $code;
    if ( @$args == 2 ) {
        my $pos_code =
            "do { my \$pos = $args[1] - 1; \$pos = 0 if \$pos < 0; \$pos}";

        $pos_code = _eval_at_compile_time "number", $pos_code
            if $is_constant[1];
        $code = "substr( $args[0], $pos_code )";
    }
    else {
        ## must be 3 arg form.
        my $pos_len_code =
            "do { my ( \$pos, \$len ) = ( $args[1] - 1, ${$args->[2]} ); my \$end = \$pos + \$len; \$pos = 0 if \$pos < 0; \$len = \$end - \$pos ; \$len = 0 if \$len < 0; ( \$pos, \$len ) }";

        ## Not bothering to optimize the substring( <whatever>, <const>, <var> )
        ## situation, only substring( <whatever>, <const>, <const> )

        if ( $is_constant[1] && $is_constant[2] ) {
            my ( $pos, $len ) = eval $pos_len_code
                or die "$! executing XPath (number, number) $pos_len_code at compile time\n";
            $code = "substr( $args[0], $pos, $len )" ;
        }
        else {
            $code = "substr( $args[0], $pos_len_code )" ;
        }
    }

    $code = _eval_at_compile_time "string", $code
        if $all_constant;

    return bless \$code, "string";

}
##########
sub XFD::Function::substring_after {
    _build_expr 2, 2, "substring-after", shift, "string",
        "string", sub {
            "do { my ( \$s, \$ss ) = ( $_[0], $_[1] ); my \$pos = index \$s, \$ss; \$pos >= 0 ? substr \$s, \$pos + length \$ss : ''  }";
        };
}
##########
sub XFD::Function::substring_before {
    _build_expr 2, 2, "substring-before", shift, "string",
        "string", sub {
            "do { my \$s = $_[0]; my \$pos = index \$s, $_[1]; \$pos >= 0 ? substr \$s, 0, \$pos : ''  }";
        };
}
##########
sub XFD::Function::translate {
    ## We don't implement the argless version because we can't for all nodes, since
    ## that would require keeping entire subtrees around.  We might be
    ## able to do it for attributes and leaf elements, throwing an error
    ## at runtime if the node is not a leaf node.

    ## TODO: verify that quotemeta is really enough and is correct, here.

    ## We don't handle the case where only one of $from and $to is constant, should
    ## be rare (hell, just seeing translate() anywhere should be rare).  This was
    ## not true for substring() above, which I suspect will be called a bit more
    ## than translate().
    _build_expr 3, 3, "translate", shift, "string",
        "string", sub {
            "do { my ( \$s, \$f, \$t ) = ( $_[0], quotemeta $_[1], quotemeta $_[2] ); eval qq{\\\$s =~ tr/\$f/\$t/d}; \$s }";
        };
}
##########
{
    my $true_code = "1";

    sub XFD::Function::true {
        _takes_no_parms "true", @_;
        return bless \$true_code, "boolean constant";
    }
}

###############################################################################
##
## Variable references
##
sub get_var {
    my ( $var_name ) = shift;
    my $code = "\$d->_look_up_var( '$var_name' )";
    bless \$code, "any";
}

###############################################################################
##
## Operators (other than Union)
##
sub _relational_op_type {
    my $foo = ref( $_[0]->[0] ) . "|" . ref( $_[0]->[1] );
    for (qw( boolean number string )) {
        return $_ if 0 <= index $foo, $_;
    }
    die "Couldn't discern a parameter type in $foo";
}

sub _relational_op {
    my ( $name, $numeric_op, $non_numeric_op ) = (shift, shift, shift);
    my $op_type = _relational_op_type @_;
    my $op = $op_type ne "string" ? $numeric_op : $non_numeric_op;
    _build_expr 2, 2, $name, @_, $op_type,
        "boolean", sub { "( $_[0] $op $_[1] )" }
}

sub _math_op {
    my ( $name, $op ) = (shift, shift);
    _build_expr 2, 2, $name, @_, "number",
        "number", sub { "( $_[0] $op $_[1] )" }
}

##########
sub parens {
    my ( $op_type ) = split / /, ref $_[0];
    _build_expr 1, 1, "(...)", [shift], $op_type,
        $op_type, sub { "( $_[0] )" }
}
##########
sub negation {
    my ( $op_type ) = split / /, ref $_[0];
    _build_expr 1, 1, "(...)", [shift], "number",
        "number", sub { "do { my \$n = $_[0]; \$n eq 'NaN' ? \$n : 0-\$n }" }
}
##########
sub and {
    _build_expr 2, 2, "and", shift, "boolean",
        "boolean", sub { "( $_[0] and $_[1] )" }
}
##########
sub or {
    _build_expr 2, 2, "or", shift, "boolean",
        "boolean", sub { "( $_[0] or $_[1] )" }
}
##########
##
## Relational ops
##
sub equals        { _relational_op  "=", "==", "eq", shift }
sub not_equals    { _relational_op "!=", "!=", "ne", shift }
sub lt            { _relational_op "<",  "<",  "lt", shift }
sub lte           { _relational_op "<=", "<=", "le", shift }
sub gt            { _relational_op ">",  ">",  "gt", shift }
sub gte           { _relational_op ">=", ">=", "ge", shift }
##########
sub addition       { _math_op "mod", "+", shift }
sub subtraction    { _math_op "-",   "-", shift }
sub multiplication { _math_op "+",   "*", shift }
sub division       { _math_op "div", "/", shift }
sub modulus        { _math_op "mod", "%", shift }

###############################################################################
##
## Location Path Tests
##
## As the
## grammar parses a location path, it stringse these objects together.
## When a location path has been completely assembed, the objects
## are converted in to code.  This is necessary because paths are recognized
## from left to right by the grammar, but need to be assembled right
## to left.  We could use closure to accomplish this, but closures leak
## in different ways in different perls.
##
@XFD::doc_node::ISA = qw( XFD::PathTest );

sub XFD::doc_node::incr_code_template {
    my $self = shift;
    return <<CODE_END;
{ # doc_node
my \$main_ctx = \$ctx; ## Hack to get precursorized exprs to use the correct ctx.
    <NEXT>
} # end doc_node
CODE_END
}


##########
@XFD::self_node::ISA = qw( XFD::PathTest );

sub XFD::self_node::curry_tests {
    my $self = shift;

    ## If this is *not* a standalone, then it's basically a noop, so
    ## delegate to the next test.
    return $self->[_next]->curry_tests
        if defined $self->[_next];

    ## Otherwise, return every node test type.  This happens when '.'
    ## is the entire location path
    @all_curry_tests;
}

sub XFD::self_node::incr_code_template { "<NEXT>" }


##########
@XFD::node_name::ISA = qw( XFD::PathTest );

sub XFD::node_name::curry_tests { qw( EltTests AttrTests ) }

sub XFD::node_name::incr_code_template {
    my $self = shift;
    return <<CODE_END;
## node name '$self->[0]'
if ( exists \$ctx->{Node}->{Name} && \$ctx->{Node}->{Name} eq '$self->[0]' ) {
    <NEXT>
} # node name '$self->[0]'
CODE_END
}

sub XFD::node_name::immed_code_template {
    my $self = shift;
    return <<CODE_END;
grep(  ## node name '$self->[0]'
    exists \$_->{Node}->{Name} && \$_->{Node}->{Name} eq '$self->[0]',
    <NEXT>
) # node name '$self->[0]'
CODE_END
}

##########
@XFD::any_node_name::ISA = qw( XFD::PathTest );

sub XFD::any_node_name::curry_tests { qw( EltTests AttrTests ) }

sub XFD::any_node_name::incr_code_template { <<CODE_END }
## any node name
if ( \$ctx->{NodeType} eq "element" || \$ctx->{NodeType} eq "attribute" ) {
    <NEXT>
} # any node name
CODE_END

##########
@XFD::union::ISA = qw( XFD::PathTest );

sub XFD::union::new {
    my $class = shift;
    ## No _next, so don't use base class' new().
    return bless [@_], $class;
}

sub XFD::union::add { push @{shift()}, @_ }

## TODO: Don't rel2abs here, meaning predicatize it.
sub XFD::union::as_incr_code {
    my $self = shift;

    return join "",
        map( {
            $_->isa( "XFD::PathTest" )
               ? ( "# union\n", _rel2abs( $_ )->as_incr_code( @_ ) )
               : die
"XPath's union operator ('|') doesn't work on a ", ref $_, ", perhaps 'or' is needed.\n";
        } @$self
    ), "# end union\n" ;
}

##########
@XFD::predicate::ISA = qw( XFD::PathTest );

## new() might get a location path passed as a param, or find them in the
## predicates.  The former happens when st. like [@foo] occurs, the latter
## when [@foo=2] occurs (since the "=" converted @foo to a predicate).

sub _expr() { 0 }

sub XFD::predicate::curry_tests {
    my $self = shift;

    ## If there's something after us, delegate.
    return $self->[_next]->curry_tests
        if defined $self->[_next];

    ## Otherwise, return every node test type.
    @all_curry_tests;
}

sub XFD::predicate::incr_code_template {
    my $self = shift;
    my $expr = $self->[_expr];

    local $predicate_depth = $predicate_depth + 1;

    unless ( defined $expr ) {
        ## Must have been precursor-ized, so the action will
        ## be run according to the predicates...
        return "<NEXT>";
    }

    my $pred_expr = _build_expr 1, 1, "boolean", [ $expr ], "boolean",
        "boolean", sub { shift };

    if ( $fold_constants && 0 <= index ref $pred_expr, "constant" ) {
        if ( $$pred_expr eq "0" ) {
            ## TODO: optimize away the entire rule if there are no
            ## functions with side effects (which includes all
            ## internal functions).
            return "## Failing predicate\n"
        }
        return "## Passing predicate $pred_expr\n<NEXT>"
    }

    return <<CODE_END;
# predicate
if (
$$pred_expr
) {
    <NEXT>} # predicate
CODE_END
}

###############################################################################
##
## Axes
##

##
## The grammar calls axis(), which returns an object.
##
sub axis {
    my $axis_name = shift;
    $axis_name =~ s/_/<UNDERSCORE_NOT_ALLOWED>/g;
    $axis_name =~ s/-/_/g;
    return "XFD::Axis::$axis_name"->new( @_ );
}

@XFD::Axis::ISA = qw( XFD::PathTest );

##########
@XFD::Axis::attribute::ISA = qw( XFD::Axis );

sub XFD::Axis::attribute::curry_tests { ( "EltTests" ) }

sub XFD::Axis::attribute::incr_code_template {
    my $self = shift;

    ## node type tests only apply to certain event types, so
    ## we only curry to those events.  This makes NodeType
    ## tests run-time noops, and simplifies others (node_name)
    ## because they do not need to test $ctx->{NodeType}.
    my @curry_tests =
        grep $_ eq "AttrTests", $self->[_next]->curry_tests;

    ## TODO: Warn about impossible to satisfy test unless @curry_tests.

    my $curry_code = join "\n",
        map "    push \@{\$child_ctx->{$_}}, \$next;",
            @curry_tests;

    return <<CODE_END;
## attribute::
{
    ## Curry the remainder of the tests
    ## to be handled in the appropriate
    ## attribute events.
    my \$next = sub {
        my ( \$d, \$ctx, \$child_ctx ) = \@_;
        <NEXT>
    };
$curry_code
} # attribute::
CODE_END
}


sub XFD::Axis::attribute::immed_code_template {
    return <<CODE_END;
( ## attribute::
    exists \$ctx->{Node}->{Attributes} && \$ctx->{Node}->{Attributes}
    ? map {
        my \$ctx = \$_;
        map {
            {
                NodeType => 'attribute',
                Node     => \$_,
                Parent   => \$ctx,
            };
        } values %{\$ctx->{Node}->{Attributes}}
    } <NEXT>
    : ()
) # attribute::
CODE_END
}


##########
@XFD::Axis::child::ISA = qw( XFD::Axis );

sub XFD::Axis::child::incr_code_template {
    my $self = shift;

    ## node type tests only apply to certain event types, so
    ## we only curry to those events.  This makes NodeType
    ## tests run-time noops, and simplifies others (node_name)
    ## because they do not need to test $ctx->{NodeType}.
    my @curry_tests =
        grep exists $child_curry_tests{$_}, $self->[_next]->curry_tests;

    my $curry_code = join "\n",
        map "    push \@{\$child_ctx->{$_}}, \$next;",
            @curry_tests;

    return <<CODE_END;
## child::
{
    ## Curry the remainder of the tests
    ## to be handled in the appropriate
    ## child events.
    my \$next = sub {
        my ( \$d, \$ctx, \$child_ctx ) = \@_;
        <NEXT>
    };
$curry_code
} # child::
CODE_END
}


##########
@XFD::Axis::descendant_or_self::ISA = qw( XFD::Axis );

sub XFD::Axis::descendant_or_self::new {
    ++$self_curriers;
    package XFD::Axis::descendant_or_self;
        shift->SUPER::new( @_ );
}

sub XFD::Axis::descendant_or_self::incr_code_template {
    my $self = shift;

    ## node type tests only apply to certain event types, so
    ## we only curry to those events.  This makes NodeType
    ## tests run-time noops, and simplifies others (node_name)
    ## because they do not need to test $ctx->{NodeType}.
    ## We always curry to EltTests to propogate the tests down
    ## the chain.  Unfortunately, this 
    my @curry_tests =
        grep exists $child_curry_tests{$_},
            $self->[_next]->curry_tests;

    return <<CODE_END;
## descendant-or-self::
{
    my \$sub = sub {
        my ( \$d, \$ctx, \$child_ctx ) = \@_;
        <NEXT>
    };

    ## Curry this action to the appropriate
    ## descendant events, and...
    push \@{\$child_ctx->{DescendantTests}}, \$sub;

    ## run it on the context node (aka "self")
    ## Can't use goto &\$sub because we need to fall
    ## thru for unions
    \$sub->( \@_ );
} # descendant-or-self::
CODE_END
}


##########
@XFD::Axis::self::ISA = qw( XFD::Axis );

sub XFD::Axis::self::incr_code_template { "<NEXT>" }

##########
##
## all_nodes_or_self is a pseudo-axis not available from XPath expressions.
## It's used to wrap XPath expressions that are returned from the
## parsing (ie not location paths).  As such, unlike other axes, it
## can be passed all of the code that is to be evaluated.
##
@XFD::Axis::all_nodes_or_self::ISA = qw( XFD::Axis );

sub XFD::Axis::all_nodes_or_self::new {
    ++$self_curriers;
    package XFD::Axis::all_nodes_or_self;
        shift->SUPER::new( @_ );
}

sub XFD::Axis::all_nodes_or_self::incr_code_template {
    my $self = shift;

    ## node type tests only apply to certain event types, so
    ## we only curry to those events.  This makes NodeType()
    ## tests run-time noops, and simplifies others (node_name)
    ## because they do not need to test $ctx->{NodeType}.
    my @curry_tests = (
        "EltTests",
        grep $_ ne "EltTests", $self->[_next]->curry_tests
    );

    my $curry_code = join "\n",
        map "        push \@{\$child_ctx->{$_}}, \$sub;",
            @curry_tests;

    return <<CODE_END;
## all-nodes-or-self:: (pseudo-axis for '.' steps)
{
    my \$sub;
    \$sub = sub {
        my ( \$d, \$ctx, \$child_ctx ) = \@_;

my \$main_ctx = \$ctx; ## Hack to get precursorized exprs to use the correct ctx.

        ## Curry this action to the appropriate
        ## descendant events, and...
$curry_code

        ## ...test this node (self).
        <NEXT>
    };

    ## run it on the context node (aka "self")
    ## Can't use goto &\$sub because we need to fall
    ## thru for unions
    \$sub->( \@_ );
} # all-nodes-or-self::
CODE_END
}


## This special API is called by the parser when the grammar
## returns an expression.
sub all_nodes_or_self($) {
    my $next_code = ${$_[0]};

    return \"" unless length $next_code;

    _indent $next_code if _indentomatic;

    ## We bend over backwards a bit to avoid using a closure by
    ## making the self-propogating sub a plain old anon sub.
    ## TODO: compile the anon sub out here.  Not doing that now because
    ## this makes debugging easier.

    my $i = $anon_sub_count++;
    return \<<CODE_END;
## all-nodes-or-self (an internal-use-only axis not available via XPath)
\$d->{AnonSubs}->[$i] ||= sub {
    my ( \$d, \$ctx, \$child_ctx ) = \@_;

    ## Enqueue this function to be applied to all 
    ## appropriate child nodes.
    push \@{\$child_ctx->{EltTests}},     \$d->{AnonSubs}->[$i];
    push \@{\$child_ctx->{CommentTests}}, \$d->{AnonSubs}->[$i];
    push \@{\$child_ctx->{PITests}},      \$d->{AnonSubs}->[$i];
    push \@{\$child_ctx->{TextTests}},    \$d->{AnonSubs}->[$i];
    push \@{\$child_ctx->{AttrTests}},    \$d->{AnonSubs}->[$i];

$next_code};

goto &{\$d->{AnonSubs}->[$i]};
CODE_END
}
###############################################################################
##
## Node Type Tests
##
@XFD::NodeType::ISA = qw( XFD::PathTest );

## These "tests" all work by only getting curried to the appropriate
## event types :).
sub XFD::NodeType::incr_code_template { "<NEXT>" }

##########
@XFD::NodeType::node::ISA = qw( XFD::NodeType );

## This is a pass-through.
sub XFD::NodeType::node::curry_tests {
    my $self = shift;
    return $self->[_next]->curry_tests
        if defined $self->[_next];

    return @all_curry_tests;
}

##########
@XFD::NodeType::text::ISA = qw( XFD::NodeType );
sub XFD::NodeType::text::curry_tests { "TextTests" }
##########
@XFD::NodeType::comment::ISA = qw( XFD::NodeType );
sub XFD::NodeType::comment::curry_tests { "CommentTests" }
##########
@XFD::NodeType::processing_instruction::ISA = qw( XFD::NodeType );
sub XFD::NodeType::processing_instruction::curry_tests { "PITests" }
##########

1;

package XML::Filter::Dispatcher ;

=head1 NAME

XML::Filter::Dispatcher - Path based event dispatching with DOM support

=head1 SYNOPSIS

    use XML::Filter::Dispatcher qw( :all );

    my $f = XML::Filter::Dispatcher->new(
        Rules => [
           'foo'            => \&handle_foo_start_tag,
           '@bar'           => \&handle_bar_attr_at_start_tag,

           ## Send any <foo> elts and their contents to $handler
           'foo'            => $handler,

           ## Print the text of all <description> elements
           'string( description )' => sub { print xresult },
       ],
       Vars => [
           "id" => [ string => "12a" ],
       ],
    );

=head1 DESCRIPTION

B<WARNING>: Beta code alert.

A SAX2 filter that dispatches SAX events based on "EventPath" patterns
as the SAX events arrive.  The SAX events are not buffered or converted
to an in-memory document representation like a DOM tree.  This provides
for low lag operation because the actions associated with each pattern
are executed as soon as possible, usually in an element's
C<start_element()> event method.

This differs from traditional XML pattern matching tools like
XPath and XSLT (which is XPath-based) which require the entire
document to be built in memory (as a "DOM tree") before queries can be
executed.  In SAX terms, this means that they have to build a DOM tree
from SAX events and delay pattern matching until the C<end_document()>
event method is called.

=head2 Rules

A rule is composed of a pattern and an action.  Each
XML::Filter::Dispatcher instance has a list of rules; all rules with
patterns that match a particular SAX event fire their actions when that
SAX event is received.

=head2 Patterns

Note: this section describes EventPath and discusses differences between
EventPath and XPath.  If you are not familiar with XPath you may want
to skim those bits; they're provided for the benefit of people coming
from an XPath background but hopefully don't hinder others.  A working
knowledge of SAX is necessary for the advanced bits.

EventPath patterns may match the document, elements, attributes, text
nodes, comments, processing instructions, and (not yet implemented)
namespace nodes.  Patterns like this are referred to as "location paths"
and resemble Unix file paths or URIs in appearance and functionality.

Location paths describe a location (or set of locations) in the document
much the same way a filespec describes a location in a filesystem.  The
path C</a/b/c> could refer to a directory named C<c> on a filesystem or
a set of C<e<lt>cE<gt>> elements in an XML document.  In either case,
the path indicates that C<c> must be a child of C<b>, C<b> must be
<a>'s, and <a> is a root level entity.  More examples later.

EventPath patterns may also extract strings, numbers and boolean values
from a document.  These are called "expression patterns" and are only
said to match when the values they extract are "true" according to XPath
semantics (XPath truth-ness differs from Perl truth-ness, see
L<EventPath Truth|EventPath Truth> below).  Expression patterns look
like C<string( /a/b/c )> or C<number( part-number )>, and if the result
is true, the action will be executed and the result can be retrieved
using the L<xresult|xresult> method.

TODO: rename xresult to be ep_result or something.

We cover patterns in more detail below, starting with some examples.

If you'd like to get some experience with pattern matching in an
interactive XPath web site, there's a really good XPath/XSLT based
tutorial and lab at
L<http://www.zvon.org/xxl/XPathTutorial/General/examples.html|http://www.zvon.org/xxl/XPathTutorial/General/examples.html>.

=head2 Actions

Two kinds of actions are supported: Perl subroutine calls and
dispatching events to other SAX processors.  When a pattern matches, the
associated action

=head2 Examples

This is perhaps best introduced by some examples.  Here's a routine that runs a
rather knuckleheaded document through a dispatcher:

    use XML::SAX::Machines qw( Pipeline );

    sub run { Pipeline( shift )->parse_string( <<XML_END ) }
      <stooges>
        <stooge name="Moe" hairstyle="bowl cut">
          <attitude>Bully</attitude>
        </stooge>
        <stooge name="Shemp" hairstyle="mop">
          <attitude>Klutz</attitude>
          <stooge name="Larry" hairstyle="bushy">
            <attitude>Middleman</attitude>
          </stooge>
        </stooge>
        <stooge name="Curly" hairstyle="bald">
          <attitude>Fool</attitude>
          <stooge name="Shemp" repeat="yes">
            <stooge name="Joe" hairstyle="bald">
              <stooge name="Curly Joe" hairstyle="bald" />
            </stooge>
          </stooge>
        </stooge>
      </stooges>
    XML_END

=over

=item Counting Stooges

Let's count the number of stooge characters in that document.  To do that, we'd
like a rule that fires on almost all C<E<lt>stoogeE<gt>> elements:

    my $count;

    run(
        XML::Filter::Dispatcher->new(
            Rules => [
                'stooge' => sub { ++$count },
            ],
        )
    );

    print "$count\n";  ## 7

Hmmm, that's one too many: it's picking up on Shemp twice since the document
shows that Shemp had two periods of stoogedom.  The second node has a
convenient C<repeat="yes"> attribute we can use to ignore the duplicate.

We can ignore the duplicate element by adding a "predicate"
expression to the pattern to accept only those elements with no C<repeat>
attribute.  Changing that rule to

                'stooge[not(@repeat)]' => ...

or even the more pedantic

                'stooge[not(@repeat) or not(@repeat = "yes")]' => ...

yields the expected answer (6).

=item Hairstyles and Attitudes

Now let's try to figure out the hairstyles the stooges wore.  To extract 
just the names of hairstyles, we could do something like:

    my %styles;

    run(
        XML::Filter::Dispatcher->new(
            Rules => [
                'string( @hairstyle )' => sub { $styles{xresult()} = 1 },
            ],
        )
    );

    print join( ", ", sort keys %styles ), "\n";

which prints "bald, bowl cut, bushy, mop".  That rule extracts the text
of each C<hairstyle> attribute and the C<xresult()> returns it.

The text contents of elements like C<E<lt>attitudesE<gt>> can also be
sussed out by using a rule like:

                'string( attitude )' => sub { $styles{xresult()} = 1 },

which prints "Bully, Fool, Klutz, Middleman".

Finally, we might want to correlate hairstyles and attitudes by using
a rule like:

    my %styles;

    run(
        XML::Filter::Dispatcher->new(
            Rules => [
                'concat(@hairstyle,"=>",attitude)' => sub {
                    $styles{$1} = $2 if xresult() =~ /(.+)=>(.+)/;
                },
            ],
        )
    );

    print map "$_ => $styles{$_}\n", sort keys %styles;

which prints:

    bald => Fool
    bowl cut => Bully
    bushy => Middleman
    mop => Klutz

=back

=head3 Examples that need to be written

=over

=item * Examples of dispatching to other SAX handlers

=item * Examples for accumulating data

=item * Advanced pattern matching examples

=cut

=head2 EventPath Dialect

"EventPath" patterns are that large subset of XPath patterns that can be
run in a SAX environment without a DOM.  There are a few crucial
differences between the environments that EventPath and XPath each
operate in.

XPath operates on a tree of "nodes" where each entity in an XML document
has only one corresponding node.  The tree metaphor used in XPath has a
literal representation in memory.  For instance, an element
C<E<lt>fooE<gt>> is represented by a single node which contains other
nodes.

EventPath operates on a series of events and both documents and
elements, which are each represented by single nodes in DOM trees, are
both represented by two event method calls, C<start_...()> and
C<end_...()>.  This means that EventPath patterns may match in a
C<start_...()> method or an C<end_...()> method, or even both if you try
hard enough.  Not all patterns have this dual nature; comment matches
occur only in C<comment()> event methods for instance.

EventPath patterns match as early in the document as
possible.  The only times an EventPath pattern will match in an
C<end_...()> method are when the pattern refers to an element's contents
or it uses the C<is-end-event()> function (described below) to do so
intentionally.

The tree metaphor is used to arrange and describe the
relationships between events.  In the DOM trees an XPath engine operates
on, a document or an element is represented by a single entity, called a
node.  In the event streams that EventPath operates on, documents and
element

=head3 Why EventPath and not XPath?

EventPath is not a standard of any kind, but XPath can't cope with
situations where there is no DOM and there are some features that
EventPath need (start_element() vs. end_element() processing for
example) that are not compatible with XPath.

Some of the features of XPath require that the source document be fully
translated in to a DOM tree of nodes before the features can be evaluated.
(Nodes are things like elements, attributes, text, comments, processing
instructions, namespace mappings etc).

These features are not supported and are not likely to be, you might
want to use L<XML::Filter::XSLT|XML::Filter::XSLT> for "full" XPath
support (tho it be in an XSLT framework) or wait for
L<XML::TWIG|XML::TWIG> to grow SAX support.

Rather than build a DOM, XML::Filter::Dispatcher only keeps a bare
minimum of nodes: the current node and it's parent, grandparent, and so
on, up to the document ("root") node (basically the /ancestor-or-self::
axis).  This is called the "context stack", although you may not need to
know that term unless you delve in to the guts.

=head3 EventPath Truth

EventPath borrows a lot from XPath including it's notion of truth.
This is different from Perl's notion of truth; presumably to make
document processing easier.  Here's a table that may help, the
important differences are towards the end:

    Expression      EventPath  XPath    Perl
    ==========      =========  =====    ====
    false()         FALSE      FALSE    n/a (not applicable)
    true()          TRUE       TRUE     n/a
    0               FALSE      FALSE    FALSE
    -0              FALSE**    FALSE    n/a
    NaN             FALSE**    FALSE    n/a (not fully, anyway)
    1               TRUE       TRUE     TRUE
    ""              FALSE      FALSE    FALSE
    "1"             TRUE       TRUE     TRUE

    "0"             TRUE       TRUE     FALSE

 * To be regarded as a bug in this implementation
 ** Only partially implemented/supported in this implementation

Note: it looks like XPath 2.0 is defining a more workable concept
for document processing that uses something resembling Perl's empty
lists, C<()>, to indicate empty values, so C<""> and C<()> will be
distinct and C<"0"> can be interpreted as false like in Perl.  XPath2
is I<not> provided by this module yet and won't be for a long time 
(patches welcome ;).

=head3 EventPath Examples

All of this means that only a portion of XPath is available.  Luckily,
that portion is also quite useful.  Here are examples of working XPath
expressions, followed by known unimplemented features.

TODO: There is also an extension function available to differentiate between
C<start_...> and C<end_...> events.  By default

=head2 Examples

 Expression          Event Type      Description (event type)
 ==========          ==========      ========================
 /                   start_document  Selects the document node
 /[is-end-event()]   end_element        "     "     "      "

 /a                  start_element   Root elt, if it's "<a ...>"
 /a[is-end-event()]  end_element       "   "   "  "       "

 a                   start_element   All "a" elements
 a[is-end-event()]   end_element      "   "     "

 b//c                start_element   All "c" descendants of "b" elt.s

 @id                 start_element   All "id" attributes

 string( foo )       end_element     fires at each </foo> or <foo/>;
                                     xresult() returns the
                                     text contained in "<foo>...</foo>"

 string( @name )     start_element   All "name" attributes;
                                     xresult() returns the
                                     text of the attribute.

=head2 Methods

=over

=cut

$VERSION = 0.31;
@ISA = qw( XML::SAX::Base Exporter );

@EXPORT_OK = qw( xresult xset_var xget_var );
%EXPORT_TAGS = ( all => \@EXPORT_OK );


use strict ;
use vars qw( $cur_self ); ## For calling some methods as subs from actions.

use Carp ;
use Exporter;
use XML::SAX::Base;
use XML::Filter::Dispatcher::Parser;

##
## $ctx->{Vars}  a HASH of variables passed in from the parent context
##               (or the Perl world in the doc root node context).  Set
##               in all child contexts.
##
## $ctx->{ChildVars} a HASH of variables set in by this node, passed
##                   on to all child contexts, but erased before this
##                   node's siblings can see it.
##

=item new

    my $f = XML::Filter::Dispatcher->new(
        Rules => [   ## Order is significant
            "/foo/bar" => sub {
                ## Code to execute
            },
        ],
    );

=cut

sub new {
    my $proto = shift ;
    my $self = $proto->SUPER::new( @_ ) ;

    $self->{Rules} = [ %{$self->{Rules}} ]
        if ref $self->{Rules} eq "HASH";
    
    my $ctx = $self->{CtxStack}->[0] = $self->{DocCtx} = {};
    $self->{RuleTexts} = [];
    $self->{CompiledRules} = [];

    my $rule_number = 0;
    while ( @{$self->{Rules}} ) {
        my ( $expr, $action ) = (
            shift @{$self->{Rules}},
            shift @{$self->{Rules}}
        );

        my $action_type = ref $action;
        push @{$self->{Actions}}, $action;
        my $action_num = $#{$self->{Actions}};
        my $action_code;

        my %options = (
            %$self,
            RuleNumber => $rule_number++,
        );
        
        if ( $action_type && $action_type ne "CODE" ) {
            ## Must be a SAX handler, make up some action code to
            ## install it and arrange for it's removal.
#            $options{RunAtStartAndEnd} = 1;
            $action_code = <<CODE_END;
{
    my ( \$d, \$ctx ) = \@_;
    warn "replacing \\\$ctx->{TempHandler}\n" if \$ctx->{TempHandler};
    \$ctx->{TempHandler} = \$d->{Handler};
    \$d->set_handler( \$d->{Actions}[$action_num] );
    if ( \$ctx->{IsStartEvent} && \$ctx->{EventType} ne "start_document" ) {
        \$d->XML::SAX::Base::start_document(
            \@{\$d->{CtxStack}->[0]->{SAXArgs}}
        );
    }
}
CODE_END
        }
        else {
            ## It's a code ref, arrange for it to be called
            ## directly.
            $action_code = <<A;
\$d->{Actions}[$action_num]->( \$d, \$ctx->{Node} );
A
        }

        my $code = XML::Filter::Dispatcher::Parser->parse(
            $expr,
            $action_code,
            \%options,
        );

        die "Couldn't compile XPath expression '$expr'\n"
            unless defined $code;

        if ( length $code ) {
            my $rule_text = $expr;
            $rule_text =~ s/^/## /mg;
            $code = <<CODE_END;
$rule_text
package XFD;

use strict;

$code;
CODE_END

            if ( $self->{Debug} ) {
                my $c = $code;
                my $ln = 1;
                $c =~ s{^}{sprintf "%4d|", $ln++}gme;
                warn $c;
            }
            ## TODO: move this eval out of the loop.
            my @subs = eval $code;
            if ( $@ ) {
                my $c = $code;
                my $ln = 1;
                $c =~ s{^}{sprintf "%4d|", $ln++}gme;
                die $@, $c;
            }

            push @{$ctx->{DocTests}}, @subs;
        }
    }

    for ( keys %{$self->{Vars}} ) {
        $self->xset_var( $_, @{$self->{Vars}->{$_}} );
    }

    ## $ctx *is* the context for the doc's root node, but xset_var
    ## sets variables in the slot we pass on to children (only) and
    ## not peers.
    $ctx->{Vars} = $ctx->{ChildVars};
    delete $ctx->{ChildVars};

    return $self ;
}

=item xresult

    "string( foo )" => sub { xresult, "\n" }, # if imported
    "string( foo )" => sub { print shift->xresult, "\n" },

Returns the result of the last EventPath evaluated; this is the result that
fired the current rule.  The example prints all text node children of
C<E<lt>fooE<gt>> elements, for instance.

=cut

sub xresult() { ( shift || $cur_self )->{ExpressionResult} }

=item xset_var

    "foo" => sub { xset_var( bar => string => "bingo" ) }, # if imported
    "foo" => sub { shift->xset_var( bar => boolean => 1 ) },

Sets an XPath variables visible in the current context and all child
contexts.  Will not be visible in parent contexts or sibling contexts.

Legal types are C<boolean>, C<number>, and C<string>.  Node sets and
nodes are unsupported at this time, and "other" types are not useful
unless you work in your own functions that handle them.

Variables are visible as C<$bar> variable references in XPath expressions and
using xget_var in Perl code.  Setting a variable to a new value temporarily
overrides any existing value, somewhat like using Perl's C<local>.

=cut

sub xset_var {
    my $self = @_ && UNIVERSAL::isa( $_[0], __PACKAGE__ ) ? shift : $cur_self;
    croak
        "Wrong number of parameters (" . @_ . ") passed to xset_var, need 3.\n"
        if @_ != 3;

    my ( $name, $type, $value ) = @_;
    croak "undefined type passed to xset_var\n" unless defined $type;
    croak "undefined name passed to xset_var\n" unless defined $name;
    croak "undefined value passed to xset_var\n" unless defined $name;

    my $ctx = $self->{CtxStack}->[-1];
    ## TODO: rename the type non-classes to st other than "string", etc.
    $ctx->{ChildVars}->{$name} = bless \$value, $type;
}


## Used in compiled XPath exprs only; only minimal safeties engaged.
sub _look_up_var {
    my $self = shift;
    my ( $vname ) = @_;

    my $ctx = $self->{CtxStack}->[-1];

#use Data::Dumper; warn Data::Dumper::Dumper( $ctx );

    return $ctx->{ChildVars}->{$vname} if exists $ctx->{ChildVars}->{$vname};
    return $ctx->{Vars}->{$vname} if exists $ctx->{Vars}->{$vname};

    die "Unknown variable '\$$vname' referenced in XPath expression\n";
}


=item xget_var

    "bar" => sub { print xget_var( "bar" ) }, # if imported
    "bar" => sub { print shift->xget_var( "bar" ) },

Retrieves a single variable from the current context.  This may have
been set by a parent or by a previous rule firing on this node, but
not by children or preceding siblings.

Returns C<undef> if the variable is not set (or if it was set to undef).

=cut

sub xget_var {
    my $self = @_ && UNIVERSAL::isa( $_[0], __PACKAGE__ ) ? shift : $cur_self;
    croak "No variable name passed to xget_var.\n"
        unless @_;
    croak "More than one variable name passed to xget_var.\n"
        unless @_ == 1;

    my ( $vname ) = @_;

    croak "Undefined variable name passed to xget_var.\n"
        unless defined $vname;

    my $ctx = $self->{CtxStack}->[-1];

#use Data::Dumper; warn Data::Dumper::Dumper( $ctx );
    return
        exists $ctx->{ChildVars}->{$vname}
            ? ${$ctx->{ChildVars}->{$vname}}
        : exists $ctx->{Vars}->{$vname}
            ? ${$ctx->{Vars}->{$vname}}
            : undef;
}


=item xget_var_type

    "bar" => sub { print xget_var_type( "bar" ) }, # if imported
    "bar" => sub { shift->xget_var_type( "bar" ) },

Retrieves the type of a variable from the current context. This may have
been set by a parent or by a previous rule firing on this node, but
not by children or preceding siblings.

Returns C<undef> if the variable is not set.

=cut

sub xget_var_type {
    my $self = @_ && UNIVERSAL::isa( $_[0], __PACKAGE__ ) ? shift : $cur_self;
    croak "No variable name passed to xget_var_type.\n"
        unless @_;
    croak "More than one variable name passed to xget_var_type.\n"
        unless @_ == 1;

    my ( $vname ) = @_;

    croak "Undefined variable name passed to xget_var.\n"
        unless defined $vname;

    my $ctx = $self->{CtxStack}->[-1];

    return
        exists $ctx->{ChildVars}->{$vname}
            ? ref $ctx->{ChildVars}->{$vname}
        : exists $ctx->{Vars}->{$vname}
            ? ref $ctx->{Vars}->{$vname}
            : undef;
}


=back

=cut

##
## SAX handlers
##

## a helper called by the handlers...
sub _check_tests {
    my $self = shift ;
    my ( $test_type, $node_type, $event_type, $child_ctx, @sax_args ) = @_;

    local $cur_self = $self;

    my $ctx = $self->{CtxStack}->[-1];
    $ctx->{NodeType}  = $node_type;
    $ctx->{EventType} = $event_type;
    $ctx->{Node}      = $sax_args[0];
    $ctx->{SAXArgs}   = \@sax_args;

    %XFD::already_ran = ();
    @{$ctx->{Precursors}} = ();
    if ( exists $ctx->{DescendantTests} ) {
        for ( @{$ctx->{DescendantTests}} ) {
            $_->( $self, $ctx, $child_ctx );
        }
        ## Use a new array so modifications to the child context don't
        ## propogate back up.  Only need to do this on elts, since only
        ## they have descendants.
        $child_ctx->{DescendantTests} = [ @{$ctx->{DescendantTests}} ]
            if exists $ctx->{DescendantTests} && $test_type eq "EltTests";
    }

    if ( exists $ctx->{$test_type} ) {
        for ( @{$ctx->{$test_type}} ) {
            $_->( $self, $ctx, $child_ctx );
        }
    }

    $child_ctx->{Text} = ""
        if exists $ctx->{Text};

    $child_ctx->{Vars} = {
        %{$ctx->{Vars}      || {}},
        %{$ctx->{ChildVars} || {}},
    };

    %{$ctx->{ChildVars}} = () if exists $ctx->{ChildVars};
    return $child_ctx;
}


sub start_document {
    my $self = shift ;

    $self->{CtxStack} = [ $self->{DocCtx} ] ;   ## Context Stack

    ## precalc this because most actions need it.
    local $self->{CtxStack}->[-1]->{IsStartEvent} = 1;

    CORE::push @{$self->{CtxStack}}, $self->_check_tests(
        "DocTests",
        "document",
        "start_document",
        {},
        @_
    );
    $self->SUPER::start_document( @_ );
}


sub end_document {
    my $self = shift ;
    my ( $doc ) = @_;

    my $child_ctx = CORE::pop @{$self->{CtxStack}};

    my $ctx = $self->{CtxStack}->[-1];
    $ctx->{Text} .= $child_ctx->{Text}
        if exists $ctx->{Text};

    ## precalc this because most actions need it.
    local $ctx->{IsEndEvent} = 1;

    $self->_check_tests(
        "DocTests",
        "document",
        "end_document",
        $child_ctx,
        @_
    );

    my $r = $self->SUPER::end_document( @_ );

    if ( exists $ctx->{TempHandler} ) {
        $self->set_handler( $ctx->{TempHandler} );
        delete $ctx->{TempHandler};
        $self->SUPER::end_document({});
    }
    
    return $r;
}


sub start_element {
    my $self = shift ;
    my ( $elt) = @_ ;

    ## precalc this because most actions need it.
    local $self->{CtxStack}->[-1]->{IsStartEvent} = 1;

    my $child_ctx = $self->_check_tests(
        "EltTests",
        "element",
        "start_element",
        {},
        @_
    );
    CORE::push @{$self->{CtxStack}}, $child_ctx;

    ## Scan the attributes
    if (   exists $elt->{Attributes} 
        && $elt->{Attributes} 
        && exists $child_ctx->{AttrTests}
    ) {
        for my $attr ( values %{$elt->{Attributes}} ) {
            ## No need to push the new context, no nodes hang below attr nodes.
            {
                local $child_ctx->{IsStartEvent} = 1;
                local $child_ctx->{IsEndEvent}   = 1;
                $self->_check_tests(
                    "AttrTests",
                    "attribute",
                    "start_attribute",  ## not a real SAX event
                    {},
                    $attr,
                );
            }
        }
    }
    $self->SUPER::start_element( @_ );
}


sub end_element {
    my $self = shift ;
    my ( $elt ) = @_ ;

    ## This pop matches the push in start_element.
    my $child_ctx = CORE::pop @{$self->{CtxStack}};

    $self->{CtxStack}->[-1]->{Text} .= $child_ctx->{Text}
        if exists $self->{CtxStack}->[-1]->{Text};

    my $ctx = $self->{CtxStack}->[-1];
    local $ctx->{IsEndEvent} = 1;

    ## Need to pass $child_ctx in here in case we've been collecting
    ## text in $child_ctx->{Text}.  Some rule expression will want this
    ## to look at or to return as a result value.
    $self->_check_tests(
        "EltTests",
        "element",
        "end_element",
        $child_ctx,
#        @_
@{$ctx->{SAXArgs}} ## Hack to allow attributes to be avail in IsEndEvent processing
    );

    my $r = $self->SUPER::end_element( @_ );

    if ( exists $ctx->{TempHandler} ) {
        $self->SUPER::end_document({});
        $self->set_handler( $ctx->{TempHandler} );
        delete $ctx->{TempHandler};
    }

    return $r;
}


sub characters {
    my $self = shift ;
    my ( $data ) = @_;

    ## precalc this because most actions need it.
    my $ctx = $self->{CtxStack}->[-1];
    local $ctx->{IsStartEvent} = 1;
    local $ctx->{IsEndEvent} = 1;

    $ctx->{Text} .= $data->{Data}
        if exists $ctx->{Text};

    my $child_ctx = $self->_check_tests(
        "TextTests",
        "text",
        "characters",
        {},
        @_
    );

    $self->SUPER::characters( @_ );
    return undef;
}


sub comment {
    my $self = shift ;
    my ( $data ) = @_;

    ## precalc this because most actions need it.
    local $self->{CtxStack}->[-1]->{IsStartEvent} = 1;
    local $self->{CtxStack}->[-1]->{IsEndEvent} = 1;

    my $child_ctx = $self->_check_tests(
        "CommentTests",
        "comment",
        "comment",
        {},
        @_
    );

    $self->SUPER::comment( @_ );
    return undef;
}


sub processing_instruction {
    my $self = shift ;
    my ( $data ) = @_;

    ## precalc this because most actions need it.
    local $self->{CtxStack}->[-1]->{IsStartEvent} = 1;
    local $self->{CtxStack}->[-1]->{IsEndEvent} = 1;

    my $child_ctx = $self->_check_tests(
        "PITests",
        "processing-instruction",
        "processing_instruction",
        {},
        @_
    );

    $self->SUPER::processing_instruction( @_ );
    return undef;
}

=head2 Notes for XPath Afficianados

This section assumes familiarity with XPath in order to explain some of
the particulars and side effects of the incremental XPath engine.

=over

=item *

Much of XPath's power comes from the concept of a "node set".  A node
set is a set of nodes returned by many XPath expressions.
Event XPath fires a rule once for each node the rule applies to.  If there
is a location path in the expression, the rule will fire once for each
document element (perhaps twice if both start and end SAX events are
trapped, see C<is-start-event()> and C<is-end-event()> below.

Expressions like C<0>, C<false()>, C<1>, and C<'a'> have no location
path and apply to all nodes (including namespace nodes and processing
instructions).

=item *

Because of the implied set membership operation on node set expressions,
C<foo>, C<./foo>, C<.//foo> and C<//foo> are all equivalent rules; they all
fire for every element node named "foo" in the document.  This is
because the context is always that of the current node for the SAX event
(except for attributes, which SAX doesn't have an event for, but we act
like it did; ie each attr gets it's own context to operate in).  This is
a lot like the C<match=> expression in XSLT C<E<lt>xsl:templateE<gt>>
constructs.

=item *

The XPath parser catches some simple mistakes Perlers might make in typing
XPath expressions, such as using C<&&> or C<==> instead of C<and> or C<=>.

=item *

SAX does not define events for attributes; these are passed in to the
start_element (but not end_element) methods as part of the element
node.  XML::Filter::Dispatcher does allow selecting attribute nodes and
passes in just the selected attribute node, see the examples above.

=item *

Axes in path steps (/foo::...)

Only some axes can be reasonably supported within a SAX framework without
building a DOM and/or queueing SAX events for in-document-order delivery.

=item *

text node aggregation

SAX does not guarantee that C<characters> events will be aggregated as
much as possible, as text() nodes do in XPath.  Generally, however,
this is not a problem; instead of writing

    "quotation/text()" => sub {
        ## BUG: may be called several times within each quotation elt.
        my $self = shift;
        print "He said '", $self->current_node->{Data}, "'\n'";
    },

write

    "string( quotation )" => sub {
        my $self = shift;
        print "He said '", $self->expression_result, "'\n'";
    },

The former is unsafe; consider the XML:

    <quotation>I am <!-- bs -->GREAT!<!-- bs --></quotation>

Rules like C<.../text()> will fire twice, which is not what is needed here.

Rules like C<string( ... )> will fire once, at the end_element event, with
all descendant text of quotation as the expression result.

You can also place an L<XML::Filter::BufferText|XML::Filter::BufferText>
instance upstream of XML::Filter::Dispatcher if you really want to
use the former syntax (but the C<GREAT!> example will still generate
more than one event due to the comment).

=item *

Axes

=over

=item o

self (yes)

=item o

descendant (yes)

=item o

descendant-or-self (yes)

=item o

child (yes)

=item o

attribute (yes)

=item o

namespace (todo)

=item o

ancestor (todo, will be limited)

=item o

ancestor-or-self (todo, will be limited)

=item o

parent (todo, will be limited)

parent/ancestor paths will not allow you to descend the tree, that would
require DOM building and SAX event queueing.

=item o

preceding (no: reverse axis, would require DOM building)

=item o

preceding-sibling (no: reverse axis, would require DOM building)

=item o

following (no: forward axis, would require DOM building and rule
activation queueing)

=item o

following-sibling (no: forward axis, would require DOM building
and rule activation queueing)

=back

=item *

Implemented XPath Features

Anything not on this list or listed as unimplemented is a TODO.  Ring me
up if you need it.

=over

=item *

String Functions

=over

=item o

concat( string, string, string* )

=item o

contains( string, string )

=item o

normalize-space( string )

=item o

starts-with( string, string )

=item o

string( object )

Object may be a number, boolean, string, or the result of a location path:

    string( 10 );
    string( /a/b/c );
    string( @id );

Unlike normal DOM oriented XPath, calling string on a location path causes
the string to be calculated once each time the location path matches.  So
a run like:

    "string(@part-number)" => sub {
        my $self = shift;
        print "Part number: ", $self->expression_result, "\n";
    }

will print as many times as there are C<part-number> attributes in the
document.  This is true anywhere an XPath node set is used as an
argument to a function or logical operator.

=item o

string-length( string )

string-length() not supported; can't stringify the context node without
keeping all of the context node's children in mempory.  Could enable it
for leaf nodes, I suppose, like attrs and #PCDATA containing elts.  Drop
me a line if you need this (it's not totally trivial or I'd have done it).

=item o

substring( string, number, number? )

=item o

substring-after( string, string )

=item o

substring-before( string, string )

=item o

translate( string, string, string )

=back

=item *

Boolean Functions, Operators

=over

=item o

boolean( object )

See notes about node sets for the string() function above.

=item o

false()

=item o

not( object )

See notes about node sets for the string() function above.

=item o

true()

=back

=item *

Number Functions, Operators

=over

=item o

ceil( number )

=item o

floor( number )

=item o

number( object )

Converts strings, numbers, booleans, or the result of a location path
(C<number( /a/b/c )>).  See the C<string( object )> description above for more
information on location paths.

Unlike real XPath, this dies if the object cannot be cleanly converted in to a
number.  This is due to Perl's varying level of support for NaN, and may change
in the future.

=back

=item *

All relational operators

No support for nodesets, though.

=item *

All logical operators

Supports limited nodesets, see the string() function description for details.

=item *

Additional Functions

=over

=item o

is-end-event()

This is en extension function that returns C<true> when an end_element or
end_document event is being processed.

=item o

is-start-event()

This is en extension function that returns C<true> when handling and SAX event
other than end_element or end_document.

=back

=back

=item *

Missing Features

Some features are entirely or just currently missing due to the lack of
nodesets or the time needed to work around their lack.  This is an incomplete list; it's growing as I find new things not to implement.

=over

=item o

count()

No nodesets => no count() of nodes in a node set.

=item o

last()

With SAX, you can't tell when you are at the end of what would be a node set
in XPath.

=item o

position()

I will implement pieces of this as I can.  None are implemented as yet.

=back

=item *

Todo features

=over

=item o

id()

=item o

lang()

=item o

local-name()

May not be able to handle local-name( arg ), just argless local-name().

=item o

name()

May not be able to handle name( arg ), just argless name().

=item o

namespace-uri()

May not be able to handle namespace-uri( arg ), just argless namespace-uri().

=item o

sum( node-set )

=back

=item *

Extensions

=over

=item o

is-start-event(), is-end-event()

XPath has no concept of time; it's meant to operate on a tree of nodes.  SAX
has C<start_element> and C<end_element> events and C<start_document> and
C<end_document> events.

By default, XML::Filter::Dispatcher acts on start events and not end events
(note that all rules are evaluated on both, but the actions are not run on end_
events by default).

By including a call to the C<is-start-event()> or C<is-end-event()> functions in a
predicate the rule may be forced to fire only on end events or on both start
and end events (using a C<[is-start-event() or is-end-event()]> idiom).

=back

=back

=head1 TODO

=over

=item *

Namespace support.

=item *

Text node aggregation so C<text()> handlers fire once per text node
instead of once per C<characters()> event.

=item *

Nice messages on legitimate but unsupported axes.

=item *

/../ (parent node)

=item *

C<add_rule()>, C<remove_rule()>, C<set_rules()> methods.

=back

=head1 LIMITATIONS

=over

=item *

NaN is not handled properly due to mediocre support in C<perl>,
especially across some platforms that it apparently isn't easily supported on.

=item *

-0 (negative zero) is not provided or handled properly

=item *

+/- Infinity is not handled properly due to mediocre support in C<perl>,
especially across some platforms that it apparently isn't easily supported on.

=back

This is more of a frustration than a limitation, but this class requires that
you pass in a type when setting variables (in the C<Vars> ctor parameter or
when calling C<xset_var>).  This is so that the engine can tell what type a
variable is, since string(), number() and boolean() all treat the Perlian C<0>
differently depending on it's type.  In Perl the digit C<0> means C<false>,
C<0> or C<'0'>, depending on context, but it's a consistent semantic.  When
passing a C<0> from Perl lands to XPath-land, we need to give it a type so that
C<string()> can, for instance, decide whether to convert it to C<'0'> or
C<'false'>.

=head1 THANKS

...to Kip Hampton, Robin Berjon and Matt Sergeant for sanity checks and
to James Clark (of Expat fame) for posting a Yacc XPath grammar where
I could snarf it years later and add lots of Perl code to it.

=head1 AUTHOR

    Barrie Slaymaker <barries@slaysys.com>

=head1 COPYRIGHT

    Copyright 2002, Barrie Slaymaker, All Rights Reserved.

You may use this module under the terms of the Artistic or GNU Pulic
licenses your choice.  Also, a portion of XML::Filter::Dispatcher::Parser
is covered by:

        The Parse::Yapp module and its related modules and shell scripts are
        copyright (c) 1998-1999 Francois Desarmenien, France. All rights
        reserved.

        You may use and distribute them under the terms of either the GNU
        General Public License or the Artistic License, as specified in the
        Perl README file.

Note: Parse::Yapp is only needed if you want to modify
lib/XML/Filter/Dispatcher/Grammar.pm

=cut

1 ;

package XML::Filter::Dispatcher ;

=head1 NAME

XML::Filter::Dispatcher - Path based event dispatching with DOM support

=head1 SYNOPSIS

    use XML::Filter::Dispatcher ;

    ## Using callbacks:
    my $h = XML::Filter::Dispatcher->new(
        Rules => {   ## use [...] instead of {...} if order is important
            ## A small subset of XPath expressions can be used here
           '//foo'  => \&handle_foo_start_tag,
           '//@bar' => \&handle_bar_attr_at_start_tag,
       },
#       DOMBuilder => XML::LibXML::Filter::Builder->new() ;
    );

=head1 DESCRIPTION

B<WARNING>: Alpha code alert.

Allows a reasonable (IMHO) subset of XPath expressions to be mapped to
handlers, Provides a stack mechanism for building output data
structures.  data structures easy.  Inherits from the XML::SAX::Base
module so it can be used as filter quite easily.

TODO: Provide an "auto-forward" feature that can be used to gate the
output events so as to prune the tree.

B<WARNING>: The documentation sucks and the API is likely to change.

=head1 Incremental XPath Dialect

Some of the features of XPath require that the source document be fully
translated in to a tree of "nodes" before the features can be evaluated.
(Nodes are things like elements, attributes, text, comments, processing
instructions, namespace mappings etc).

These features are not supported and are not likely to be, you might
want to use L<XML::Filter::XSLT> for "full" XPath support (tho it be in
an XSLT framework) or wait for L<XML::TWIG> to be SAXed up.

Rather than build a DOM, XML::Filter::Dispatcher only keeps a bare
minimum of nodes: the current node and it's parent, grandparent, and so
on, up to the document ("root") node (basically the /ancestor-or-self::
axis).  This is called the "context stack", although you may not need to
know that term unless you delve in to the guts.

All of this means that only a portion of XPath is available.  Luckily,
that portion is also quite useful.  Here are examples of working XPath
expressions, followed by known unimplemented features.

TODO: There is also an extension function available to differentiate between
C<start_...> and C<end_...> events.  By default

=head2 Examples

 Expression     Event Type      Description (event type)
 ==========     ==========      ========================
 /              start_document  Selects the document node
 /a             start_element   Root elt, if it's "<a ...>"
 //a            start_element   All "a" elements
 //b//c         start_element   All "c" descendants of "b" elt.s


=head2 Notes for XPath Afficianados

This section assumes familiarity with XPath in order to explain some of
the particulars and side effects of the incremental XPath engine.

=over

=item *

Much of XPath's power comes from the concept of a "node set".  A node
set is a set of nodes returned by many XPath expressions.
XML::Filter::Dispatcher treats node set expressions as boolean
expressions: if the current event (node) would be in the node set
returned by an expression in a "real" XPath processor, the expression is
true and the handler runs.

=item *

SAX does not define events for attributes; these are passed in to the
start_element and (usually) end_element methods as part of the element
node.  XML::Filter::Dispatcher does allow selecting attribute nodes and
passes in just the selected attribute node, see the examples above.

=item *

Axes in path steps (/foo::...)

Axes can be reasonably supported within a SAX framework to a varying
degree without buffering of document nodes or queueing SAX events for
in-document-order delivery.

=over

=item Axes

=over

=item self (yes)

=item descendant (yes)

=item descendant-or-self (yes)

=item child (yes)

=item attribute (yes)

=item namespace (todo)

=item ancestor (todo, will be limited)

=item ancestor-or-self (todo, will be limited)

=item parent (todo, will be limited)

parent/ancestor paths will not allow you to descend the tree, that would
require DOM building and SAX event queueing.

=item preceding (no: reverse axis, would require DOM building)

=item preceding-sibling (no: reverse axis, would require DOM building)

=item following (no: forward axis, would require DOM building and rule
activation queueing)

=item following-sibling (no: forward axis, would require DOM building
and rule activation queueing)

=back

=back

=back

=cut

$VERSION = 0.11;
@ISA = qw( XML::SAX::Base );

use strict ;

use Carp ;
use XML::SAX::Base;
use XML::Filter::Dispatcher::Parser;

sub new {
    my $proto = shift ;
    my $self = $proto->SUPER::new( @_ ) ;

    $self->{Rules} = [ %{$self->{Rules}} ]
        if ref $self->{Rules} eq "HASH";
    
    my $ctx = $self->{DocCtx} = {};
    $self->{CompiledRules} = [];

    while ( @{$self->{Rules}} ) {
        my ( $expr, $handler ) = (
            shift @{$self->{Rules}},
            shift @{$self->{Rules}}
        );
        my $op = XML::Filter::Dispatcher::Parser->parse(
            $expr,
            $self,
        );
        $op->bootstrap( $handler, $ctx );
        CORE::push @{$self->{CompiledRules}}, $op;
    }

    return $self ;
}

##
## Misc. methods
##
sub elt_depth {
    my $self = shift ;
    confess "elt_depth() takes no parameters\n"
        if @_ ;
    return $self->{ELT_DEPTH} ;
}


sub path {
    my $self = shift ;

    $self->{PATH} = shift if @_ ;

    return $self->{PATH} ;
}



sub _call {
    my $self = shift ;

    for ( shift ) {
        if ( ! ref ) {
            my $sub = $self->can( $_ ) ;
            confess "Handler $_ not declared"
        	unless $sub ;
            $sub->( $self, @_ ) ;
        }
        elsif ( ref eq "CODE" ) {
            $_->( $self, @_ ) ;
        }
        elsif ( ref eq "ARRAY" ) {
            $self->_call( $_, @_ ) for @$_ ;
        }
    }
}

##
## Object stack access
##
sub stack_size {
    my $self = shift ;
    return scalar @{$self->{STACK}} ;
}


sub root {
    my $self = shift ;
    confess "root takes no parameters, use push()"
        if @_ ;
    confess "No root object pushed"
        unless @{$self->{STACK}} ;
    return $self->{STACK}->[0]->[1] ;
}


sub push {
    my $self = shift ;
    confess "push() requires at least 1 parameter"
        unless @_ ;
    confess "undef passed instead of a reference"
        unless defined $_[-1] ;
    push @{$self->{STACK}}, map [ $self->{ELT_DEPTH}, $_ ], @_ ;
}


sub _peek {
    my $self = shift ;
    confess "Stack is empty"
        unless @{$self->{STACK}} ;
    $self->{STACK}->[-1] ;
}


sub peek { shift()->_peek->[-1] }


sub pop {
    my $self = shift ;
    confess "Stack is empty"
        unless @{$self->{STACK}} ;
    $self->{LAST_POPPED} = CORE::pop( @{$self->{STACK}} )->[-1] ;
}


sub pop_to_depth {
    my $self = shift ;
    $self->pop
        while $self->stack_size && $self->_peek->[0] > $self->elt_depth ;
}


##
## Visualization
##

=item as_graphviz

    binmode STDOUT,
    print STDOUT $d->as_graphviz->as_png;

Charts the dispatcher's rule base.  Creates I<huge> graphs for all but
trivial rule bases.

=cut

sub as_graphviz {
    my $self = shift;

    require GraphViz;
    my $g = GraphViz->new;
    $g->add_node(
        name  => int $self,
        label => ref $self,
        shape => "record",
        color => "blue",
    );

    for ( @{$self->{CompiledRules}} ) {
        $_->_add_to_graphviz( $g );
        $g->add_edge( { from => int $self, to => int $_ } );
    }

    return $g;
}

=item as_png

    $d->as_png( "file.png" );
    $d->as_png( ">file.png" );
    $d->as_png( "| viewer_program" );
    $d->as_png( "| cat > foo.png && png_viewer foo.png" );

Shorthand for printing as_graphviz_as_png to the file.

=cut

sub as_png {
    my $self = shift;

    my ( $f ) = @_;
    $f = ">$f" unless $f =~ /^\s*[|>]/;

    open F, "$f" or die "$!: $f";
    binmode F;
    print F $self->as_graphviz->as_png;
    close F;
}

##
## SAX handlers
##

## When there are no checks we need to use the "empty" list.
my $mt = [];

sub _check_tests {
    my $self = shift ;
    my ( $test_type, $event_type, $sax_data ) = @_;

    my $ctx = $self->{CtxStack}->[-1];
    $ctx->{EventType} = $event_type;
    $ctx->{EventData} = $sax_data;
    my $new_ctx = {};

    my $t = $ctx->{$test_type} || $mt;

    for ( @$t ) {
        my $action = $_->check( $ctx, $new_ctx );
        next unless defined $action;
        if ( ref $action eq "CODE" ) {
            $action->( $self, $sax_data );
        }
        else {
            $action->install( $new_ctx );
        }
    }

    return $new_ctx;
}


sub start_document {
    my $self = shift ;
    $self->{CtxStack} = [ $self->{DocCtx} ] ;   ## Context Stack

    $self->{STACK} = [] ;
    $self->{ELT_DEPTH} = 0 ;

    CORE::push @{$self->{CtxStack}}, $self->_check_tests(
        "DocTests",
        "start_document",
        @_
    );
}


sub start_element {
    my $self = shift ;
    my ( $elt) = @_ ;

    my $type = $elt->{Name} ;

#    warn $type, "\n", Dumper( $elt ), Dumper( $self );

    $self->{PATH} .= "/" . $type ;
    ++$self->{ELT_DEPTH} ;

    $self->{CtxStack}->[-1]->{Elt} = $elt;

    my $new_ctx = $self->_check_tests(
        "EltTests",
        "start_element",
        @_
    );
    CORE::push @{$self->{CtxStack}}, $new_ctx;

    ## Scan the attributes
    if ( $elt->{Attributes} && exists $new_ctx->{AttrTests} ) {
        for my $attr ( values %{$elt->{Attributes}} ) {
            ## No need to push the new context, no nodes hang below attr nodes.
            $self->{CtxStack}->[-1]->{Attr} = $attr;
            $self->_check_tests(
                "AttrTests",
                "attribute",  ## not a real SAX event
                $attr,
            );
        }
        delete $new_ctx->{Attr};
    }

#    my $sub_name = $type ;
#    $sub_name =~ s/\W/_/g ;

#    my $method = $self->can( "start_${sub_name}_elt" ) ;
#    $self->$method( @_ ) if $method ;

#    $self->_call( $self->{StartEltHandlers}->{$type}, @_ )
#        if exists $self->{StartEltHandlers}->{$type} ;
}

#use Data::Dumper ;

sub end_element {
    my $self = shift ;
    my ( $elt ) = @_ ;

    my $type = $elt->{Name} ;

    #warn "/", $type, "\n" ;

    $type =~ s/\W/_/g ;

    my $method = $self->can( "end_${type}_elt" ) ;
    $self->$method( @_ ) if $method ;

    $self->_call( $self->{EndEltHandlers}->{$type}, @_ )
        if exists $self->{EndEltHandlers}->{$type} ;

    --$self->{ELT_DEPTH} ;
    $self->{PATH} =~ s{/[^/]+\z}{} ;
    $self->pop_to_depth ;

    ## TODO: Run check_tests here.

    CORE::pop @{$self->{CtxStack}};
}


sub end_document {
    my $self = shift ;
    return $self->{LAST_POPPED} ;
}


sub characters {
    my $self = shift ;
    my ( $data ) = @_ ;
    if ( @{$self->{STACK}} && ref $self->peek eq "SCALAR" ) {
        ${$self->peek} .= $data->{Data} ;
    }
}

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

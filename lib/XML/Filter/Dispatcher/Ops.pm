package XML::Filter::Dispatcher::Ops;

=head1 NAME

XML::Filter::Dispatcher::Ops - The Syntax Tree

=head1 SYNOPSIS

    None.  Used by XML::Filter::Dispatcher.

=cut

use strict;

## TODO: Replace XFD:: with XML::Filter::Dispatcher

sub XFD::_declare {
    my $pkg = caller;

    {
        no strict "refs";
        local $^W = 0;

        @{"${pkg}::ISA"} = qw( XFD::Base );
        @{"${pkg}::parms"}  = grep
            ! /^_/,
                map
                    $_ eq "undef"
                        ? undef
                        : $_,
                    @_;
        @{"${pkg}::fields"} = grep
            $_ ne "undef",
            map {
                my $f = $_ ;
                $f =~ s/^_//;
                $f
            } @_ ;
    }

    ## Build the accessors.
    my $i = 0;
    my @code;
    for ( @_ ) {
        next if $_ eq "undef";

        my $name = $_;
        $name =~ s/^_//;

        push @code, <<FOO;
            sub ${pkg}::_$name    () { $i }
            sub ${pkg}::$name     () { shift->[$i] }
FOO
        $i++;
    }

    @code and eval join "", @code, "1" or die $!;
}


package XFD::Base;

use Carp;

sub new {
    my $proto = shift;
    my $parser = shift;

    warn $proto, "( ", join( ", ", map defined $_ ? $_ : "undef", @_ ), " )\n"
        if $parser->{USER}->{Debug};

    my @parms;
    my @fields;
    {
        no strict "refs";
        @parms = @{"${proto}::parms"};
        @fields = @{"${proto}::fields"};
    }

    if ( $#_ > $#parms ) {
        croak "Extra parameter $proto->new( ",
            join( ", ", map defined $_ ? $_ : "undef", @parms ),
            " ): Got ( ",
            join ( ", ", map defined $_ ? $_ : "undef", @_ ),
            " )"
    }
    elsif ( $#_ < $#parms ) {
        croak "Missing parameter $proto->new( ",
            join( ", ", map defined $_ ? $_ : "undef", @parms ),
            " ): Got ( ",
            join ( ", ", map defined $_ ? $_ : "undef", @_ ),
            " )"
    }

    my $self = bless [ (undef) x @fields ], $proto;
    my $i = 0;
    for ( 0..$#_ ) {
        my $v = $_[$_];
        $self->[$i++] = $v if defined $parms[$_];
    }

    ## TODO: Call this on the main op after the parse is complete.
    $self->error_check;

    return $self;
}


sub clone {
    my $self = shift;
    return bless [ @{$self} ], ref $self;
}


sub error_check {
    ## Here to be overloaded...
}


sub field_name {
    my $pkg = ref shift;

    no strict "refs";
    ${"${pkg}::fields"}[shift];
}


sub as_graphviz {
    my $self = shift;
    require GraphViz;
    my $g = GraphViz->new();
    $self->_add_to_graphviz( $g );
    return $g;
}


=item as_png

    $d->as_png( "file.png" );
    $d->as_png( ">file.png" );
    $d->as_png( "| viewer_program" );

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


sub _add_to_graphviz {
    my $self = shift ;
    my ( $g ) = @_ ;

    my $type = ref $self;
    $type =~ s/.*?:://;

    my $i = 0;
    my $label = "{$type|{" .
        join( "|",
            map {
                my $name = $self->field_name( $i );
                ++$i;
                if ( defined ) {
                    if ( ! ref ) {
                        $name .= "\n" . ( defined $_ ? $_ : "<undef>" );
                    }
                    elsif ( ref eq "CODE" ) {
                        $name .= "\nCODE";
                    }
                    $name ||= "UNKNOWN";

                    my $port = "<port$i>";
                    $name =~ s{([<>\[\]\{\}|])}{\\$1}g;
                    if ( $name =~ s{\n}{|$port} ) {
                        $name = "{$name}";
                    }
                    else {
                        $name = "$port$name";
                    }
                }
                defined $_ ? $name : ();
            } @$self
        ) .  "}}";

    $g->add_node(
        shape    => "record",
        name     => int $self,
        label    => $label,
        font     => "Arial",
        fontsize => 10,
    );

    $i = 0;
    for ( @$self ) {
        ++$i;
        next unless ref;
        next if ref eq "CODE";
        $_->_add_to_graphviz( $g );
        $g->add_edge({
            from      => int $self,
            from_port => $i,
            to        => int $_,
        });
    }
}


###############################################################################

package XFD::doc_node;
use Carp;

BEGIN { XFD::_declare( qw( undef next ) ) }

sub bootstrap {
    my $self = shift;
    my ( $action, $ctx ) = @_;

    if ( $self->[_next] ) {
        $self->[_next]->bootstrap( $action );
    }
    else {
        $self->[_next] = $action;
    }

    push @{$ctx->{DocTests}}, $self if $ctx;
}


## Only called in doc_node context, so no actual check done.
sub check {
    my $self = shift;
    my ( $ctx, $new_ctx ) = @_;

    my $next = $self->[_next];
    UNIVERSAL::isa( $next, "XFD::Base" ) ? $next->check( @_ ) : $next;
}


sub install {
    croak "absolute paths like '/...' not allowed in expressions";
}


package XFD::step;
BEGIN { XFD::_declare( qw( axis node_test predicates _next ) ) }

## TODO: Have a subclass for each node type?  This would save us having
## to check ref $self->[_node_test] below.

use Carp;

my %supported_axes = (
    "descendant-or-self" => undef,
    "self"               => undef,
    "child"              => undef,
    "attribute"          => undef,
);

sub error_check {
    my $self = shift;

    my $axis = $self->[_axis];
    if ( defined $axis ) {
        croak "Axis $axis not supported"
            if ! exists $supported_axes{$axis};

        ## TODO: after predicates are implemented, see if we need to
        ## walk down the step hierarchy.
        croak "attribute:: step cannot be followed by a '/'"
            if $axis eq "attribute"
                && $self->[_next]
                && UNIVERSAL::isa( $self->[_next], "XFD::step" );
    }
}


sub bootstrap {
    my $self = shift;
    croak "step can't deal with bootstrapping a context" if @_ > 1;

    my ( $action ) = @_;
    if ( $self->[_next] ) {
        $self->[_next]->bootstrap( $action );
    }
    else {
        $self->[_next] = $action;
    }
}


sub set_next {
    my $self = shift;
    my ( $next ) = @_;
    if ( $self->[_next] ) {
        $self->[_next]->set_next( $next );
    }
    else {
        $self->[_next] = $next;
    }
}


sub check {
    my $self = shift;
    my ( $ctx, $new_ctx ) = @_;

    croak "Predicates not handled yet in location steps"
        if defined $self->[_predicates];

    my $axis = $self->[_axis] || "child";
    my $do_next_step;

    if ( $axis eq "descendant" ) {
        ## Create a clone for the next generation and adjust it so
        ## it will fire then.
        my $clone = $self->clone;
        $clone->[_axis] = "descendant-or-self";
        return $clone;
    }
    elsif ( $axis eq "descendant-or-self" ) {
        ## No need to clone, since ::step does not have any real
        ## state.
        ## Install manually and fall through so we can still do our
        ## own action if all the tests fit.
        $self->install( $new_ctx );
        $do_next_step = 1;
    }
    elsif ( $axis eq "child" ) {
        my $clone = $self->clone;
        $clone->[_axis] = "self";
        return $clone;
    }
    elsif ( $axis eq "self" ) {
        $do_next_step = 1;
    }
    elsif ( $axis eq "attribute" ) {
        if ( exists $ctx->{Elt} ) {
            ## Queue us up to actually look at the attrs.
            push @{$new_ctx->{AttrTests}}, $self;
            return undef;
        }
        return undef
            unless exists $ctx->{Attr};
    }
    else {
        croak "BUG: Unhandled axis: $axis";
    }

    my $node_test = $self->[_node_test];

    ## Predicates are likely to be hung off _next.
    if ( ref $node_test ) {
        return undef unless $node_test->is_desired_node_type( $ctx );
    }
    elsif ( $axis eq "attribute" ) {
        ## TODO: Namespaces etc.
        return undef unless $node_test eq "*"
            || $node_test eq $ctx->{Attr}->{Name};
    }
    else {
        ## TODO: Namespaces etc.
        return undef unless $node_test eq "*"
            || $node_test eq $ctx->{Elt}->{Name};
    }

    my $next = $self->[_next];
    return $next->check( @_ )
        if $do_next_step && UNIVERSAL::isa( $next, "XFD::Base" );

    return $next;
}


sub install {
    my $self = shift;
    my ( $new_ctx ) = @_;

    push @{$new_ctx->{EltTests}}, $self;
    undef;
}

package XFD::node_type;

BEGIN { XFD::_declare( qw( type ) ) }

sub is_desired_node_type {
    my $self = shift;
    my ( $node ) = @_;

    my $nt = $self->[_type];

    return 1 if $nt eq "node";

    return 0;
}

1;

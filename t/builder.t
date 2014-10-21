#!/usr/local/lib/perl -w

use strict;

use Carp;
use Test;
use XML::Filter::Dispatcher qw( :all );
use UNIVERSAL;

my $has_graph;

my $h;

my $ab = QB->new( "ab", <<'XML_END' );
<root a="A"><a aa1="AA1" aa2="AA2"><b>B1</b><b>B2</b></a></root>
XML_END

my $ns = QB->new( "ns", <<'XML_END' );
<root
    xmlns="default-ns"
    xmlns:foo="foo-ns"
    a="A"
    foo:a="FOOA"
><a aa1="AA1" foo:aa1="AA2"><b>B1</b><foo:b>B2</foo:b></a></root>
XML_END

my @tests = (
sub {
    $h = $ab->playback( XML::Filter::Dispatcher->new(
        Rules => [
            ## Any leaf nodes get stringified
            "//*" => [
                "string()" => sub {
                    push @{xpeek->{$_[1]->{LocalName}}}, xvalue;
                },
            ],

            ## This next one is where we'd construct contained objects.
            "//*[*]" => sub { xpush xpeek->{$_[1]->{LocalName}} = {} },

            ## This next one is where we'd construct the root object.
            "/*"              => sub { xpush {} },

            ## And here's where we return the root object
            "/end-element::*" => sub { xpop },
        ],
    ) );

    ok ref $h, "HASH";
},

sub { ok $h->{a}->{b}->[0], "B1" },
sub { ok $h->{a}->{b}->[1], "B2" },

);


plan tests => scalar @tests;

$_->() for @tests;


###############################################################################
##
## This quick little buffering filter is used to save us the overhead
## of a parse for each test.  This saves me sanity (since I run the test
## suite a lot), allows me to see which tests are noticably slower in
## case something pathalogical happens, and keeps admins from getting the
## impression that this is a slow package based on test suite speed.
package QB;
use vars qw( $AUTOLOAD );
use File::Basename;

sub new {
    my $self = bless [], shift;

    my ( $name, $doc ) = @_;

    my $cache_fn = basename( $0 ) . ".cache.$name";
    if ( -e $cache_fn && -M $cache_fn < -M $0 ) {
        my $old_self = do $cache_fn;
        return $old_self if defined $old_self;
        warn "$!$@";
        unlink $cache_fn;
    }

    require XML::SAX::PurePerl; ## Cannot use ParserFactory; LibXML 1.31 is broken.
    require Data::Dumper;
    my $p = XML::SAX::PurePerl->new( Handler => $self );
    $p->parse_string( $doc );
    if ( open F, ">$cache_fn" ) {
        local $Data::Dumper::Terse;
        $Data::Dumper::Terse = 1;
        print F Data::Dumper::Dumper( $self );
        close F;
    }

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
    my $r;
    for ( @$self ) {
        my $m = $_->[0];
        no strict "refs";
        $r = $h->$m( @{$_->[1]} ) if $h->can( $m );
    }
    return $r;
}

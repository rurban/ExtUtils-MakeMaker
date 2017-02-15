package ExtUtils::MM_Unix;

require 5.006;

use strict;

use ExtUtils::MakeMaker::Config;
use File::Basename qw(basename dirname);
use ExtUtils::MakeMaker qw($Verbose neatvalue _sprintf562);

# If we make $VERSION an our variable parse_version() breaks
use vars qw($VERSION);
$VERSION = '7.26';
$VERSION = eval $VERSION;  ## no critic [BuiltinFunctions::ProhibitStringyEval]

use ExtUtils::MM_Any;
our @ISA = qw( ExtUtils::MM_Any );


=head1 NAME

ExtUtils::MM_Unix - methods used by ExtUtils::MakeMaker

=head1 SYNOPSIS

C<require ExtUtils::MM_Unix;>

=head1 DESCRIPTION

The methods provided by this package are designed to be used in
conjunction with ExtUtils::MakeMaker. When MakeMaker writes a
Makefile, it creates one or more objects that inherit their methods
from a package C<MM>. MM itself doesn't provide any methods, but it
ISA ExtUtils::MM_Unix class. The inheritance tree of MM lets operating
specific packages take the responsibility for all the methods provided
by MM_Unix. We are trying to reduce the number of the necessary
overrides by defining rather primitive operations within
ExtUtils::MM_Unix.

If you are going to write a platform specific MM package, please try
to limit the necessary overrides to primitive methods, and if it is not
possible to do so, let's work out how to achieve that gain.

If you are overriding any of these methods in your Makefile.PL (in the
MY class), please report that to the makemaker mailing list. We are
trying to minimize the necessary method overrides and switch to data
driven Makefile.PLs wherever possible. In the long run less methods
will be overridable via the MY class.

=head1 METHODS

=over 4

=item os_flavor

Simply says that we're Unix.

=cut

sub os_flavor {
    return('Unix');
}

=item all_target  (override)

Build man pages, too

=cut

sub all_target {
    my $self = shift;

    return <<'MAKE_EXT';
all :: pure_all manifypods
	$(NOECHO) $(NOOP)
MAKE_EXT
}

=item init_tools (override)

Initializes tools to use their common (and faster) Unix commands.

=cut

sub init_tools {
    my $self = shift;

    $self->{ECHO}       ||= 'echo';
    $self->{ECHO_N}     ||= 'echo -n';
    $self->{RM_F}       ||= "rm -f";
    $self->{RM_RF}      ||= "rm -rf";
    $self->{TOUCH}      ||= "touch";
    $self->{TEST_F}     ||= "test -f";
    $self->{TEST_S}     ||= "test -s";
    $self->{CP}         ||= "cp";
    $self->{MV}         ||= "mv";
    $self->{CHMOD}      ||= "chmod";
    $self->{FALSE}      ||= 'false';
    $self->{TRUE}       ||= 'true';

    $self->{LD}         ||= 'ld';

    return $self->SUPER::init_tools(@_);

    # After SUPER::init_tools so $Config{shell} has a
    # chance to get set.
    $self->{SHELL}      ||= '/bin/sh';

    return;
}

=item init_DIRFILESEP (override)

Using / for Unix.  Called by EUMM->new.
Not needed here as MM_Any defaults to Unix.

=cut
  
#sub init_DIRFILESEP {
#    my($self) = @_;
#
#    $self->{DIRFILESEP} = '/';
#}

=item init_platform  (override)

=item platform_constants  (override)

Add MM_Unix_VERSION.

=cut

sub init_platform {
    my($self) = shift;

    $self->{MM_Unix_VERSION} = $VERSION;
    $self->{PERL_MALLOC_DEF} = '-DPERL_EXTMALLOC_DEF -Dmalloc=Perl_malloc '.
                               '-Dfree=Perl_mfree -Drealloc=Perl_realloc '.
                               '-Dcalloc=Perl_calloc';

}

sub platform_constants {
    my($self) = shift;
    my $make_frag = '';

    foreach my $macro (qw(MM_Unix_VERSION PERL_MALLOC_DEF))
    {
        next unless defined $self->{$macro};
        $make_frag .= "$macro = $self->{$macro}\n";
    }

    return $make_frag;
}

=item test_via_harness (override)

To be able to replicate run-time errors with Windows DLL's on lazy-loaded
shared libs, Unix machines need to have PERL_DL_NONLAZY set for tests.

This is only effective with HPUX and dlopen (Unix), setting dynaloader
binding to immediate, and not lazy/deferred. All symbols must be
resolvable immediately, which is the only supported binding mode on
Windows.

=cut

sub test_via_harness {
    my($self, $perl, $tests) = @_;
    return $self->SUPER::test_via_harness("PERL_DL_NONLAZY=1 $perl", $tests);
}

=item test_via_script (override)

Again, the PERL_DL_NONLAZY thing.

=cut

sub test_via_script {
    my($self, $perl, $script) = @_;
    return $self->SUPER::test_via_script("PERL_DL_NONLAZY=1 $perl", $script);
}

1;

=back

=head1 SEE ALSO

L<ExtUtils::MakeMaker>

=cut

__END__

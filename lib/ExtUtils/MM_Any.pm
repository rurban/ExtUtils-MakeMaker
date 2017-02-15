package ExtUtils::MM_Any;

use strict;
our $VERSION = '7.26';
$VERSION = eval $VERSION;

use Carp;
use File::Spec;
use File::Basename;
BEGIN { our @ISA = qw(File::Spec); }

# We need $Verbose
use ExtUtils::MakeMaker qw($Verbose neatvalue _sprintf562);
use ExtUtils::MakeMaker::Config;

our %Config_Override;

# So we don't have to keep calling the methods over and over again,
# we have these globals to cache the values.  Faster and shrtr.
my $Curdir  = __PACKAGE__->curdir;
my $Updir   = __PACKAGE__->updir;

my $METASPEC_URL = 'https://metacpan.org/pod/CPAN::Meta::Spec';
my $METASPEC_V = 2;

=head1 NAME

ExtUtils::MM_Any - Platform-agnostic MM methods

=head1 SYNOPSIS

  FOR INTERNAL USE ONLY!

  package ExtUtils::MM_SomeOS;

  require ExtUtils::MM_Any;
  @ISA = qw(ExtUtils::MM_Any);

=head1 DESCRIPTION

ExtUtils::MM_Any is a superclass for the ExtUtils::MM_* set of
modules.  It contains methods which are either inherently
cross-platform or are written in a cross-platform manner.

Subclass just off of ExtUtils::MM_Any, not also MM_Unix anymore.
MM_Any is not purely abstract anymore.

=head1 METHODS

The following description of methods is still under
development. Please refer to the code for not suitably documented
sections and complain loudly to the makemaker@perl.org mailing list.
Better yet, provide a patch.

Not all of the methods below are overridable in a
Makefile.PL. Overridable methods are marked as (o). All methods are
overridable by a platform specific MM_*.pm file.

Cross-platform methods are being moved into MM_Any.  If you can't find
something that used to be in here, look in MM_Unix.

=head2 Cross-platform helper methods

These are methods which help writing cross-platform code.

=over

=cut

my %Is;
BEGIN {
    $Is{OS2}     = $^O eq 'os2';
    $Is{Win32}   = $^O eq 'MSWin32' || $Config{osname} eq 'NetWare';
    $Is{Dos}     = $^O eq 'dos';
    $Is{VMS}     = $^O eq 'VMS';
    $Is{OSF}     = $^O eq 'dec_osf';
    $Is{IRIX}    = $^O eq 'irix';
    $Is{NetBSD}  = $^O eq 'netbsd';
    $Is{Interix} = $^O eq 'interix';
    $Is{SunOS4}  = $^O eq 'sunos';
    $Is{Solaris} = $^O eq 'solaris';
    $Is{SunOS}   = $Is{SunOS4} || $Is{Solaris};
    $Is{BSD}     = ($^O =~ /^(?:free|net|open)bsd$/ or
                   grep( $^O eq $_, qw(bsdos interix dragonfly) )
                  );
    $Is{Android} = $^O =~ /android/;
}

BEGIN {
    if( $Is{VMS} ) {
        # For things like vmsify()
        require VMS::Filespec;
        VMS::Filespec->import;
    }
}

=item os_flavor  I<Abstract>

    my @os_flavor = $mm->os_flavor;

@os_flavor is the style of operating system this is, usually
corresponding to the MM_*.pm file we're using.

The first element of @os_flavor is the major family (ie. Unix,
Windows, VMS, OS/2, etc...) and the rest are sub families.

Some examples:

    Cygwin98       ('Unix',  'Cygwin', 'Cygwin9x')
    Windows        ('Win32')
    Win98          ('Win32', 'Win9x')
    Linux          ('Unix',  'Linux')
    MacOS X        ('Unix',  'Darwin', 'MacOS', 'MacOS X')
    OS/2           ('OS/2')

This is used to write code for styles of operating system.
See os_flavor_is() for use.


=item os_flavor_is

    my $is_this_flavor = $mm->os_flavor_is($this_flavor);
    my $is_this_flavor = $mm->os_flavor_is(@one_of_these_flavors);

Checks to see if the current operating system is one of the given flavors.

This is useful for code like:

    if( $mm->os_flavor_is('Unix') ) {
        $out = `foo 2>&1`;
    }
    else {
        $out = `foo`;
    }

=cut

sub os_flavor_is {
    my ($self, @flavors) = @_;
    my %flavors = map { ($_ => 1) } $self->os_flavor;
    return (grep { $flavors{$_} } @flavors) ? 1 : 0;
}


=item can_load_xs

    my $can_load_xs = $self->can_load_xs;

Returns true if we have the ability to load XS.

This is important because miniperl, used to build XS modules in the
core, can not load XS.

=cut

sub can_load_xs {
    return defined &DynaLoader::boot_DynaLoader ? 1 : 0;
}


=item can_run

  use ExtUtils::MM;
  my $runnable = MM->can_run($Config{make});

If called in a scalar context it will return the full path to the binary
you asked for if it was found, or C<undef> if it was not.

If called in a list context, it will return a list of the full paths to instances
of the binary where found in C<PATH>, or an empty list if it was not found.

Copied from L<IPC::Cmd|IPC::Cmd/"$path = can_run( PROGRAM );">, but modified into
a method (and removed C<$INSTANCES> capability).

=cut

sub can_run {
    my ($self, $command) = @_;

    # a lot of VMS executables have a symbol defined
    # check those first
    if ( $^O eq 'VMS' ) {
        require VMS::DCLsym;
        my $syms = VMS::DCLsym->new;
        return $command if scalar $syms->getsym( uc $command );
    }

    my @possibles;

    if( File::Spec->file_name_is_absolute($command) ) {
        return $self->maybe_command($command);

    } else {
        for my $dir (
            File::Spec->path,
            File::Spec->curdir
        ) {
            next if ! $dir || ! -d $dir;
            my $abs = File::Spec->catfile($self->os_flavor_is('Win32') ? Win32::GetShortPathName( $dir ) : $dir, $command);
            push @possibles, $abs if $abs = $self->maybe_command($abs);
        }
    }
    return @possibles if wantarray;
    return shift @possibles;
}


=item can_redirect_error

  $useredirect = MM->can_redirect_error;

True if on an OS where qx operator (or backticks) can redirect C<STDERR>
onto C<STDOUT>.

=cut

sub can_redirect_error {
  my ($self) = @_;
  $self->os_flavor_is('Unix')
      or ($self->os_flavor_is('Win32') and !$self->os_flavor_is('Win9x'))
      or $self->os_flavor_is('OS/2')
}


=item is_make_type

    my $is_dmake = $self->is_make_type('dmake');

Returns true if C<<$self->make>> is the given type; possibilities are:

  gmake    GNU make
  dmake
  nmake
  bsdmake  BSD pmake-derived

=cut

my %maketype2true;
# undocumented - so t/cd.t can still do its thing
sub _clear_maketype_cache { %maketype2true = () }

sub is_make_type {
    my($self, $type) = @_;
    return $maketype2true{$type} if defined $maketype2true{$type};
    (undef, undef, my $make_basename) = $self->splitpath($self->make);
    return $maketype2true{$type} = 1
        if $make_basename =~ /\b$type\b/i; # executable's filename
    return $maketype2true{$type} = 0
        if $make_basename =~ /\b[gdn]make\b/i; # Never fall through for dmake/nmake/gmake
    # now have to run with "-v" and guess
    my $redirect = $self->can_redirect_error ? '2>&1' : '';
    my $make = $self->make || $self->{MAKE};
    my $minus_v = `"$make" -v $redirect`;
    return $maketype2true{$type} = 1
        if $type eq 'gmake' and $minus_v =~ /GNU make/i;
    return $maketype2true{$type} = 1
        if $type eq 'bsdmake'
      and $minus_v =~ /^usage: make \[-BeikNnqrstWwX\]/im;
    $maketype2true{$type} = 0; # it wasn't whatever you asked
}


=item can_dep_space

    my $can_dep_space = $self->can_dep_space;

Returns true if C<make> can handle (probably by quoting)
dependencies that contain a space. Currently known true for GNU make,
false for BSD pmake derivative.

=cut

my $cached_dep_space;
sub can_dep_space {
    my ($self) = @_;
    return $cached_dep_space if defined $cached_dep_space;
    return $cached_dep_space = 1 if $self->is_make_type('gmake');
    return $cached_dep_space = 0 if $self->is_make_type('dmake'); # only on W32
    return $cached_dep_space = 0 if $self->is_make_type('bsdmake');
    return $cached_dep_space = 0; # assume no
}


=item quote_dep

  $text = $mm->quote_dep($text);

Method that protects Makefile single-value constants (mainly filenames),
so that make will still treat them as single values even if they
inconveniently have spaces in. If the make program being used cannot
achieve such protection and the given text would need it, throws an
exception.

=cut

sub quote_dep {
    my ($self, $arg) = @_;
    die <<EOF if $arg =~ / / and not $self->can_dep_space;
Tried to use make dependency with space for make that can't:
  '$arg'
EOF
    $arg =~ s/( )/\\$1/g; # how GNU make does it
    return $arg;
}


=item split_command

    my @cmds = $MM->split_command($cmd, @args);

Most OS have a maximum command length they can execute at once.  Large
modules can easily generate commands well past that limit.  Its
necessary to split long commands up into a series of shorter commands.

C<split_command> will return a series of @cmds each processing part of
the args.  Collectively they will process all the arguments.  Each
individual line in @cmds will not be longer than the
$self->max_exec_len being careful to take into account macro expansion.

$cmd should include any switches and repeated initial arguments.

If no @args are given, no @cmds will be returned.

Pairs of arguments will always be preserved in a single command, this
is a heuristic for things like pm_to_blib and pod2man which work on
pairs of arguments.  This makes things like this safe:

    $self->split_command($cmd, %pod2man);


=cut

sub split_command {
    my($self, $cmd, @args) = @_;

    my @cmds = ();
    return(@cmds) unless @args;

    # If the command was given as a here-doc, there's probably a trailing
    # newline.
    chomp $cmd;

    # set aside 30% for macro expansion.
    my $len_left = int($self->max_exec_len * 0.70);
    $len_left -= length $self->_expand_macros($cmd);

    do {
        my $arg_str = '';
        my @next_args;
        while( @next_args = splice(@args, 0, 2) ) {
            # Two at a time to preserve pairs.
            my $next_arg_str = "\t  ". join ' ', @next_args, "\n";

            if( !length $arg_str ) {
                $arg_str .= $next_arg_str
            }
            elsif( length($arg_str) + length($next_arg_str) > $len_left ) {
                unshift @args, @next_args;
                last;
            }
            else {
                $arg_str .= $next_arg_str;
            }
        }
        chop $arg_str;

        push @cmds, $self->escape_newlines("$cmd \n$arg_str");
    } while @args;

    return @cmds;
}


sub _expand_macros {
    my($self, $cmd) = @_;

    $cmd =~ s{\$\((\w+)\)}{
        defined $self->{$1} ? $self->{$1} : "\$($1)"
    }e;
    return $cmd;
}


=item make_type

Returns a suitable string describing the type of makefile being written.

=cut

# override if this isn't suitable!
sub make_type { return 'Unix-style'; }


=item stashmeta

    my @recipelines = $MM->stashmeta($text, $file);

Generates a set of C<@recipelines> which will result in the literal
C<$text> ending up in literal C<$file> when the recipe is executed. Call
it once, with all the text you want in C<$file>. Make macros will not
be expanded, so the locations will be fixed at configure-time, not
at build-time.

=cut

sub stashmeta {
    my($self, $text, $file) = @_;
    $self->echo($text, $file, { allow_variables => 0, append => 0 });
}


=item echo

    my @commands = $MM->echo($text);
    my @commands = $MM->echo($text, $file);
    my @commands = $MM->echo($text, $file, \%opts);

Generates a set of @commands which print the $text to a $file.

If $file is not given, output goes to STDOUT.

If $opts{append} is true the $file will be appended to rather than
overwritten.  Default is to overwrite.

If $opts{allow_variables} is true, make variables of the form
C<$(...)> will not be escaped.  Other C<$> will.  Default is to escape
all C<$>.

Example of use:

    my $make = join '', map "\t$_\n", $MM->echo($text, $file);

=cut

sub echo {
    my($self, $text, $file, $opts) = @_;

    # Compatibility with old options
    if( !ref $opts ) {
        my $append = $opts;
        $opts = { append => $append || 0 };
    }
    $opts->{allow_variables} = 0 unless defined $opts->{allow_variables};

    my $ql_opts = { allow_variables => $opts->{allow_variables} };
    my @cmds = map { '$(NOECHO) $(ECHO) '.$self->quote_literal($_, $ql_opts) }
               split /\n/, $text;
    if( $file ) {
        my $redirect = $opts->{append} ? '>>' : '>';
        $cmds[0] .= " $redirect $file";
        $_ .= " >> $file" foreach @cmds[1..$#cmds];
    }

    return @cmds;
}


=item wraplist

  my $args = $mm->wraplist(@list);

Takes an array of items and turns them into a well-formatted list of
arguments.  In most cases this is simply something like:

    FOO \
    BAR \
    BAZ

=cut

sub wraplist {
    my $self = shift;
    return join " \\\n\t", @_;
}


=item maketext_filter

    my $filter_make_text = $mm->maketext_filter($make_text);

The text of the Makefile is run through this method before writing to
disk.  It allows systems a chance to make portability fixes to the
Makefile.

By default it does nothing.

This method is protected and not intended to be called outside of
MakeMaker.

=cut

sub maketext_filter { return $_[1] }


=item cd  I<Abstract>

  my $subdir_cmd = $MM->cd($subdir, @cmds);

This will generate a make fragment which runs the @cmds in the given
$dir.  The rough equivalent to this, except cross platform.

  cd $subdir && $cmd

Currently $dir can only go down one level.  "foo" is fine.  "foo/bar" is
not.  "../foo" is right out.

The resulting $subdir_cmd has no leading tab nor trailing newline.  This
makes it easier to embed in a make string.  For example.

      my $make = sprintf <<'CODE', $subdir_cmd;
  foo :
      $(ECHO) what
      %s
      $(ECHO) mouche
  CODE


=item oneliner  I<Abstract>

  my $oneliner = $MM->oneliner($perl_code);
  my $oneliner = $MM->oneliner($perl_code, \@switches);

This will generate a perl one-liner safe for the particular platform
you're on based on the given $perl_code and @switches (a -e is
assumed) suitable for using in a make target.  It will use the proper
shell quoting and escapes.

$(PERLRUN) will be used as perl.

Any newlines in $perl_code will be escaped.  Leading and trailing
newlines will be stripped.  Makes this idiom much easier:

    my $code = $MM->oneliner(<<'CODE', [...switches...]);
some code here
another line here
CODE

Usage might be something like:

    # an echo emulation
    $oneliner = $MM->oneliner('print "Foo\n"');
    $make = '$oneliner > somefile';

Dollar signs in the $perl_code will be protected from make using the
C<quote_literal> method, unless they are recognised as being a make
variable, C<$(varname)>, in which case they will be left for make
to expand. Remember to quote make macros else it might be used as a
bareword. For example:

    # Assign the value of the $(VERSION_FROM) make macro to $vf.
    $oneliner = $MM->oneliner('$vf = "$(VERSION_FROM)"');

Its currently very simple and may be expanded sometime in the figure
to include more flexible code and switches.


=item escape_dollarsigns

    my $escaped_text = $MM->escape_dollarsigns($text);

Escapes stray C<$> so they are not interpreted as make variables.

It lets by C<$(...)>.

=cut

sub escape_dollarsigns {
    my($self, $text) = @_;

    # Escape dollar signs which are not starting a variable
    $text =~ s{\$ (?!\() }{\$\$}gx;

    return $text;
}


=item escape_all_dollarsigns

    my $escaped_text = $MM->escape_all_dollarsigns($text);

Escapes all C<$> so they are not interpreted as make variables.

=cut

sub escape_all_dollarsigns {
    my($self, $text) = @_;

    # Escape dollar signs
    $text =~ s{\$}{\$\$}gx;

    return $text;
}


=item escape_newlines  I<Abstract>

    my $escaped_text = $MM->escape_newlines($text);

Shell escapes newlines in $text.


=item max_exec_len  I<Abstract>

    my $max_exec_len = $MM->max_exec_len;

Calculates the maximum command size the OS can exec.  Effectively,
this is the max size of a shell command line.

=for _private
$self->{_MAX_EXEC_LEN} is set by this method, but only for testing purposes.


=item make

    my $make = $MM->make;

Returns the make variant we're generating the Makefile for.  This attempts
to do some normalization on the information from %Config or the user.

=cut

sub make {
    my ($self) = @_;

    my $make = lc $self->{MAKE};

    # Truncate anything like foomake6 to just foomake.
    $make =~ s/^(\w+make).*/$1/;

    # Turn gnumake into gmake.
    $make =~ s/^gnu/g/;

    return $make;
}

=back

=head2 Targets

These are methods which produce make targets.

=over

=item all_target

Generate the default target 'all'.

=cut

sub all_target {
    my $self = shift;

    return <<'MAKE_EXT';
all :: pure_all
	$(NOECHO) $(NOOP)
MAKE_EXT

}

=item c_o (o)

Defines the suffix rules to compile different flavors of C files to
object files.

=cut

sub c_o {
# --- Translation Sections ---

    my ($self) = @_;
    return '' unless $self->needs_linking();
    my(@m);

    my $command = '$(CCCMD)';
    my $flags   = '$(CCCDLFLAGS) "-I$(PERL_INC)" $(PASTHRU_DEFINE) $(DEFINE)';

    if (my $cpp = $Config{cpprun}) {
        my $cpp_cmd = $self->const_cccmd;
        $cpp_cmd =~ s/^CCCMD\s*=\s*\$\(CC\)/$cpp/;
        push @m, qq{
.c.i:
	$cpp_cmd $flags \$*.c > \$*.i
};
    }

    my $m_o = $self->{XSMULTI} ? $self->xs_obj_opt('$*.s') : '';
    push @m, sprintf <<'EOF', $command, $flags, $m_o;

.c.s :
	%s -S %s $*.c %s
EOF

    my @exts = qw(c cpp cxx cc);
    push @exts, 'C' if !$Is{OS2} and !$Is{Win32} and !$Is{Dos}; #Case-specific
    $m_o = $self->{XSMULTI} ? $self->xs_obj_opt('$*$(OBJ_EXT)') : '';
    for my $ext (@exts) {
	push @m, "\n.$ext\$(OBJ_EXT) :\n\t$command $flags \$*.$ext" . ( $m_o ? " $m_o" : '' ) . "\n";
    }
    return join "", @m;
}


=item xs_obj_opt

Takes the object file as an argument, and returns the portion of compile
command-line that will output to the specified object file.

=cut

sub xs_obj_opt {
    my ($self, $output_file) = @_;
    "-o $output_file";
}


=item cflags (o)

Does very much the same as the cflags script in the perl
distribution. It doesn't return the whole compiler command line, but
initializes all of its parts. The const_cccmd method then actually
returns the definition of the CCCMD macro which uses these parts.

=cut

#'

sub cflags {
    my($self,$libperl)=@_;
    return $self->{CFLAGS} if $self->{CFLAGS};
    return '' unless $self->needs_linking();

    my($prog, $uc, $perltype, %cflags);
    $libperl ||= $self->{LIBPERL_A} || "libperl$self->{LIB_EXT}" ;
    $libperl =~ s/\.\$\(A\)$/$self->{LIB_EXT}/;

    @cflags{qw(cc ccflags optimize shellflags)}
	= @Config{qw(cc ccflags optimize shellflags)};

    # Perl 5.21.4 adds the (gcc) warning (-Wall ...) and std (-std=c89)
    # flags to the %Config, and the modules in the core should be built
    # with the warning flags, but NOT the -std=c89 flags (the latter
    # would break using any system header files that are strict C99).
    my @ccextraflags = qw(ccwarnflags);
    if ($ENV{PERL_CORE}) {
      for my $x (@ccextraflags) {
        if (exists $Config{$x}) {
          $cflags{$x} = $Config{$x};
        }
      }
    }

    my($optdebug) = "";

    $cflags{shellflags} ||= '';

    my(%map) =  (
		D =>   '-DDEBUGGING',
		E =>   '-DEMBED',
		DE =>  '-DDEBUGGING -DEMBED',
		M =>   '-DEMBED -DMULTIPLICITY',
		DM =>  '-DDEBUGGING -DEMBED -DMULTIPLICITY',
		);

    if ($libperl =~ /libperl(\w*)\Q$self->{LIB_EXT}/){
	$uc = uc($1);
    } else {
	$uc = ""; # avoid warning
    }
    $perltype = $map{$uc} ? $map{$uc} : "";

    if ($uc =~ /^D/) {
	$optdebug = "-g";
    }


    my($name);
    ( $name = $self->{NAME} . "_cflags" ) =~ s/:/_/g ;
    if ($prog = $Config{$name}) {
	# Expand hints for this extension via the shell
	print "Processing $name hint:\n" if $Verbose;
	my(@o)=`cc=\"$cflags{cc}\"
	  ccflags=\"$cflags{ccflags}\"
	  optimize=\"$cflags{optimize}\"
	  perltype=\"$cflags{perltype}\"
	  optdebug=\"$cflags{optdebug}\"
	  eval '$prog'
	  echo cc=\$cc
	  echo ccflags=\$ccflags
	  echo optimize=\$optimize
	  echo perltype=\$perltype
	  echo optdebug=\$optdebug
	  `;
	foreach my $line (@o){
	    chomp $line;
	    if ($line =~ /(.*?)=\s*(.*)\s*$/){
		$cflags{$1} = $2;
		print "	$1 = $2\n" if $Verbose;
	    } else {
		print "Unrecognised result from hint: '$line'\n";
	    }
	}
    }

    if ($optdebug) {
	$cflags{optimize} = $optdebug;
    }

    for (qw(ccflags optimize perltype)) {
        $cflags{$_} ||= '';
	$cflags{$_} =~ s/^\s+//;
	$cflags{$_} =~ s/\s+/ /g;
	$cflags{$_} =~ s/\s+$//;
	$self->{uc $_} ||= $cflags{$_};
    }

    if ($self->{POLLUTE}) {
	$self->{CCFLAGS} .= ' -DPERL_POLLUTE ';
    }

    for my $x (@ccextraflags) {
      next unless exists $cflags{$x};
      $self->{CCFLAGS} .= $cflags{$x} =~ m!^\s! ? $cflags{$x} : ' ' . $cflags{$x};
    }

    my $pollute = '';
    if ($Config{usemymalloc} and not $Config{bincompat5005}
	and not $Config{ccflags} =~ /-DPERL_POLLUTE_MALLOC\b/
	and $self->{PERL_MALLOC_OK}) {
	$pollute = '$(PERL_MALLOC_DEF)';
    }

    return $self->{CFLAGS} = qq{
CCFLAGS = $self->{CCFLAGS}
OPTIMIZE = $self->{OPTIMIZE}
PERLTYPE = $self->{PERLTYPE}
MPOLLUTE = $pollute
};

}


=item const_cccmd (o)

Returns the full compiler call for C programs and stores the
definition in CONST_CCCMD.

=cut

sub const_cccmd {
    my($self,$libperl)=@_;
    return $self->{CONST_CCCMD} if $self->{CONST_CCCMD};
    return '' unless $self->needs_linking();
    return $self->{CONST_CCCMD} =
	q{CCCMD = $(CC) -c $(PASTHRU_INC) $(INC) \\
	$(CCFLAGS) $(OPTIMIZE) \\
	$(PERLTYPE) $(MPOLLUTE) $(DEFINE_VERSION) \\
	$(XS_DEFINE_VERSION)};
}

=item const_config (o)

Sets SHELL if needed, then defines a couple of constants in the Makefile
that are imported from %Config.

=cut

sub const_config {
# --- Constants Sections ---

    my ($self) = @_;
    my @m = $self->specify_shell(); # Usually returns empty string
    push @m, <<"END";

# These definitions are from config.sh (via $INC{'Config.pm'}).
# They may have been overridden via Makefile.PL or on the command line.
END

    my(%once_only);
    foreach my $key (@{$self->{CONFIG}}){
        # SITE*EXP macros are defined in &constants; avoid duplicates here
        next if $once_only{$key};
        push @m, uc($key) , ' = ' , $self->{uc $key}, "\n";
        $once_only{$key} = 1;
    }
    join('', @m);
}

=item const_loadlibs (o)

Defines EXTRALIBS, LDLOADLIBS, BSLOADLIBS, LD_RUN_PATH. See
L<ExtUtils::Liblist> for details.

=cut

sub const_loadlibs {
    my ($self) = @_;
    return "" unless $self->needs_linking;
    my @m;
    push @m, qq{
# $self->{NAME} might depend on some other libraries:
# See ExtUtils::Liblist for details
#
};
    for my $tmp (qw/
         EXTRALIBS LDLOADLIBS BSLOADLIBS
         /) {
        next unless defined $self->{$tmp};
        push @m, "$tmp = $self->{$tmp}\n";
    }
    # don't set LD_RUN_PATH if empty
    for my $tmp (qw/
         LD_RUN_PATH
         /) {
        next unless $self->{$tmp};
        push @m, "$tmp = $self->{$tmp}\n";
    }
    return join "", @m;
}

=item constants (o)

  my $make_frag = $mm->constants;

Prints out macros for lots of constants.

=cut

sub constants {
    my($self) = @_;
    my @m = ();

    $self->{DFSEP} = '$(DIRFILESEP)';  # alias for internal use

    for my $macro (qw(

              AR_STATIC_ARGS DIRFILESEP DFSEP
              NAME NAME_SYM
              VERSION    VERSION_MACRO    VERSION_SYM DEFINE_VERSION
              XS_VERSION XS_VERSION_MACRO             XS_DEFINE_VERSION
              INST_ARCHLIB INST_SCRIPT INST_BIN INST_LIB
              INST_MAN1DIR INST_MAN3DIR
              MAN1EXT      MAN3EXT
              INSTALLDIRS INSTALL_BASE DESTDIR PREFIX
              PERLPREFIX      SITEPREFIX      VENDORPREFIX
                   ),
                   (map { ("INSTALL".$_,
                          "DESTINSTALL".$_)
                        } $self->installvars),
                   qw(
              PERL_LIB
              PERL_ARCHLIB PERL_ARCHLIBDEP
              LIBPERL_A MYEXTLIB
              FIRST_MAKEFILE MAKEFILE_OLD MAKE_APERL_FILE
              PERLMAINCC PERL_SRC PERL_INC PERL_INCDEP
              PERL            FULLPERL          ABSPERL
              PERLRUN         FULLPERLRUN       ABSPERLRUN
              PERLRUNINST     FULLPERLRUNINST   ABSPERLRUNINST
              PERL_CORE
              PERM_DIR PERM_RW PERM_RWX

	      ) )
    {
	next unless defined $self->{$macro};

        # pathnames can have sharp signs in them; escape them so
        # make doesn't think it is a comment-start character.
        $self->{$macro} =~ s/#/\\#/g;
	$self->{$macro} = $self->quote_dep($self->{$macro})
	  if $ExtUtils::MakeMaker::macro_dep{$macro};
	push @m, "$macro = $self->{$macro}\n";
    }

    push @m, qq{
MAKEMAKER   = $self->{MAKEMAKER}
MM_VERSION  = $self->{MM_VERSION}
MM_REVISION = $self->{MM_REVISION}
};

    push @m, q{
# FULLEXT = Pathname for extension directory (eg Foo/Bar/Oracle).
# BASEEXT = Basename part of FULLEXT. May be just equal FULLEXT. (eg Oracle)
# PARENT_NAME = NAME without BASEEXT and no trailing :: (eg Foo::Bar)
# DLBASE  = Basename part of dynamic library. May be just equal BASEEXT.
};

    for my $macro (qw/
              MAKE
	      FULLEXT BASEEXT PARENT_NAME DLBASE VERSION_FROM INC DEFINE OBJECT
	      LDFROM LINKTYPE BOOTDEP
	      /	)
    {
	next unless defined $self->{$macro};
	push @m, "$macro = $self->{$macro}\n";
    }

    push @m, "
# Handy lists of source code files:
XS_FILES = ".$self->wraplist(sort keys %{$self->{XS}})."
C_FILES  = ".$self->wraplist(sort @{$self->{C}})."
O_FILES  = ".$self->wraplist(sort @{$self->{O_FILES}})."
H_FILES  = ".$self->wraplist(sort @{$self->{H}})."
MAN1PODS = ".$self->wraplist(sort keys %{$self->{MAN1PODS}})."
MAN3PODS = ".$self->wraplist(sort keys %{$self->{MAN3PODS}})."
";


    push @m, q{
# Where is the Config information that we are using/depend on
CONFIGDEP = $(PERL_ARCHLIBDEP)$(DFSEP)Config.pm $(PERL_INCDEP)$(DFSEP)config.h
} if -e $self->catfile( $self->{PERL_INC}, 'config.h' );


    push @m, qq{
# Where to build things
INST_LIBDIR      = $self->{INST_LIBDIR}
INST_ARCHLIBDIR  = $self->{INST_ARCHLIBDIR}

INST_AUTODIR     = $self->{INST_AUTODIR}
INST_ARCHAUTODIR = $self->{INST_ARCHAUTODIR}

INST_STATIC      = $self->{INST_STATIC}
INST_DYNAMIC     = $self->{INST_DYNAMIC}
INST_BOOT        = $self->{INST_BOOT}
};

    push @m, qq{
# Extra linker info
EXPORT_LIST        = $self->{EXPORT_LIST}
PERL_ARCHIVE       = $self->{PERL_ARCHIVE}
PERL_ARCHIVEDEP    = $self->{PERL_ARCHIVEDEP}
PERL_ARCHIVE_AFTER = $self->{PERL_ARCHIVE_AFTER}
};

    push @m, "

TO_INST_PM = ".$self->wraplist(map $self->quote_dep($_), sort keys %{$self->{PM}})."\n";

    join('',@m);
}


=item depend (o)

Same as macro for the depend attribute.

=cut

sub depend {
    my($self,%attribs) = @_;
    my(@m,$key,$val);
    for my $key (sort keys %attribs){
	my $val = $attribs{$key};
	next unless defined $key and defined $val;
	push @m, "$key : $val\n";
    }
    join "", @m;
}


=item dist (o)

  my $dist_macros = $mm->dist(%overrides);

Generates a make fragment defining all the macros initialized in
init_dist.

%overrides can be used to override any of the above.

=cut

sub dist {
    my($self, %attribs) = @_;

    my $make = '';
    if ( $attribs{SUFFIX} && $attribs{SUFFIX} !~ m!^\.! ) {
      $attribs{SUFFIX} = '.' . $attribs{SUFFIX};
    }
    foreach my $key (qw(
            TAR TARFLAGS ZIP ZIPFLAGS COMPRESS SUFFIX SHAR
            PREOP POSTOP TO_UNIX
            CI RCS_LABEL DIST_CP DIST_DEFAULT
            DISTNAME DISTVNAME
           ))
    {
        my $value = $attribs{$key} || $self->{$key};
        $make .= "$key = $value\n";
    }

    return $make;
}

=item dist_basics (o)

Defines the targets distclean, distcheck, skipcheck, manifest, veryclean.

=cut

sub dist_basics {
    my($self) = shift;

    return <<'MAKE_FRAG';
distclean :: realclean distcheck
	$(NOECHO) $(NOOP)

distcheck :
	$(PERLRUN) "-MExtUtils::Manifest=fullcheck" -e fullcheck

skipcheck :
	$(PERLRUN) "-MExtUtils::Manifest=skipcheck" -e skipcheck

manifest :
	$(PERLRUN) "-MExtUtils::Manifest=mkmanifest" -e mkmanifest

veryclean : realclean
	$(RM_F) *~ */*~ *.orig */*.orig *.bak */*.bak *.old */*.old

MAKE_FRAG

}

=item dist_ci (o)

Defines a check in target for RCS.

=cut

sub dist_ci {
    my($self) = shift;
    return sprintf "ci :\n\t%s\n", $self->oneliner(<<'EOF', [qw(-MExtUtils::Manifest=maniread)]);
@all = sort keys %{ maniread() };
print(qq{Executing $(CI) @all\n});
system(qq{$(CI) @all}) == 0 or die $!;
print(qq{Executing $(RCS_LABEL) ...\n});
system(qq{$(RCS_LABEL) @all}) == 0 or die $!;
EOF
}

=item dist_core (o)

  my $dist_make_fragment = $MM->dist_core;

Puts the targets necessary for 'make dist' together into one make
fragment.

=cut

sub dist_core {
    my($self) = shift;

    my $make_frag = '';
    foreach my $target (qw(dist tardist uutardist tarfile zipdist zipfile
                           shdist))
    {
        my $method = $target.'_target';
        $make_frag .= "\n";
        $make_frag .= $self->$method();
    }

    return $make_frag;
}


=item B<dist_target>

  my $make_frag = $MM->dist_target;

Returns the 'dist' target to make an archive for distribution.  This
target simply checks to make sure the Makefile is up-to-date and
depends on $(DIST_DEFAULT).

=cut

sub dist_target {
    my($self) = shift;

    my $date_check = $self->oneliner(<<'CODE', ['-l']);
print 'Warning: Makefile possibly out of date with $(VERSION_FROM)'
    if -e '$(VERSION_FROM)' and -M '$(VERSION_FROM)' < -M '$(FIRST_MAKEFILE)';
CODE

    return sprintf <<'MAKE_FRAG', $date_check;
dist : $(DIST_DEFAULT) $(FIRST_MAKEFILE)
	$(NOECHO) %s
MAKE_FRAG
}

=item B<tardist_target>

  my $make_frag = $MM->tardist_target;

Returns the 'tardist' target which is simply so 'make tardist' works.
The real work is done by the dynamically named tardistfile_target()
method, tardist should have that as a dependency.

=cut

sub tardist_target {
    my($self) = shift;

    return <<'MAKE_FRAG';
tardist : $(DISTVNAME).tar$(SUFFIX)
	$(NOECHO) $(NOOP)
MAKE_FRAG
}

=item B<zipdist_target>

  my $make_frag = $MM->zipdist_target;

Returns the 'zipdist' target which is simply so 'make zipdist' works.
The real work is done by the dynamically named zipdistfile_target()
method, zipdist should have that as a dependency.

=cut

sub zipdist_target {
    my($self) = shift;

    return <<'MAKE_FRAG';
zipdist : $(DISTVNAME).zip
	$(NOECHO) $(NOOP)
MAKE_FRAG
}

=item B<tarfile_target>

  my $make_frag = $MM->tarfile_target;

The name of this target is the name of the tarball generated by
tardist.  This target does the actual work of turning the distdir into
a tarball.

=cut

sub tarfile_target {
    my($self) = shift;

    return <<'MAKE_FRAG';
$(DISTVNAME).tar$(SUFFIX) : distdir
	$(PREOP)
	$(TO_UNIX)
	$(TAR) $(TARFLAGS) $(DISTVNAME).tar $(DISTVNAME)
	$(RM_RF) $(DISTVNAME)
	$(COMPRESS) $(DISTVNAME).tar
	$(NOECHO) $(ECHO) 'Created $(DISTVNAME).tar$(SUFFIX)'
	$(POSTOP)
MAKE_FRAG
}

=item zipfile_target

  my $make_frag = $MM->zipfile_target;

The name of this target is the name of the zip file generated by
zipdist.  This target does the actual work of turning the distdir into
a zip file.

=cut

sub zipfile_target {
    my($self) = shift;

    return <<'MAKE_FRAG';
$(DISTVNAME).zip : distdir
	$(PREOP)
	$(ZIP) $(ZIPFLAGS) $(DISTVNAME).zip $(DISTVNAME)
	$(RM_RF) $(DISTVNAME)
	$(NOECHO) $(ECHO) 'Created $(DISTVNAME).zip'
	$(POSTOP)
MAKE_FRAG
}

=item uutardist_target

  my $make_frag = $MM->uutardist_target;

Converts the tarfile into a uuencoded file

=cut

sub uutardist_target {
    my($self) = shift;

    return <<'MAKE_FRAG';
uutardist : $(DISTVNAME).tar$(SUFFIX)
	uuencode $(DISTVNAME).tar$(SUFFIX) $(DISTVNAME).tar$(SUFFIX) > $(DISTVNAME).tar$(SUFFIX)_uu
	$(NOECHO) $(ECHO) 'Created $(DISTVNAME).tar$(SUFFIX)_uu'
MAKE_FRAG
}


=item shdist_target

  my $make_frag = $MM->shdist_target;

Converts the distdir into a shell archive.

=cut

sub shdist_target {
    my($self) = shift;

    return <<'MAKE_FRAG';
shdist : distdir
	$(PREOP)
	$(SHAR) $(DISTVNAME) > $(DISTVNAME).shar
	$(RM_RF) $(DISTVNAME)
	$(NOECHO) $(ECHO) 'Created $(DISTVNAME).shar'
	$(POSTOP)
MAKE_FRAG
}


=item dlsyms (o)

Used by some OS' to define DL_FUNCS and DL_VARS and write the *.exp files.

Normally just returns an empty string.

=cut

sub dlsyms {
    return '';
}


=item dynamic_bs (o)

Defines targets for bootstrap files.

=cut

sub dynamic_bs {
    my($self, %attribs) = @_;
    return "\nBOOTSTRAP =\n" unless $self->has_link_code();
    my @exts;
    if ($self->{XSMULTI}) {
	@exts = $self->_xs_list_basenames;
    } else {
	@exts = '$(BASEEXT)';
    }
    return join "\n",
        "BOOTSTRAP = @{[map { qq{$_.bs} } @exts]}\n",
        map { $self->_xs_make_bs($_) } @exts;
}

sub _xs_make_bs {
    my ($self, $basename) = @_;
    my ($v, $d, $f) = File::Spec->splitpath($basename);
    my @d = File::Spec->splitdir($d);
    shift @d if $self->{XSMULTI} and $d[0] eq 'lib';
    my $instdir = $self->catdir('$(INST_ARCHLIB)', 'auto', @d, $f);
    $instdir = '$(INST_ARCHAUTODIR)' if $basename eq '$(BASEEXT)';
    my $instfile = $self->catfile($instdir, "$f.bs");
    my $exists = "$instdir\$(DFSEP).exists"; # match blibdirs_target
    #                                 1          2          3
    return _sprintf562 <<'MAKE_FRAG', $basename, $instfile, $exists;
# As Mkbootstrap might not write a file (if none is required)
# we use touch to prevent make continually trying to remake it.
# The DynaLoader only reads a non-empty file.
%1$s.bs : $(FIRST_MAKEFILE) $(BOOTDEP)
	$(NOECHO) $(ECHO) "Running Mkbootstrap for %1$s ($(BSLOADLIBS))"
	$(NOECHO) $(PERLRUN) \
		"-MExtUtils::Mkbootstrap" \
		-e "Mkbootstrap('%1$s','$(BSLOADLIBS)');"
	$(NOECHO) $(TOUCH) "%1$s.bs"
	$(CHMOD) $(PERM_RW) "%1$s.bs"

%2$s : %1$s.bs %3$s
	$(NOECHO) $(RM_RF) %2$s
	- $(CP_NONEMPTY) %1$s.bs %2$s $(PERM_RW)
MAKE_FRAG
}

=item dynamic_lib (o)

Defines how to produce the *.so (or equivalent) files.

=cut

sub dynamic_lib {
    my($self, %attribs) = @_;
    return '' unless $self->needs_linking(); #might be because of a subdir
    return '' unless $self->has_link_code;
    my @m = $self->xs_dynamic_lib_macros(\%attribs);
    my @libs;
    if ($self->{XSMULTI}) {
        my @exts = $self->_xs_list_basenames;
        for my $ext (@exts) {
            my ($v, $d, $f) = File::Spec->splitpath($ext);
            my @d = File::Spec->splitdir($d);
            shift @d if $d[0] eq 'lib';
            my $instdir = $self->catdir('$(INST_ARCHLIB)', 'auto', @d, $f);

            # Dynamic library names may need special handling.
            eval { require DynaLoader };
            if (defined &DynaLoader::mod2fname) {
                $f = &DynaLoader::mod2fname([@d, $f]);
            }

            my $instfile = $self->catfile($instdir, "$f.\$(DLEXT)");
            my $objfile = $self->_xsbuild_value('xs', $ext, 'OBJECT');
            $objfile = "$ext\$(OBJ_EXT)" unless defined $objfile;
            my $ldfrom = $self->_xsbuild_value('xs', $ext, 'LDFROM');
            $ldfrom = $objfile unless defined $ldfrom;
            my $exportlist = "$ext.def";
            push @libs, [ $objfile, $instfile, $instdir, $ldfrom, $exportlist ];
        }
    } else {
        @libs = ([ qw($(OBJECT) $(INST_DYNAMIC) $(INST_ARCHAUTODIR) $(LDFROM) $(EXPORT_LIST)) ]);
    }
    push @m, map { $self->xs_make_dynamic_lib(\%attribs, @$_); } @libs;

    return join("\n",@m);
}

=item xs_dynamic_lib_macros

Defines the macros for the C<dynamic_lib> section.

=cut

sub xs_dynamic_lib_macros {
    my ($self, $attribs) = @_;
    my $otherldflags = $attribs->{OTHERLDFLAGS} || "";
    my $inst_dynamic_dep = $attribs->{INST_DYNAMIC_DEP} || "";
    my $armaybe = $self->_xs_armaybe($attribs);
    my $ld_opt = $Is{OS2} ? '$(OPTIMIZE) ' : ''; # Useful on other systems too?
    my $ld_fix = $Is{OS2} ? '|| ( $(RM_F) $@ && sh -c false )' : '';
    sprintf <<'EOF', $armaybe, $ld_opt.$otherldflags, $inst_dynamic_dep, $ld_fix;
# This section creates the dynamically loadable objects from relevant
# objects and possibly $(MYEXTLIB).
ARMAYBE = %s
OTHERLDFLAGS = %s
INST_DYNAMIC_DEP = %s
INST_DYNAMIC_FIX = %s
EOF
}

sub _xs_armaybe {
    my ($self, $attribs) = @_;
    my $armaybe = $attribs->{ARMAYBE} || $self->{ARMAYBE} || ":";
    $armaybe = 'ar' if ($Is{OSF} and $armaybe eq ':');
    $armaybe;
}

=item xs_make_dynamic_lib

Defines the recipes for the C<dynamic_lib> section.

=cut

sub xs_make_dynamic_lib {
    my ($self, $attribs, $object, $to, $todir, $ldfrom, $exportlist) = @_;
    $exportlist = '' if $exportlist ne '$(EXPORT_LIST)';
    my $armaybe = $self->_xs_armaybe($attribs);
    my @m = sprintf '%s : %s $(MYEXTLIB) %s$(DFSEP).exists %s $(PERL_ARCHIVEDEP) $(PERL_ARCHIVE_AFTER) $(INST_DYNAMIC_DEP)'."\n", $to, $object, $todir, $exportlist;
    if ($armaybe ne ':'){
        $ldfrom = 'tmp$(LIB_EXT)';
        push(@m,"	\$(ARMAYBE) cr $ldfrom $object\n");
        push(@m,"	\$(RANLIB) $ldfrom\n");
    }
    $ldfrom = "-all $ldfrom -none" if $Is{OSF};

    # The IRIX linker doesn't use LD_RUN_PATH
    my $ldrun = $Is{IRIX} && $self->{LD_RUN_PATH} ?
                       qq{-rpath "$self->{LD_RUN_PATH}"} : '';

    # For example in AIX the shared objects/libraries from previous builds
    # linger quite a while in the shared dynalinker cache even when nobody
    # is using them.  This is painful if one for instance tries to restart
    # a failed build because the link command will fail unnecessarily 'cos
    # the shared object/library is 'busy'.
    push(@m,"	\$(RM_F) \$\@\n");

    my $libs = '$(LDLOADLIBS)';
    if (($Is{NetBSD} || $Is{Interix} || $Is{Android}) && $Config{'useshrplib'} eq 'true') {
        # Use nothing on static perl platforms, and to the flags needed
        # to link against the shared libperl library on shared perl
        # platforms.  We peek at lddlflags to see if we need -Wl,-R
        # or -R to add paths to the run-time library search path.
        if ($Config{'lddlflags'} =~ /-Wl,-R/) {
            $libs .= ' "-L$(PERL_INC)" "-Wl,-R$(INSTALLARCHLIB)/CORE" "-Wl,-R$(PERL_ARCHLIB)/CORE" -lperl';
        } elsif ($Config{'lddlflags'} =~ /-R/) {
            $libs .= ' "-L$(PERL_INC)" "-R$(INSTALLARCHLIB)/CORE" "-R$(PERL_ARCHLIB)/CORE" -lperl';
        } elsif ( $Is{Android} ) {
            # The Android linker will not recognize symbols from
            # libperl unless the module explicitly depends on it.
            $libs .= ' "-L$(PERL_INC)" -lperl';
        }
    }

    my $ld_run_path_shell = "";
    if ($self->{LD_RUN_PATH} ne "") {
        $ld_run_path_shell = 'LD_RUN_PATH="$(LD_RUN_PATH)" ';
    }

    push @m, sprintf <<'MAKE', $ld_run_path_shell, $ldrun, $ldfrom, $self->xs_obj_opt('$@'), $libs, $exportlist;
	%s$(LD) %s $(LDDLFLAGS) %s $(OTHERLDFLAGS) %s $(MYEXTLIB) \
	  $(PERL_ARCHIVE) %s $(PERL_ARCHIVE_AFTER) %s \
	  $(INST_DYNAMIC_FIX)
	$(CHMOD) $(PERM_RWX) $@
MAKE
    join '', @m;
}

=item exescan

Deprecated method. Use libscan instead.

=cut

sub exescan {
    my($self,$path) = @_;
    $path;
}

=item extliblist

Called by init_others, and calls ext ExtUtils::Liblist. See
L<ExtUtils::Liblist> for details.

=cut

sub extliblist {
    my($self,$libs) = @_;
    require ExtUtils::Liblist;
    $self->ext($libs, $Verbose);
}

=item find_perl

Finds the executables PERL and FULLPERL

=cut

sub find_perl {
    my($self, $ver, $names, $dirs, $trace) = @_;

    if ($trace >= 2){
        print "Looking for perl $ver by these names:
@$names
in these dirs:
@$dirs
";
    }

    my $stderr_duped = 0;
    local *STDERR_COPY;

    unless ($Is{BSD}) {
        # >& and lexical filehandles together give 5.6.2 indigestion
        if( open(STDERR_COPY, '>&STDERR') ) {  ## no critic
            $stderr_duped = 1;
        }
        else {
            warn <<WARNING;
find_perl() can't dup STDERR: $!
You might see some garbage while we search for Perl
WARNING
        }
    }

    foreach my $name (@$names){
        my ($abs, $use_dir);
        if ($self->file_name_is_absolute($name)) {     # /foo/bar
            $abs = $name;
        } elsif ($self->canonpath($name) eq
                 $self->canonpath(basename($name))) {  # foo
            $use_dir = 1;
        } else {                                            # foo/bar
            $abs = $self->catfile($Curdir, $name);
        }
        foreach my $dir ($use_dir ? @$dirs : 1){
            next unless defined $dir; # $self->{PERL_SRC} may be undefined

            $abs = $self->catfile($dir, $name)
                if $use_dir;

            print "Checking $abs\n" if ($trace >= 2);
            next unless $self->maybe_command($abs);
            print "Executing $abs\n" if ($trace >= 2);

            my $val;
            my $version_check = qq{"$abs" -le "require $ver; print qq{VER_OK}"};

            # To avoid using the unportable 2>&1 to suppress STDERR,
            # we close it before running the command.
            # However, thanks to a thread library bug in many BSDs
            # ( http://www.freebsd.org/cgi/query-pr.cgi?pr=51535 )
            # we cannot use the fancier more portable way in here
            # but instead need to use the traditional 2>&1 construct.
            if ($Is{BSD}) {
                $val = `$version_check 2>&1`;
            } else {
                close STDERR if $stderr_duped;
                $val = `$version_check`;

                # 5.6.2's 3-arg open doesn't work with >&
                open STDERR, ">&STDERR_COPY"  ## no critic
                        if $stderr_duped;
            }

            if ($val =~ /^VER_OK/m) {
                print "Using PERL=$abs\n" if $trace;
                return $abs;
            } elsif ($trace >= 2) {
                print "Result: '$val' ".($? >> 8)."\n";
            }
        }
    }
    print "Unable to find a perl $ver (by these names: @$names, in these dirs: @$dirs)\n";
    0; # false and not empty
}


=item fixin

  $mm->fixin(@files);

Inserts the sharpbang or equivalent magic number to a set of @files.

=cut

sub fixin {    # stolen from the pink Camel book, more or less
    my ( $self, @files ) = @_;

    for my $file (@files) {
        my $file_new = "$file.new";
        my $file_bak = "$file.bak";

        open( my $fixin, '<', $file ) or croak "Can't process '$file': $!";
        local $/ = "\n";
        chomp( my $line = <$fixin> );
        next unless $line =~ s/^\s*\#!\s*//;    # Not a shebang file.

        my $shb = $self->_fixin_replace_shebang( $file, $line );
        next unless defined $shb;

        open( my $fixout, ">", "$file_new" ) or do {
            warn "Can't create new $file: $!\n";
            next;
        };

        # Print out the new #! line (or equivalent).
        local $\;
        local $/;
        print $fixout $shb, <$fixin>;
        close $fixin;
        close $fixout;

        chmod 0666, $file_bak;
        unlink $file_bak;
        unless ( _rename( $file, $file_bak ) ) {
            warn "Can't rename $file to $file_bak: $!";
            next;
        }
        unless ( _rename( $file_new, $file ) ) {
            warn "Can't rename $file_new to $file: $!";
            unless ( _rename( $file_bak, $file ) ) {
                warn "Can't rename $file_bak back to $file either: $!";
                warn "Leaving $file renamed as $file_bak\n";
            }
            next;
        }
        unlink $file_bak;
    }
    continue {
        system("$Config{'eunicefix'} $file") if $Config{'eunicefix'} ne ':';
    }
}


sub _rename {
    my($old, $new) = @_;

    foreach my $file ($old, $new) {
        if( $Is{VMS} and basename($file) !~ /\./ ) {
            # rename() in 5.8.0 on VMS will not rename a file if it
            # does not contain a dot yet it returns success.
            $file = "$file.";
        }
    }

    return rename($old, $new);
}

sub _fixin_replace_shebang {
    my ( $self, $file, $line ) = @_;

    # Now figure out the interpreter name.
    my ( $cmd, $arg ) = split ' ', $line, 2;
    $cmd =~ s!^.*/!!;

    # Now look (in reverse) for interpreter in absolute PATH (unless perl).
    my $interpreter;
    if ( $cmd =~ m{^perl(?:\z|[^a-z])} ) {
        if ( $Config{startperl} =~ m,^\#!.*/perl, ) {
            $interpreter = $Config{startperl};
            $interpreter =~ s,^\#!,,;
        }
        else {
            $interpreter = $Config{perlpath};
        }
    }
    else {
        my (@absdirs)
            = reverse grep { $self->file_name_is_absolute($_) } $self->path;
        $interpreter = '';

        foreach my $dir (@absdirs) {
            my $maybefile = $self->catfile($dir,$cmd);
            if ( $self->maybe_command($maybefile) ) {
                warn "Ignoring $interpreter in $file\n"
                    if $Verbose && $interpreter;
                $interpreter = $maybefile;
            }
        }
    }

    # Figure out how to invoke interpreter on this machine.

    my ($does_shbang) = $Config{'sharpbang'} =~ /^\s*\#\!/;
    my ($shb) = "";
    if ($interpreter) {
        print "Changing sharpbang in $file to $interpreter"
            if $Verbose;
         # this is probably value-free on DOSISH platforms
        if ($does_shbang) {
            $shb .= "$Config{'sharpbang'}$interpreter";
            $shb .= ' ' . $arg if defined $arg;
            $shb .= "\n";
        }
    }
    else {
        warn "Can't find $cmd in PATH, $file unchanged"
            if $Verbose;
        return;
    }
    return $shb
}

=item force (o)

Writes an empty FORCE: target.

=cut

sub force {
    my($self) = shift;
    '# Phony target to force checking subdirectories.
FORCE :
	$(NOECHO) $(NOOP)
';
}

=item guess_name

Guess the name of this package by examining the working directory's
name. MakeMaker calls this only if the developer has not supplied a
NAME attribute.

=cut

# ';

sub guess_name {
    my($self) = @_;
    use Cwd 'cwd';
    my $name = basename(cwd());
    $name =~ s|[\-_][\d\.\-]+\z||;  # this is new with MM 5.00, we
                                    # strip minus or underline
                                    # followed by a float or some such
    print "Warning: Guessing NAME [$name] from current directory name.\n";
    $name;
}

=item has_link_code

Returns true if C, XS, MYEXTLIB or similar objects exist within this
object that need a compiler. Does not descend into subdirectories as
needs_linking() does.

=cut

sub has_link_code {
    my($self) = shift;
    return $self->{HAS_LINK_CODE} if defined $self->{HAS_LINK_CODE};
    if ($self->{OBJECT} or @{$self->{C} || []} or $self->{MYEXTLIB}){
	$self->{HAS_LINK_CODE} = 1;
	return 1;
    }
    return $self->{HAS_LINK_CODE} = 0;
}

=item blibdirs_target

    my $make_frag = $mm->blibdirs_target;

Creates the blibdirs target which creates all the directories we use
in blib/.

The blibdirs.ts target is deprecated.  Depend on blibdirs instead.


=cut

sub _xs_list_basenames {
    my ($self) = @_;
    map { (my $b = $_) =~ s/\.xs$//; $b } sort keys %{ $self->{XS} };
}

sub blibdirs_target {
    my $self = shift;

    my @dirs = map { uc "\$(INST_$_)" } qw(libdir archlib
                                           autodir archautodir
                                           bin script
                                           man1dir man3dir
                                          );
    if ($self->{XSMULTI}) {
        for my $ext ($self->_xs_list_basenames) {
            my ($v, $d, $f) = File::Spec->splitpath($ext);
            my @d = File::Spec->splitdir($d);
            shift @d if $d[0] eq 'lib';
            push @dirs, $self->catdir('$(INST_ARCHLIB)', 'auto', @d, $f);
	}
    }

    my @exists = map { $_.'$(DFSEP).exists' } @dirs;

    my $make = sprintf <<'MAKE', join(' ', @exists);
blibdirs : %s
	$(NOECHO) $(NOOP)

# Backwards compat with 6.18 through 6.25
blibdirs.ts : blibdirs
	$(NOECHO) $(NOOP)

MAKE

    $make .= $self->dir_target(@dirs);

    return $make;
}


=item clean (o)

Defines the clean target.

=cut

sub clean {
# --- Cleanup and Distribution Sections ---

    my($self, %attribs) = @_;
    my @m;
    push(@m, '
# Delete temporary files but do not touch installed files. We don\'t delete
# the Makefile here so a later make realclean still has a makefile to use.

clean :: clean_subdirs
');

    my @files = sort values %{$self->{XS}}; # .c files from *.xs files
    push @files, map {
	my $file = $_;
	map { $file.$_ } $self->{OBJ_EXT}, qw(.def _def.old .bs .bso .exp .base);
    } $self->_xs_list_basenames;
    my @dirs  = qw(blib);

    # Normally these are all under blib but they might have been
    # redefined.
    # XXX normally this would be a good idea, but the Perl core sets
    # INST_LIB = ../../lib rather than actually installing the files.
    # So a "make clean" in an ext/ directory would blow away lib.
    # Until the core is adjusted let's leave this out.
#     push @dirs, qw($(INST_ARCHLIB) $(INST_LIB)
#                    $(INST_BIN) $(INST_SCRIPT)
#                    $(INST_MAN1DIR) $(INST_MAN3DIR)
#                    $(INST_LIBDIR) $(INST_ARCHLIBDIR) $(INST_AUTODIR)
#                    $(INST_STATIC) $(INST_DYNAMIC)
#                 );


    if( $attribs{FILES} ) {
        # Use @dirs because we don't know what's in here.
        push @dirs, ref $attribs{FILES}                ?
                        @{$attribs{FILES}}             :
                        split /\s+/, $attribs{FILES}   ;
    }

    push(@files, qw[$(MAKE_APERL_FILE)
                    MYMETA.json MYMETA.yml perlmain.c tmon.out mon.out so_locations
                    blibdirs.ts pm_to_blib pm_to_blib.ts
                    *$(OBJ_EXT) *$(LIB_EXT) perl.exe perl perl$(EXE_EXT)
                    $(BOOTSTRAP) $(BASEEXT).bso
                    $(BASEEXT).def lib$(BASEEXT).def
                    $(BASEEXT).exp $(BASEEXT).x
                   ]);

    push(@files, $self->catfile('$(INST_ARCHAUTODIR)','extralibs.all'));
    push(@files, $self->catfile('$(INST_ARCHAUTODIR)','extralibs.ld'));

    # core files
    if ($^O eq 'vos') {
        push(@files, qw[perl*.kp]);
    }
    else {
        push(@files, qw[core core.*perl.*.? *perl.core]);
    }

    push(@files, map { "core." . "[0-9]"x$_ } (1..5));

    # OS specific things to clean up.  Use @dirs since we don't know
    # what might be in here.
    push @dirs, $self->extra_clean_files;

    # Occasionally files are repeated several times from different sources
    { my(%f) = map { ($_ => 1) } @files; @files = sort keys %f; }
    { my(%d) = map { ($_ => 1) } @dirs;  @dirs  = sort keys %d; }

    push @m, map "\t$_\n", $self->split_command('- $(RM_F)',  @files);
    push @m, map "\t$_\n", $self->split_command('- $(RM_RF)', @dirs);

    # Leave Makefile.old around for realclean
    push @m, <<'MAKE';
	  $(NOECHO) $(RM_F) $(MAKEFILE_OLD)
	- $(MV) $(FIRST_MAKEFILE) $(MAKEFILE_OLD) $(DEV_NULL)
MAKE

    push(@m, "\t$attribs{POSTOP}\n")   if $attribs{POSTOP};

    join("", @m);
}


=item clean_subdirs_target

  my $make_frag = $MM->clean_subdirs_target;

Returns the clean_subdirs target.  This is used by the clean target to
call clean on any subdirectories which contain Makefiles.

=cut

sub clean_subdirs_target {
    my($self) = shift;

    # No subdirectories, no cleaning.
    return <<'NOOP_FRAG' unless @{$self->{DIR}};
clean_subdirs :
	$(NOECHO) $(NOOP)
NOOP_FRAG


    my $clean = "clean_subdirs :\n";

    for my $dir (@{$self->{DIR}}) {
        my $subclean = $self->oneliner(sprintf <<'CODE', $dir);
exit 0 unless chdir '%s';  system '$(MAKE) clean' if -f '$(FIRST_MAKEFILE)';
CODE

        $clean .= "\t$subclean\n";
    }

    return $clean;
}


=item dir_target

    my $make_frag = $mm->dir_target(@directories);

Generates targets to create the specified directories and set its
permission to PERM_DIR.

Because depending on a directory to just ensure it exists doesn't work
too well (the modified time changes too often) dir_target() creates a
.exists file in the created directory.  It is this you should depend on.
For portability purposes you should use the $(DIRFILESEP) macro rather
than a '/' to separate the directory from the file.

    yourdirectory$(DIRFILESEP).exists

=cut

sub dir_target {
    my($self, @dirs) = @_;

    my $make = '';
    foreach my $dir (@dirs) {
        $make .= sprintf <<'MAKE', ($dir) x 4;
%s$(DFSEP).exists :: Makefile.PL
	$(NOECHO) $(MKPATH) %s
	$(NOECHO) $(CHMOD) $(PERM_DIR) %s
	$(NOECHO) $(TOUCH) %s$(DFSEP).exists

MAKE

    }

    return $make;
}


=item distdir

Defines the scratch directory target that will hold the distribution
before tar-ing (or shar-ing).

=cut

# For backwards compatibility.
*dist_dir = *distdir;

sub distdir {
    my($self) = shift;

    my $meta_target = $self->{NO_META} ? '' : 'distmeta';
    my $sign_target = !$self->{SIGN}   ? '' : 'distsignature';

    return sprintf <<'MAKE_FRAG', $meta_target, $sign_target;
create_distdir :
	$(RM_RF) $(DISTVNAME)
	$(PERLRUN) "-MExtUtils::Manifest=manicopy,maniread" \
		-e "manicopy(maniread(),'$(DISTVNAME)', '$(DIST_CP)');"

distdir : create_distdir %s %s
	$(NOECHO) $(NOOP)

MAKE_FRAG

}


=item dist_test

Defines a target that produces the distribution in the
scratch directory, and runs 'perl Makefile.PL; make ;make test' in that
subdirectory.

=cut

sub dist_test {
    my($self) = shift;

    my $mpl_args = join " ", map qq["$_"], @ARGV;

    my $test = $self->cd('$(DISTVNAME)',
                         '$(ABSPERLRUN) Makefile.PL '.$mpl_args,
                         '$(MAKE) $(PASTHRU)',
                         '$(MAKE) test $(PASTHRU)'
                        );

    return sprintf <<'MAKE_FRAG', $test;
disttest : distdir
	%s

MAKE_FRAG


}


=item xs_dlsyms_ext

Returns file-extension for C<xs_make_dlsyms> method's output file,
including any "." character.

=cut

sub xs_dlsyms_ext {
    die "Pure virtual method";
}

=item xs_dlsyms_extra

Returns any extra text to be prepended to the C<$extra> argument of
C<xs_make_dlsyms>.

=cut

sub xs_dlsyms_extra {
    '';
}

=item xs_dlsyms_iterator

Iterates over necessary shared objects, calling C<xs_make_dlsyms> method
for each with appropriate arguments.

=cut

sub xs_dlsyms_iterator {
    my ($self, $attribs) = @_;
    if ($self->{XSMULTI}) {
        my @m;
        for my $ext ($self->_xs_list_basenames) {
            my @parts = File::Spec->splitdir($ext);
            shift @parts if $parts[0] eq 'lib';
            my $name = join '::', @parts;
            push @m, $self->xs_make_dlsyms(
                $attribs,
                $ext . $self->xs_dlsyms_ext,
                "$ext.xs",
                $name,
                $parts[-1],
                {}, [], {}, [],
                $self->xs_dlsyms_extra . q!, 'FILE' => ! . neatvalue($ext),
            );
        }
        return join "\n", @m;
    } else {
        return $self->xs_make_dlsyms(
            $attribs,
            $self->{BASEEXT} . $self->xs_dlsyms_ext,
            'Makefile.PL',
            $self->{NAME},
            $self->{DLBASE},
            $attribs->{DL_FUNCS} || $self->{DL_FUNCS} || {},
            $attribs->{FUNCLIST} || $self->{FUNCLIST} || [],
            $attribs->{IMPORTS} || $self->{IMPORTS} || {},
            $attribs->{DL_VARS} || $self->{DL_VARS} || [],
            $self->xs_dlsyms_extra,
        );
    }
}

=item xs_make_dlsyms

    $self->xs_make_dlsyms(
        \%attribs, # hashref from %attribs in caller
        "$self->{BASEEXT}.def", # output file for Makefile target
        'Makefile.PL', # dependency
        $self->{NAME}, # shared object's "name"
        $self->{DLBASE}, # last ::-separated part of name
        $attribs{DL_FUNCS} || $self->{DL_FUNCS} || {}, # various params
        $attribs{FUNCLIST} || $self->{FUNCLIST} || [],
        $attribs{IMPORTS} || $self->{IMPORTS} || {},
        $attribs{DL_VARS} || $self->{DL_VARS} || [],
        # optional extra param that will be added as param to Mksymlists
    );

Utility method that returns Makefile snippet to call C<Mksymlists>.

=cut

sub xs_make_dlsyms {
    my ($self, $attribs, $target, $dep, $name, $dlbase, $funcs, $funclist, $imports, $vars, $extra) = @_;
    my @m = (
     "\n$target: $dep\n",
     q!	$(PERLRUN) -MExtUtils::Mksymlists \\
     -e "Mksymlists('NAME'=>\"!, $name,
     q!\", 'DLBASE' => '!,$dlbase,
     # The above two lines quoted differently to work around
     # a bug in the 4DOS/4NT command line interpreter.  The visible
     # result of the bug was files named q('extension_name',) *with the
     # single quotes and the comma* in the extension build directories.
     q!', 'DL_FUNCS' => !,neatvalue($funcs),
     q!, 'FUNCLIST' => !,neatvalue($funclist),
     q!, 'IMPORTS' => !,neatvalue($imports),
     q!, 'DL_VARS' => !, neatvalue($vars)
    );
    push @m, $extra if defined $extra;
    push @m, qq!);"\n!;
    join '', @m;
}

=item dynamic (o)

Defines the dynamic target.

=cut

sub dynamic {
# --- Dynamic Loading Sections ---

    my($self) = shift;
    '
dynamic :: $(FIRST_MAKEFILE) config $(INST_BOOT) $(INST_DYNAMIC)
	$(NOECHO) $(NOOP)
';
}

=item install (o)

Defines the install target.

=cut

sub install {
    my($self, %attribs) = @_;
    my(@m);

    push @m, q{
install :: pure_install doc_install
	$(NOECHO) $(NOOP)

install_perl :: pure_perl_install doc_perl_install
	$(NOECHO) $(NOOP)

install_site :: pure_site_install doc_site_install
	$(NOECHO) $(NOOP)

install_vendor :: pure_vendor_install doc_vendor_install
	$(NOECHO) $(NOOP)

pure_install :: pure_$(INSTALLDIRS)_install
	$(NOECHO) $(NOOP)

doc_install :: doc_$(INSTALLDIRS)_install
	$(NOECHO) $(NOOP)

pure__install : pure_site_install
	$(NOECHO) $(ECHO) INSTALLDIRS not defined, defaulting to INSTALLDIRS=site

doc__install : doc_site_install
	$(NOECHO) $(ECHO) INSTALLDIRS not defined, defaulting to INSTALLDIRS=site

pure_perl_install :: all
	$(NOECHO) $(MOD_INSTALL) \
};

    push @m,
q{		read "}.$self->catfile('$(PERL_ARCHLIB)','auto','$(FULLEXT)','.packlist').q{" \
		write "}.$self->catfile('$(DESTINSTALLARCHLIB)','auto','$(FULLEXT)','.packlist').q{" \
} unless $self->{NO_PACKLIST};

    push @m,
q{		"$(INST_LIB)" "$(DESTINSTALLPRIVLIB)" \
		"$(INST_ARCHLIB)" "$(DESTINSTALLARCHLIB)" \
		"$(INST_BIN)" "$(DESTINSTALLBIN)" \
		"$(INST_SCRIPT)" "$(DESTINSTALLSCRIPT)" \
		"$(INST_MAN1DIR)" "$(DESTINSTALLMAN1DIR)" \
		"$(INST_MAN3DIR)" "$(DESTINSTALLMAN3DIR)"
	$(NOECHO) $(WARN_IF_OLD_PACKLIST) \
		"}.$self->catdir('$(SITEARCHEXP)','auto','$(FULLEXT)').q{"


pure_site_install :: all
	$(NOECHO) $(MOD_INSTALL) \
};
    push @m,
q{		read "}.$self->catfile('$(SITEARCHEXP)','auto','$(FULLEXT)','.packlist').q{" \
		write "}.$self->catfile('$(DESTINSTALLSITEARCH)','auto','$(FULLEXT)','.packlist').q{" \
} unless $self->{NO_PACKLIST};

    push @m,
q{		"$(INST_LIB)" "$(DESTINSTALLSITELIB)" \
		"$(INST_ARCHLIB)" "$(DESTINSTALLSITEARCH)" \
		"$(INST_BIN)" "$(DESTINSTALLSITEBIN)" \
		"$(INST_SCRIPT)" "$(DESTINSTALLSITESCRIPT)" \
		"$(INST_MAN1DIR)" "$(DESTINSTALLSITEMAN1DIR)" \
		"$(INST_MAN3DIR)" "$(DESTINSTALLSITEMAN3DIR)"
	$(NOECHO) $(WARN_IF_OLD_PACKLIST) \
		"}.$self->catdir('$(PERL_ARCHLIB)','auto','$(FULLEXT)').q{"

pure_vendor_install :: all
	$(NOECHO) $(MOD_INSTALL) \
};
    push @m,
q{		read "}.$self->catfile('$(VENDORARCHEXP)','auto','$(FULLEXT)','.packlist').q{" \
		write "}.$self->catfile('$(DESTINSTALLVENDORARCH)','auto','$(FULLEXT)','.packlist').q{" \
} unless $self->{NO_PACKLIST};

    push @m,
q{		"$(INST_LIB)" "$(DESTINSTALLVENDORLIB)" \
		"$(INST_ARCHLIB)" "$(DESTINSTALLVENDORARCH)" \
		"$(INST_BIN)" "$(DESTINSTALLVENDORBIN)" \
		"$(INST_SCRIPT)" "$(DESTINSTALLVENDORSCRIPT)" \
		"$(INST_MAN1DIR)" "$(DESTINSTALLVENDORMAN1DIR)" \
		"$(INST_MAN3DIR)" "$(DESTINSTALLVENDORMAN3DIR)"

};

    push @m, q{
doc_perl_install :: all
	$(NOECHO) $(NOOP)

doc_site_install :: all
	$(NOECHO) $(NOOP)

doc_vendor_install :: all
	$(NOECHO) $(NOOP)

} if $self->{NO_PERLLOCAL};

    push @m, q{
doc_perl_install :: all
	$(NOECHO) $(ECHO) Appending installation info to "$(DESTINSTALLARCHLIB)/perllocal.pod"
	-$(NOECHO) $(MKPATH) "$(DESTINSTALLARCHLIB)"
	-$(NOECHO) $(DOC_INSTALL) \
		"Module" "$(NAME)" \
		"installed into" "$(INSTALLPRIVLIB)" \
		LINKTYPE "$(LINKTYPE)" \
		VERSION "$(VERSION)" \
		EXE_FILES "$(EXE_FILES)" \
		>> "}.$self->catfile('$(DESTINSTALLARCHLIB)','perllocal.pod').q{"

doc_site_install :: all
	$(NOECHO) $(ECHO) Appending installation info to "$(DESTINSTALLARCHLIB)/perllocal.pod"
	-$(NOECHO) $(MKPATH) "$(DESTINSTALLARCHLIB)"
	-$(NOECHO) $(DOC_INSTALL) \
		"Module" "$(NAME)" \
		"installed into" "$(INSTALLSITELIB)" \
		LINKTYPE "$(LINKTYPE)" \
		VERSION "$(VERSION)" \
		EXE_FILES "$(EXE_FILES)" \
		>> "}.$self->catfile('$(DESTINSTALLARCHLIB)','perllocal.pod').q{"

doc_vendor_install :: all
	$(NOECHO) $(ECHO) Appending installation info to "$(DESTINSTALLARCHLIB)/perllocal.pod"
	-$(NOECHO) $(MKPATH) "$(DESTINSTALLARCHLIB)"
	-$(NOECHO) $(DOC_INSTALL) \
		"Module" "$(NAME)" \
		"installed into" "$(INSTALLVENDORLIB)" \
		LINKTYPE "$(LINKTYPE)" \
		VERSION "$(VERSION)" \
		EXE_FILES "$(EXE_FILES)" \
		>> "}.$self->catfile('$(DESTINSTALLARCHLIB)','perllocal.pod').q{"

} unless $self->{NO_PERLLOCAL};

    push @m, q{
uninstall :: uninstall_from_$(INSTALLDIRS)dirs
	$(NOECHO) $(NOOP)

uninstall_from_perldirs ::
	$(NOECHO) $(UNINSTALL) "}.$self->catfile('$(PERL_ARCHLIB)','auto','$(FULLEXT)','.packlist').q{"

uninstall_from_sitedirs ::
	$(NOECHO) $(UNINSTALL) "}.$self->catfile('$(SITEARCHEXP)','auto','$(FULLEXT)','.packlist').q{"

uninstall_from_vendordirs ::
	$(NOECHO) $(UNINSTALL) "}.$self->catfile('$(VENDORARCHEXP)','auto','$(FULLEXT)','.packlist').q{"
};

    join("",@m);
}

=item installbin (o)

Defines targets to make and to install EXE_FILES.

=cut

sub installbin {
    my($self) = shift;

    return "" unless $self->{EXE_FILES} && ref $self->{EXE_FILES} eq "ARRAY";
    my @exefiles = sort @{$self->{EXE_FILES}};
    return "" unless @exefiles;

    @exefiles = map vmsify($_), @exefiles if $Is{VMS};

    my %fromto;
    for my $from (@exefiles) {
	my($path)= $self->catfile('$(INST_SCRIPT)', basename($from));

	local($_) = $path; # for backwards compatibility
	my $to = $self->libscan($path);
	print "libscan($from) => '$to'\n" if ($Verbose >=2);

        $to = vmsify($to) if $Is{VMS};
	$fromto{$from} = $to;
    }
    my @to   = sort values %fromto;

    my @m;
    push(@m, qq{
EXE_FILES = @exefiles

pure_all :: @to
	\$(NOECHO) \$(NOOP)

realclean ::
});

    # realclean can get rather large.
    push @m, map "\t$_\n", $self->split_command('$(RM_F)', @to);
    push @m, "\n";

    # A target for each exe file.
    my @froms = sort keys %fromto;
    for my $from (@froms) {
        #                              1      2
        push @m, _sprintf562 <<'MAKE', $from, $fromto{$from};
%2$s : %1$s $(FIRST_MAKEFILE) $(INST_SCRIPT)$(DFSEP).exists $(INST_BIN)$(DFSEP).exists
	$(NOECHO) $(RM_F) %2$s
	$(CP) %1$s %2$s
	$(FIXIN) %2$s
	-$(NOECHO) $(CHMOD) $(PERM_RWX) %2$s

MAKE

    }

    join "", @m;
}

=item linkext (o)

Defines the linkext target which in turn defines the LINKTYPE.

=cut

# LINKTYPE => static or dynamic or ''
sub linkext {
    my($self, %attribs) = @_;
    my $linktype = $attribs{LINKTYPE};
    $linktype = $self->{LINKTYPE} unless defined $linktype;
    if (defined $linktype and $linktype eq '') {
        warn "Warning: LINKTYPE set to '', no longer necessary\n";
    }
    $linktype = '$(LINKTYPE)' unless defined $linktype;
    "
linkext :: $linktype
	\$(NOECHO) \$(NOOP)
";
}

=item lsdir

Takes as arguments a directory name and a regular expression. Returns
all entries in the directory that match the regular expression.

=cut

sub lsdir {
    #  $self
    my(undef, $dir, $regex) = @_;
    opendir(my $dh, defined($dir) ? $dir : ".")
        or return;
    my @ls = readdir $dh;
    closedir $dh;
    @ls = grep(/$regex/, @ls) if defined $regex;
    @ls;
}

=item macro (o)

Simple subroutine to insert the macros defined by the macro attribute
into the Makefile.

=cut

sub macro {
    my($self,%attribs) = @_;
    my @m;
    foreach my $key (sort keys %attribs) {
	my $val = $attribs{$key};
	push @m, "$key = $val\n";
    }
    join "", @m;
}

=item makeaperl (o)

Called by staticmake. Defines how to write the Makefile to produce a
static new perl.

By default the Makefile produced includes all the static extensions in
the perl library. (Purified versions of library files, e.g.,
DynaLoader_pure_p1_c0_032.a are automatically ignored to avoid link errors.)

=cut

sub makeaperl {
    my($self, %attribs) = @_;
    my($makefilename, $searchdirs, $static, $extra, $perlinc, $target, $tmp, $libperl) =
	@attribs{qw(MAKE DIRS STAT EXTRA INCL TARGET TMP LIBPERL)};
    s/^(.*)/"-I$1"/ for @{$perlinc || []};
    my(@m);
    push @m, "
# --- MakeMaker makeaperl section ---
MAP_TARGET    = $target
FULLPERL      = $self->{FULLPERL}
MAP_PERLINC   = @{$perlinc || []}
";
    return join '', @m if $self->{PARENT};

    my($dir) = join ":", @{$self->{DIR}};

    unless ($self->{MAKEAPERL}) {
	push @m, q{
$(MAP_TARGET) :: $(MAKE_APERL_FILE)
	$(MAKE) $(USEMAKEFILE) $(MAKE_APERL_FILE) $@

$(MAKE_APERL_FILE) : static $(FIRST_MAKEFILE) pm_to_blib
	$(NOECHO) $(ECHO) Writing \"$(MAKE_APERL_FILE)\" for this $(MAP_TARGET)
	$(NOECHO) $(PERLRUNINST) \
		Makefile.PL DIR="}, $dir, q{" \
		MAKEFILE=$(MAKE_APERL_FILE) LINKTYPE=static \
		MAKEAPERL=1 NORECURS=1 CCCDLFLAGS=};

	foreach (@ARGV){
		my $arg = $_; # avoid lvalue aliasing
		if ( $arg =~ /(^.*?=)(.*['\s].*)/ ) {
			$arg = $1 . $self->quote_literal($2);
		}
		push @m, " \\\n\t\t$arg";
	}
	push @m, "\n";

	return join '', @m;
    }

    my $cccmd = $self->const_cccmd($libperl);
    $cccmd =~ s/^CCCMD\s*=\s*//;
    $cccmd =~ s/\$\(INC\)/ "-I$self->{PERL_INC}" /;
    $cccmd .= " $Config{cccdlflags}"
	if ($Config{useshrplib} eq 'true');
    $cccmd =~ s/\(CC\)/\(PERLMAINCC\)/;

    # The front matter of the linkcommand...
    my $linkcmd = join ' ', "\$(CC)",
	    grep($_, @Config{qw(ldflags ccdlflags)});
    $linkcmd =~ s/\s+/ /g;
    $linkcmd =~ s,(perl\.exp),\$(PERL_INC)/$1,;

    # Which *.a files could we make use of...
    my %static;
    require File::Find;
    # don't use File::Spec here because on Win32 F::F still uses "/"
    my $installed_version = join('/',
	'auto', $self->{FULLEXT}, "$self->{BASEEXT}$self->{LIB_EXT}"
    );
    File::Find::find(sub {
	return unless m/\Q$self->{LIB_EXT}\E$/;

        # Skip perl's libraries.
        return if m/^libperl/ or m/^perl\Q$self->{LIB_EXT}\E$/;

	# Skip purified versions of libraries
        # (e.g., DynaLoader_pure_p1_c0_032.a)
	return if m/_pure_\w+_\w+_\w+\.\w+$/ and -f "$File::Find::dir/.pure";

	if( exists $self->{INCLUDE_EXT} ){
		my $found = 0;

		(my $xx = $File::Find::name) =~ s,.*?/auto/,,s;
		$xx =~ s,/?$_,,;
		$xx =~ s,/,::,g;

		# Throw away anything not explicitly marked for inclusion.
		# DynaLoader is implied.
		foreach my $incl ((@{$self->{INCLUDE_EXT}},'DynaLoader')){
			if( $xx eq $incl ){
				$found++;
				last;
			}
		}
		return unless $found;
	}
	elsif( exists $self->{EXCLUDE_EXT} ){
		(my $xx = $File::Find::name) =~ s,.*?/auto/,,s;
		$xx =~ s,/?$_,,;
		$xx =~ s,/,::,g;

		# Throw away anything explicitly marked for exclusion
		foreach my $excl (@{$self->{EXCLUDE_EXT}}){
			return if( $xx eq $excl );
		}
	}

	# don't include the installed version of this extension. I
	# leave this line here, although it is not necessary anymore:
	# I patched minimod.PL instead, so that Miniperl.pm won't
	# include duplicates

	# Once the patch to minimod.PL is in the distribution, I can
	# drop it
	return if $File::Find::name =~ m:\Q$installed_version\E\z:;
	use Cwd 'cwd';
	$static{cwd() . "/" . $_}++;
    }, grep( -d $_, @{$searchdirs || []}) );

    # We trust that what has been handed in as argument, will be buildable
    $static = [] unless $static;
    @static{@{$static}} = (1) x @{$static};

    $extra = [] unless $extra && ref $extra eq 'ARRAY';
    for (sort keys %static) {
	next unless /\Q$self->{LIB_EXT}\E\z/;
	$_ = dirname($_) . "/extralibs.ld";
	push @$extra, $_;
    }

    s/^(.*)/"-I$1"/ for @{$perlinc || []};

    $target ||= "perl";
    $tmp    ||= ".";

# MAP_STATIC doesn't look into subdirs yet. Once "all" is made and we
# regenerate the Makefiles, MAP_STATIC and the dependencies for
# extralibs.all are computed correctly
    my @map_static = reverse sort keys %static;
    push @m, "
MAP_LINKCMD   = $linkcmd
MAP_STATIC    = ", join(" \\\n\t", map { qq{"$_"} } @map_static), "
MAP_STATICDEP = ", join(' ', map { $self->quote_dep($_) } @map_static), "

MAP_PRELIBS   = $Config{perllibs} $Config{cryptlib}
";

    my $lperl;
    if (defined $libperl) {
	($lperl = $libperl) =~ s/\$\(A\)/$self->{LIB_EXT}/;
    }
    unless ($libperl && -f $lperl) { # Ilya's code...
	my $dir = $self->{PERL_SRC} || "$self->{PERL_ARCHLIB}/CORE";
	$dir = "$self->{PERL_ARCHLIB}/.." if $self->{UNINSTALLED_PERL};
	$libperl ||= "libperl$self->{LIB_EXT}";
	$libperl   = "$dir/$libperl";
	$lperl   ||= "libperl$self->{LIB_EXT}";
	$lperl     = "$dir/$lperl";

        if (! -f $libperl and ! -f $lperl) {
          # We did not find a static libperl. Maybe there is a shared one?
          if ($Is{SunOS}) {
            $lperl  = $libperl = "$dir/$Config{libperl}";
            # SUNOS ld does not take the full path to a shared library
            $libperl = '' if $Is{SunOS4};
          }
        }

	print <<EOF unless -f $lperl || defined($self->{PERL_SRC});
Warning: $libperl not found
If you're going to build a static perl binary, make sure perl is installed
otherwise ignore this warning
EOF
    }

    # SUNOS ld does not take the full path to a shared library
    my $llibperl = $libperl ? '$(MAP_LIBPERL)' : '-lperl';
    my $libperl_dep = $self->quote_dep($libperl);

    push @m, "
MAP_LIBPERL = $libperl
MAP_LIBPERLDEP = $libperl_dep
LLIBPERL    = $llibperl
";

    push @m, '
$(INST_ARCHAUTODIR)/extralibs.all : $(INST_ARCHAUTODIR)$(DFSEP).exists '.join(" \\\n\t", @$extra).'
	$(NOECHO) $(RM_F)  $@
	$(NOECHO) $(TOUCH) $@
';

    foreach my $catfile (@$extra){
	push @m, "\tcat $catfile >> \$\@\n";
    }

    my $ldfrom = $self->{XSMULTI} ? '' : '$(LDFROM)';
    #                             1     2                        3        4
    push @m, _sprintf562 <<'EOF', $tmp, $ldfrom, $self->xs_obj_opt('$@'), $makefilename;
$(MAP_TARGET) :: %1$s/perlmain$(OBJ_EXT) $(MAP_LIBPERLDEP) $(MAP_STATICDEP) $(INST_ARCHAUTODIR)/extralibs.all
	$(MAP_LINKCMD) %2$s $(OPTIMIZE) %1$s/perlmain$(OBJ_EXT) %3$s $(MAP_STATIC) "$(LLIBPERL)" `cat $(INST_ARCHAUTODIR)/extralibs.all` $(MAP_PRELIBS)
	$(NOECHO) $(ECHO) "To install the new '$(MAP_TARGET)' binary, call"
	$(NOECHO) $(ECHO) "    $(MAKE) $(USEMAKEFILE) %4$s inst_perl MAP_TARGET=$(MAP_TARGET)"
	$(NOECHO) $(ECHO) "    $(MAKE) $(USEMAKEFILE) %4$s map_clean"

%1$s/perlmain\$(OBJ_EXT): %1$s/perlmain.c
EOF
    push @m, "\t".$self->cd($tmp, qq[$cccmd "-I\$(PERL_INC)" perlmain.c])."\n";

    my $maybe_DynaLoader = $Config{usedl} ? 'q(DynaLoader)' : '';
    push @m, _sprintf562 <<'EOF', $tmp, $makefilename, $maybe_DynaLoader;

%1$s/perlmain.c: %2$s
	$(NOECHO) $(ECHO) Writing $@
	$(NOECHO) $(PERL) $(MAP_PERLINC) "-MExtUtils::Miniperl" \
		-e "writemain(grep(s#.*/auto/##s, @ARGV), %3$s)" $(MAP_STATIC) > $@t
	$(MV) $@t $@

EOF
    push @m, "\t", q{$(NOECHO) $(PERL) "$(INSTALLSCRIPT)/fixpmain"
} if (defined (&Dos::UseLFN) && Dos::UseLFN()==0);


    push @m, q{
doc_inst_perl :
	$(NOECHO) $(ECHO) Appending installation info to "$(DESTINSTALLARCHLIB)/perllocal.pod"
	-$(NOECHO) $(MKPATH) "$(DESTINSTALLARCHLIB)"
	-$(NOECHO) $(DOC_INSTALL) \
		"Perl binary" "$(MAP_TARGET)" \
		MAP_STATIC "$(MAP_STATIC)" \
		MAP_EXTRA "`cat $(INST_ARCHAUTODIR)/extralibs.all`" \
		MAP_LIBPERL "$(MAP_LIBPERL)" \
		>> "}.$self->catfile('$(DESTINSTALLARCHLIB)','perllocal.pod').q{"

};

    push @m, q{
inst_perl : pure_inst_perl doc_inst_perl

pure_inst_perl : $(MAP_TARGET)
	}.$self->{CP}.q{ $(MAP_TARGET) "}.$self->catfile('$(DESTINSTALLBIN)','$(MAP_TARGET)').q{"

clean :: map_clean

map_clean :
	}.$self->{RM_F}.qq{ $tmp/perlmain\$(OBJ_EXT) $tmp/perlmain.c \$(MAP_TARGET) $makefilename \$(INST_ARCHAUTODIR)/extralibs.all
};

    join '', @m;
}

=item makefile (o)

Defines how to rewrite the Makefile.

=cut

sub makefile {
    my($self) = shift;
    my $m;
    # We do not know what target was originally specified so we
    # must force a manual rerun to be sure. But as it should only
    # happen very rarely it is not a significant problem.
    $m = '
$(OBJECT) : $(FIRST_MAKEFILE)

' if $self->{OBJECT};

    my $newer_than_target = $Is{VMS} ? '$(MMS$SOURCE_LIST)' : '$?';
    my $mpl_args = join " ", map qq["$_"], @ARGV;
    my $cross = '';
    if (defined $::Cross::platform) {
        # Inherited from win32/buildext.pl
        $cross = "-MCross=$::Cross::platform ";
    }
    $m .= sprintf <<'MAKE_FRAG', $newer_than_target, $cross, $mpl_args;
# We take a very conservative approach here, but it's worth it.
# We move Makefile to Makefile.old here to avoid gnu make looping.
$(FIRST_MAKEFILE) : Makefile.PL $(CONFIGDEP)
	$(NOECHO) $(ECHO) "Makefile out-of-date with respect to %s"
	$(NOECHO) $(ECHO) "Cleaning current config before rebuilding Makefile..."
	-$(NOECHO) $(RM_F) $(MAKEFILE_OLD)
	-$(NOECHO) $(MV)   $(FIRST_MAKEFILE) $(MAKEFILE_OLD)
	- $(MAKE) $(USEMAKEFILE) $(MAKEFILE_OLD) clean $(DEV_NULL)
	$(PERLRUN) %sMakefile.PL %s
	$(NOECHO) $(ECHO) "==> Your Makefile has been rebuilt. <=="
	$(NOECHO) $(ECHO) "==> Please rerun the $(MAKE) command.  <=="
	$(FALSE)

MAKE_FRAG

    return $m;
}   

=item makemakerdflt_target

  my $make_frag = $mm->makemakerdflt_target

Returns a make fragment with the makemakerdeflt_target specified.
This target is the first target in the Makefile, is the default target
and simply points off to 'all' just in case any make variant gets
confused or something gets snuck in before the real 'all' target.

=cut

sub makemakerdflt_target {
    return <<'MAKE_FRAG';
makemakerdflt : all
	$(NOECHO) $(NOOP)
MAKE_FRAG

}


=item manifypods_target

  my $manifypods_target = $self->manifypods_target;

Generates the manifypods target.  This target generates man pages from
all POD files in MAN1PODS and MAN3PODS.

=cut

sub manifypods_target {
    my($self) = shift;

    my $man1pods      = '';
    my $man3pods      = '';
    my $dependencies  = '';

    # populate manXpods & dependencies:
    foreach my $name (sort keys %{$self->{MAN1PODS}}, sort keys %{$self->{MAN3PODS}}) {
        $dependencies .= " \\\n\t$name";
    }

    my $manify = <<END;
manifypods : pure_all config $dependencies
END

    my @man_cmds;
    foreach my $section (qw(1 3)) {
        my $pods = $self->{"MAN${section}PODS"};
        my $p2m = sprintf <<'CMD', $section, $] > 5.008 ? " -u" : "";
	$(NOECHO) $(POD2MAN) --section=%s --perm_rw=$(PERM_RW)%s
CMD
        push @man_cmds, $self->split_command($p2m, map {($_,$pods->{$_})} sort keys %$pods);
    }

    $manify .= "\t\$(NOECHO) \$(NOOP)\n" unless @man_cmds;
    $manify .= join '', map { "$_\n" } @man_cmds;

    return $manify;
}

{
    my $has_cpan_meta;
    sub _has_cpan_meta {
        return $has_cpan_meta if defined $has_cpan_meta;
        return $has_cpan_meta = !!eval {
            require CPAN::Meta;
            CPAN::Meta->VERSION(2.112150);
            1;
        };
    }
}


=item maybe_command

Returns true, if the argument is likely to be a command.

=cut

sub maybe_command {
    my($self,$file) = @_;
    return $file if -x $file && ! -d $file;
    return;
}

=item metafile_target

    my $target = $mm->metafile_target;

Generate the metafile target.

Writes the file META.yml (YAML encoded meta-data) and META.json
(JSON encoded meta-data) about the module in the distdir.
The format follows Module::Build's as closely as possible.

=cut

sub metafile_target {
    my $self = shift;
    return <<'MAKE_FRAG' if $self->{NO_META} or ! _has_cpan_meta();
metafile :
	$(NOECHO) $(NOOP)
MAKE_FRAG

    my $metadata   = $self->metafile_data(
        $self->{META_ADD}   || {},
        $self->{META_MERGE} || {},
    );

    my $meta = $self->_fix_metadata_before_conversion( $metadata );

    my @write_metayml = $self->stashmeta(
      $meta->as_string({version => "1.4"}), 'META_new.yml'
    );
    my @write_metajson = $self->stashmeta(
      $meta->as_string({version => "2.0"}), 'META_new.json'
    );

    my $metayml = join("\n\t", @write_metayml);
    my $metajson = join("\n\t", @write_metajson);
    return sprintf <<'MAKE_FRAG', $metayml, $metajson;
metafile : create_distdir
	$(NOECHO) $(ECHO) Generating META.yml
	%s
	-$(NOECHO) $(MV) META_new.yml $(DISTVNAME)/META.yml
	$(NOECHO) $(ECHO) Generating META.json
	%s
	-$(NOECHO) $(MV) META_new.json $(DISTVNAME)/META.json
MAKE_FRAG

}

=begin private

=item _fix_metadata_before_conversion

    $mm->_fix_metadata_before_conversion( \%metadata );

Fixes errors in the metadata before it's handed off to CPAN::Meta for
conversion. This hopefully results in something that can be used further
on, no guarantee is made though.

=end private

=cut

sub _fix_metadata_before_conversion {
    my ( $self, $metadata ) = @_;

    # we should never be called unless this already passed but
    # prefer to be defensive in case somebody else calls this

    return unless _has_cpan_meta;

    my $bad_version = $metadata->{version} &&
                      !CPAN::Meta::Validator->new->version( 'version', $metadata->{version} );
    # just delete all invalid versions
    if( $bad_version ) {
        warn "Can't parse version '$metadata->{version}'\n";
        $metadata->{version} = '';
    }

    my $validator2 = CPAN::Meta::Validator->new( $metadata );
    my @errors;
    push @errors, $validator2->errors if !$validator2->is_valid;
    my $validator14 = CPAN::Meta::Validator->new(
        {
            %$metadata,
            'meta-spec' => { version => 1.4 },
        }
    );
    push @errors, $validator14->errors if !$validator14->is_valid;
    # fix non-camelcase custom resource keys (only other trick we know)
    for my $error ( @errors ) {
        my ( $key ) = ( $error =~ /Custom resource '(.*)' must be in CamelCase./ );
        next if !$key;

        # first try to remove all non-alphabetic chars
        ( my $new_key = $key ) =~ s/[^_a-zA-Z]//g;

        # if that doesn't work, uppercase first one
        $new_key = ucfirst $new_key if !$validator14->custom_1( $new_key );

        # copy to new key if that worked
        $metadata->{resources}{$new_key} = $metadata->{resources}{$key}
          if $validator14->custom_1( $new_key );

        # and delete old one in any case
        delete $metadata->{resources}{$key};
    }

    # paper over validation issues, but still complain, necessary because
    # there's no guarantee that the above will fix ALL errors
    my $meta = eval { CPAN::Meta->create( $metadata, { lazy_validation => 1 } ) };
    warn $@ if $@ and
               $@ !~ /encountered CODE.*, but JSON can only represent references to arrays or hashes/;

    # use the original metadata straight if the conversion failed
    # or if it can't be stringified.
    if( !$meta                                                  ||
        !eval { $meta->as_string( { version => $METASPEC_V } ) }      ||
        !eval { $meta->as_string }
    ) {
        $meta = bless $metadata, 'CPAN::Meta';
    }

    my $now_license = $meta->as_struct({ version => 2 })->{license};
    if ($self->{LICENSE} and $self->{LICENSE} ne 'unknown' and
        @{$now_license} == 1 and $now_license->[0] eq 'unknown'
    ) {
        warn "Invalid LICENSE value '$self->{LICENSE}' ignored\n";
    }

    $meta;
}


=begin private

=item_sort_pairs

    my @pairs = _sort_pairs($sort_sub, \%hash);

Sorts the pairs of a hash based on keys ordered according
to C<$sort_sub>.

=end private

=cut

sub _sort_pairs {
    my $sort  = shift;
    my $pairs = shift;
    return map  { $_ => $pairs->{$_} }
           sort $sort
           keys %$pairs;
}


# Taken from Module::Build::Base
sub _hash_merge {
    my ($self, $h, $k, $v) = @_;
    if (ref $h->{$k} eq 'ARRAY') {
        push @{$h->{$k}}, ref $v ? @$v : $v;
    } elsif (ref $h->{$k} eq 'HASH') {
        $self->_hash_merge($h->{$k}, $_, $v->{$_}) foreach keys %$v;
    } else {
        $h->{$k} = $v;
    }
}


=item metafile_data

    my $metadata_hashref = $mm->metafile_data(\%meta_add, \%meta_merge);

Returns the data which MakeMaker turns into the META.yml file 
and the META.json file. It is always in version 2.0 of the format.

Values of %meta_add will overwrite any existing metadata in those
keys.  %meta_merge will be merged with them.

=cut

sub metafile_data {
    my $self = shift;
    my($meta_add, $meta_merge) = @_;

    $meta_add ||= {};
    $meta_merge ||= {};

    my $version = _normalize_version($self->{VERSION});
    my $release_status = ($version =~ /_/) ? 'unstable' : 'stable';
    my %meta = (
        # required
        abstract     => $self->{ABSTRACT} || 'unknown',
        author       => defined($self->{AUTHOR}) ? $self->{AUTHOR} : ['unknown'],
        dynamic_config => 1,
        generated_by => "ExtUtils::MakeMaker version $ExtUtils::MakeMaker::VERSION",
        license      => [ $self->{LICENSE} || 'unknown' ],
        'meta-spec'  => {
            url         => $METASPEC_URL,
            version     => $METASPEC_V,
        },
        name         => $self->{DISTNAME},
        release_status => $release_status,
        version      => $version,

        # optional
        no_index     => { directory => [qw(t inc)] },
    );
    $self->_add_requirements_to_meta(\%meta);

    if (!eval { require JSON::PP; require CPAN::Meta::Converter; CPAN::Meta::Converter->VERSION(2.141170) }) {
      return \%meta;
    }

    # needs to be based on the original version
    my $v1_add = _metaspec_version($meta_add) !~ /^2/;

    for my $frag ($meta_add, $meta_merge) {
        my $def_v = _metaspec_version($frag == $meta_add ? $meta_merge : $meta_add);
        $frag = CPAN::Meta::Converter->new($frag, default_version => $def_v)->upgrade_fragment;
    }

    # if we upgraded a 1.x _ADD fragment, we gave it a prereqs key that
    # will override all prereqs, which is more than the user asked for;
    # instead, we'll go inside the prereqs and override all those
    while( my($key, $val) = each %$meta_add ) {
        if ($v1_add and $key eq 'prereqs') {
            $meta{$key}{$_} = $val->{$_} for keys %$val;
        } elsif ($key ne 'meta-spec') {
            $meta{$key} = $val;
        }
    }

    while( my($key, $val) = each %$meta_merge ) {
        next if $key eq 'meta-spec';
        $self->_hash_merge(\%meta, $key, $val);
    }

    return \%meta;
}


=begin private

=cut

sub _add_requirements_to_meta {
    my ( $self, $meta ) = @_;
    # Check the original args so we can tell between the user setting it
    # to an empty hash and it just being initialized.
    $meta->{prereqs}{configure}{requires} = $self->{ARGS}{CONFIGURE_REQUIRES}
        ? $self->{CONFIGURE_REQUIRES}
        : { 'ExtUtils::MakeMaker' => 0, };
    $meta->{prereqs}{build}{requires} = $self->{ARGS}{BUILD_REQUIRES}
        ? $self->{BUILD_REQUIRES}
        : { 'ExtUtils::MakeMaker' => 0, };
    $meta->{prereqs}{test}{requires} = $self->{TEST_REQUIRES}
        if $self->{ARGS}{TEST_REQUIRES};
    $meta->{prereqs}{runtime}{requires} = $self->{PREREQ_PM}
        if $self->{ARGS}{PREREQ_PM};
    $meta->{prereqs}{runtime}{requires}{perl} = _normalize_version($self->{MIN_PERL_VERSION})
        if $self->{MIN_PERL_VERSION};
}

# spec version of given fragment - if not given, assume 1.4
sub _metaspec_version {
  my ( $meta ) = @_;
  return $meta->{'meta-spec'}->{version}
    if defined $meta->{'meta-spec'}
       and defined $meta->{'meta-spec'}->{version};
  return '1.4';
}

sub _add_requirements_to_meta_v1_4 {
    my ( $self, $meta ) = @_;
    # Check the original args so we can tell between the user setting it
    # to an empty hash and it just being initialized.
    if( $self->{ARGS}{CONFIGURE_REQUIRES} ) {
        $meta->{configure_requires} = $self->{CONFIGURE_REQUIRES};
    } else {
        $meta->{configure_requires} = {
            'ExtUtils::MakeMaker'       => 0,
        };
    }
    if( $self->{ARGS}{BUILD_REQUIRES} ) {
        $meta->{build_requires} = $self->{BUILD_REQUIRES};
    } else {
        $meta->{build_requires} = {
            'ExtUtils::MakeMaker'       => 0,
        };
    }
    if( $self->{ARGS}{TEST_REQUIRES} ) {
        $meta->{build_requires} = {
          %{ $meta->{build_requires} },
          %{ $self->{TEST_REQUIRES} },
        };
    }
    $meta->{requires} = $self->{PREREQ_PM}
        if defined $self->{PREREQ_PM};
    $meta->{requires}{perl} = _normalize_version($self->{MIN_PERL_VERSION})
        if $self->{MIN_PERL_VERSION};
}

# Adapted from Module::Build::Base
sub _normalize_version {
  my ($version) = @_;
  $version = 0 unless defined $version;

  if ( ref $version eq 'version' ) { # version objects
    $version = $version->stringify;
  }
  elsif ( $version =~ /^[^v][^.]*\.[^.]+\./ ) { # no leading v, multiple dots
    # normalize string tuples without "v": "1.2.3" -> "v1.2.3"
    $version = "v$version";
  }
  else {
    # leave alone
  }
  return $version;
}

=item _dump_hash

    $yaml = _dump_hash(\%options, %hash);

Implements a fake YAML dumper for a hash given
as a list of pairs. No quoting/escaping is done. Keys
are supposed to be strings. Values are undef, strings,
hash refs or array refs of strings.

Supported options are:

    delta => STR - indentation delta
    use_header => BOOL - whether to include a YAML header
    indent => STR - a string of spaces
          default: ''

    max_key_length => INT - maximum key length used to align
        keys and values of the same hash
        default: 20
    key_sort => CODE - a sort sub
            It may be undef, which means no sorting by keys
        default: sub { lc $a cmp lc $b }

    customs => HASH - special options for certain keys
           (whose values are hashes themselves)
        may contain: max_key_length, key_sort, customs

=end private

=cut

sub _dump_hash {
    croak "first argument should be a hash ref" unless ref $_[0] eq 'HASH';
    my $options = shift;
    my %hash = @_;

    # Use a list to preserve order.
    my @pairs;

    my $k_sort
        = exists $options->{key_sort} ? $options->{key_sort}
                                      : sub { lc $a cmp lc $b };
    if ($k_sort) {
        croak "'key_sort' should be a coderef" unless ref $k_sort eq 'CODE';
        @pairs = _sort_pairs($k_sort, \%hash);
    } else { # list of pairs, no sorting
        @pairs = @_;
    }

    my $yaml     = $options->{use_header} ? "--- #YAML:1.0\n" : '';
    my $indent   = $options->{indent} || '';
    my $k_length = min(
        ($options->{max_key_length} || 20),
        max(map { length($_) + 1 } grep { !ref $hash{$_} } keys %hash)
    );
    my $customs  = $options->{customs} || {};

    # printf format for key
    my $k_format = "%-${k_length}s";

    while( @pairs ) {
        my($key, $val) = splice @pairs, 0, 2;
        $val = '~' unless defined $val;
        if(ref $val eq 'HASH') {
            if ( keys %$val ) {
                my %k_options = ( # options for recursive call
                    delta => $options->{delta},
                    use_header => 0,
                    indent => $indent . $options->{delta},
                );
                if (exists $customs->{$key}) {
                    my %k_custom = %{$customs->{$key}};
                    foreach my $k (qw(key_sort max_key_length customs)) {
                        $k_options{$k} = $k_custom{$k} if exists $k_custom{$k};
                    }
                }
                $yaml .= $indent . "$key:\n"
                  . _dump_hash(\%k_options, %$val);
            }
            else {
                $yaml .= $indent . "$key:  {}\n";
            }
        }
        elsif (ref $val eq 'ARRAY') {
            if( @$val ) {
                $yaml .= $indent . "$key:\n";

                for (@$val) {
                    croak "only nested arrays of non-refs are supported" if ref $_;
                    $yaml .= $indent . $options->{delta} . "- $_\n";
                }
            }
            else {
                $yaml .= $indent . "$key:  []\n";
            }
        }
        elsif( ref $val and !blessed($val) ) {
            croak "only nested hashes, arrays and objects are supported";
        }
        else {  # if it's an object, just stringify it
            $yaml .= $indent . sprintf "$k_format  %s\n", "$key:", $val;
        }
    };

    return $yaml;

}

sub blessed {
    return eval { $_[0]->isa("UNIVERSAL"); };
}

sub max {
    return (sort { $b <=> $a } @_)[0];
}

sub min {
    return (sort { $a <=> $b } @_)[0];
}

=item metafile_file

    my $meta_yml = $mm->metafile_file(@metadata_pairs);

Turns the @metadata_pairs into YAML.

This method does not implement a complete YAML dumper, being limited
to dump a hash with values which are strings, undef's or nested hashes
and arrays of strings. No quoting/escaping is done.

=cut

sub metafile_file {
    my $self = shift;

    my %dump_options = (
        use_header => 1,
        delta      => ' ' x 4,
        key_sort   => undef,
    );
    return _dump_hash(\%dump_options, @_);

}


=item distmeta_target

    my $make_frag = $mm->distmeta_target;

Generates the distmeta target to add META.yml and META.json to the MANIFEST
in the distdir.

=cut

sub distmeta_target {
    my $self = shift;

    my @add_meta = (
      $self->oneliner(<<'CODE', ['-MExtUtils::Manifest=maniadd']),
exit unless -e q{META.yml};
eval { maniadd({q{META.yml} => q{Module YAML meta-data (added by MakeMaker)}}) }
    or die "Could not add META.yml to MANIFEST: ${'@'}"
CODE
      $self->oneliner(<<'CODE', ['-MExtUtils::Manifest=maniadd'])
exit unless -f q{META.json};
eval { maniadd({q{META.json} => q{Module JSON meta-data (added by MakeMaker)}}) }
    or die "Could not add META.json to MANIFEST: ${'@'}"
CODE
    );

    my @add_meta_to_distdir = map { $self->cd('$(DISTVNAME)', $_) } @add_meta;

    return sprintf <<'MAKE', @add_meta_to_distdir;
distmeta : create_distdir metafile
	$(NOECHO) %s
	$(NOECHO) %s

MAKE

}


=item mymeta

    my $mymeta = $mm->mymeta;

Generate MYMETA information as a hash either from an existing CPAN Meta file
(META.json or META.yml) or from internal data.

=cut

sub mymeta {
    my $self = shift;
    my $file = shift || ''; # for testing

    my $mymeta = $self->_mymeta_from_meta($file);
    my $v2 = 1;

    unless ( $mymeta ) {
        $mymeta = $self->metafile_data(
            $self->{META_ADD}   || {},
            $self->{META_MERGE} || {},
        );
        $v2 = 0;
    }

    # Overwrite the non-configure dependency hashes
    $self->_add_requirements_to_meta($mymeta);

    $mymeta->{dynamic_config} = 0;

    return $mymeta;
}


sub _mymeta_from_meta {
    my $self = shift;
    my $metafile = shift || ''; # for testing

    return unless _has_cpan_meta();

    my $meta;
    for my $file ( $metafile, "META.json", "META.yml" ) {
      next unless -e $file;
      eval {
          $meta = CPAN::Meta->load_file($file)->as_struct( { version => 2 } );
      };
      last if $meta;
    }
    return unless $meta;

    # META.yml before 6.25_01 cannot be trusted.  META.yml lived in the source directory.
    # There was a good chance the author accidentally uploaded a stale META.yml if they
    # rolled their own tarball rather than using "make dist".
    if ($meta->{generated_by} &&
        $meta->{generated_by} =~ /ExtUtils::MakeMaker version ([\d\._]+)/) {
        my $eummv = do { local $^W = 0; $1+0; };
        if ($eummv < 6.2501) {
            return;
        }
    }

    return $meta;
}

=item write_mymeta

    $self->write_mymeta( $mymeta );

Write MYMETA information to MYMETA.json and MYMETA.yml.

=cut

sub write_mymeta {
    my $self = shift;
    my $mymeta = shift;

    return unless _has_cpan_meta();

    my $meta_obj = $self->_fix_metadata_before_conversion( $mymeta );

    $meta_obj->save( 'MYMETA.json', { version => "2.0" } );
    $meta_obj->save( 'MYMETA.yml', { version => "1.4" } );
    return 1;
}


=item needs_linking (o)

Does this module need linking? Looks into subdirectory objects (see
also has_link_code())

=cut

sub needs_linking {
    my($self) = shift;

    my $caller = (caller(0))[3];
    confess("needs_linking called too early") if
      $caller =~ /^ExtUtils::MakeMaker::/;
    return $self->{NEEDS_LINKING} if defined $self->{NEEDS_LINKING};
    if ($self->has_link_code or $self->{MAKEAPERL}){
	$self->{NEEDS_LINKING} = 1;
	return 1;
    }
    foreach my $child (keys %{$self->{CHILDREN}}) {
	if ($self->{CHILDREN}->{$child}->needs_linking) {
	    $self->{NEEDS_LINKING} = 1;
	    return 1;
	}
    }
    return $self->{NEEDS_LINKING} = 0;
}

=item parse_abstract

parse a file and return what you think is the ABSTRACT

=cut

sub parse_abstract {
    my($self,$parsefile) = @_;
    my $result;

    local $/ = "\n";
    open(my $fh, '<', $parsefile) or die "Could not open '$parsefile': $!";
    binmode $fh;
    my $inpod = 0;
    my $pod_encoding;
    my $package = $self->{DISTNAME};
    $package =~ s/-/::/g;
    while (<$fh>) {
        $inpod = /^=(?!cut)/ ? 1 : /^=cut/ ? 0 : $inpod;
        next if !$inpod;
        s#\r*\n\z##; # handle CRLF input

        if ( /^=encoding\s*(.*)$/i ) {
            $pod_encoding = $1;
        }

        if ( /^($package(?:\.pm)? \s+ -+ \s+)(.*)/x ) {
          $result = $2;
          next;
        }
        next unless $result;

        if ( $result && ( /^\s*$/ || /^\=/ ) ) {
          last;
        }
        $result = join ' ', $result, $_;
    }
    close $fh;

    if ( $pod_encoding and !( $] < 5.008 or !$Config{useperlio} ) ) {
        # Have to wrap in an eval{} for when running under PERL_CORE
        # Encode isn't available during build phase and parsing
        # ABSTRACT isn't important there
        eval {
          require Encode;
          $result = Encode::decode($pod_encoding, $result);
        }
    }

    return $result;
}

=item parse_version

    my $version = MM->parse_version($file);

Parse a $file and return what $VERSION is set to by the first assignment.
It will return the string "undef" if it can't figure out what $VERSION
is. $VERSION should be for all to see, so C<our $VERSION> or plain $VERSION
are okay, but C<my $VERSION> is not.

C<<package Foo VERSION>> is also checked for.  The first version
declaration found is used, but this may change as it differs from how
Perl does it.

parse_version() will try to C<use version> before checking for
C<$VERSION> so the following will work.

    $VERSION = qv(1.2.3);

Beware of the security issues for cpan testers or unchecked sources, as
the '$VERSION = ...'  line in a .pm file is simply evaluated, without
any sanity check.

=cut

sub parse_version {
    my($self,$parsefile) = @_;
    my $result;

    local $/ = "\n";
    local $_;
    open(my $fh, '<', $parsefile) or die "Could not open '$parsefile': $!";
    my $inpod = 0;
    while (<$fh>) {
        $inpod = /^=(?!cut)/ ? 1 : /^=cut/ ? 0 : $inpod;
        next if $inpod || /^\s*#/;
        chop;
        next if /^\s*(if|unless|elsif)/;
        if ( m{^ \s* package \s+ \w[\w\:\']* \s+ (v?[0-9._]+) \s* (;|\{)  }x ) {
            local $^W = 0;
            $result = $1;
        }
        elsif ( m{(?<!\\) ([\$*]) (([\w\:\']*) \bVERSION)\b .* (?<![<>=!])\=[^=]}x ) {
            $result = $self->get_version($parsefile, $1, $2);
        }
        else {
            next;
        }
        last if defined $result;
    }
    close $fh;

    if ( defined $result && $result !~ /^v?[\d_\.]+$/ ) {
        require version;
        my $normal = eval { version->new( $result ) };
        $result = $normal if defined $normal;
    }
    $result = "undef" unless defined $result;
    return $result;
}

sub get_version {
    my ($self, $parsefile, $sigil, $name) = @_;
    my $line = $_; # from the while() loop in parse_version
    {
        package ExtUtils::MakeMaker::_version;
        undef *version; # in case of unexpected version() sub
        eval {
            require version;
            version::->import;
        };
        no strict;
        local *{$name};
        local $^W = 0;
        $line = $1 if $line =~ m{^(.+)}s;
        eval($line); ## no critic
        return ${$name};
    }
}

=item pasthru (o)

Defines the string that is passed to recursive make calls in
subdirectories. The variables like C<PASTHRU_DEFINE> are used in each
level, and passed downwards on the command-line with e.g. the value of
that level's DEFINE. Example:

    # Level 0 has DEFINE = -Dfunky
    # This code will define level 0's PASTHRU=PASTHRU_DEFINE="$(DEFINE)
    #     $(PASTHRU_DEFINE)"
    # Level 0's $(CCCMD) will include macros $(DEFINE) and $(PASTHRU_DEFINE)
    # So will level 1's, so when level 1 compiles, it will get right values
    # And so ad infinitum

=cut

sub pasthru {
    my($self) = shift;
    my(@m);

    my(@pasthru);
    my($sep) = $Is{VMS} ? ',' : '';
    $sep .= "\\\n\t";

    foreach my $key (qw(LIB LIBPERL_A LINKTYPE OPTIMIZE
                     PREFIX INSTALL_BASE)
                 )
    {
        next unless defined $self->{$key};
	push @pasthru, "$key=\"\$($key)\"";
    }

    foreach my $key (qw(DEFINE INC)) {
        # default to the make var
        my $val = qq{\$($key)};
        # expand within perl if given since need to use quote_literal
        # since INC might include space-protecting ""!
        chomp($val = $self->{$key}) if defined $self->{$key};
        $val .= " \$(PASTHRU_$key)";
        my $quoted = $self->quote_literal($val);
        push @pasthru, qq{PASTHRU_$key=$quoted};
    }

    push @m, "\nPASTHRU = ", join ($sep, @pasthru), "\n";
    join "", @m;
}

=item perl_script

Takes one argument, a file name, and returns the file name, if the
argument is likely to be a perl script. On MM_Unix this is true for
any ordinary, readable file.

=cut

sub perl_script {
    my($self,$file) = @_;
    return $file if -r $file && -f _;
    return;
}

=item perldepend (o)

Defines the dependency from all *.h files that come with the perl
distribution.

=cut

sub perldepend {
    my($self) = shift;
    my(@m);

    my $make_config = $self->cd('$(PERL_SRC)', '$(MAKE) lib/Config.pm');

    push @m, sprintf <<'MAKE_FRAG', $make_config if $self->{PERL_SRC};
# Check for unpropogated config.sh changes. Should never happen.
# We do NOT just update config.h because that is not sufficient.
# An out of date config.h is not fatal but complains loudly!
$(PERL_INCDEP)/config.h: $(PERL_SRC)/config.sh
	-$(NOECHO) $(ECHO) "Warning: $(PERL_INC)/config.h out of date with $(PERL_SRC)/config.sh"; $(FALSE)

$(PERL_ARCHLIB)/Config.pm: $(PERL_SRC)/config.sh
	$(NOECHO) $(ECHO) "Warning: $(PERL_ARCHLIB)/Config.pm may be out of date with $(PERL_SRC)/config.sh"
	%s
MAKE_FRAG

    return join "", @m unless $self->needs_linking;

    if ($self->{OBJECT}) {
        # Need to add an object file dependency on the perl headers.
        # this is very important for XS modules in perl.git development.
        push @m, $self->_perl_header_files_fragment("/"); # Directory separator between $(PERL_INC)/header.h
    }

    push @m, join(" ", sort values %{$self->{XS}})." : \$(XSUBPPDEPS)\n"  if %{$self->{XS}};

    return join "\n", @m;
}


=item pm_to_blib

Defines target that copies all files in the hash PM to their
destination and autosplits them. See L<ExtUtils::Install/DESCRIPTION>

=cut

sub pm_to_blib {
    my $self = shift;
    my($autodir) = $self->catdir('$(INST_LIB)','auto');
    my $r = q{
pm_to_blib : $(FIRST_MAKEFILE) $(TO_INST_PM)
};

    # VMS will swallow '' and PM_FILTER is often empty.  So use q[]
    my $pm_to_blib = $self->oneliner(<<CODE, ['-MExtUtils::Install']);
pm_to_blib({\@ARGV}, '$autodir', q[\$(PM_FILTER)], '\$(PERM_DIR)')
CODE

    my @cmds = $self->split_command($pm_to_blib,
                  map { ($self->quote_literal($_) => $self->quote_literal($self->{PM}->{$_})) } sort keys %{$self->{PM}});

    $r .= join '', map { "\t\$(NOECHO) $_\n" } @cmds;
    $r .= qq{\t\$(NOECHO) \$(TOUCH) pm_to_blib\n};

    return $r;
}

# transform dot-separated version string into comma-separated quadruple
# examples:  '1.2.3.4.5' => '1,2,3,4'
#            '1.2.3'     => '1,2,3,0'
sub _ppd_version {
    my ($self, $string) = @_;
    return join ',', ((split /\./, $string), (0) x 4)[0..3];
}

=item ppd

Defines target that creates a PPD (Perl Package Description) file
for a binary distribution.

=cut

sub ppd {
    my($self) = @_;

    my $abstract = $self->{ABSTRACT} || '';
    $abstract =~ s/\n/\\n/sg;
    $abstract =~ s/</&lt;/g;
    $abstract =~ s/>/&gt;/g;

    my $author = join(', ',@{ ref $self->{AUTHOR} eq 'ARRAY' ? $self->{AUTHOR} : [ $self->{AUTHOR} || '']});
    $author =~ s/</&lt;/g;
    $author =~ s/>/&gt;/g;

    my $ppd_file = "$self->{DISTNAME}.ppd";

    my @ppd_chunks = qq(<SOFTPKG NAME="$self->{DISTNAME}" VERSION="$self->{VERSION}">\n);

    push @ppd_chunks, sprintf <<'PPD_HTML', $abstract, $author;
    <ABSTRACT>%s</ABSTRACT>
    <AUTHOR>%s</AUTHOR>
PPD_HTML

    push @ppd_chunks, "    <IMPLEMENTATION>\n";
    if ( $self->{MIN_PERL_VERSION} ) {
        my $min_perl_version = $self->_ppd_version($self->{MIN_PERL_VERSION});
        push @ppd_chunks, sprintf <<'PPD_PERLVERS', $min_perl_version;
        <PERLCORE VERSION="%s" />
PPD_PERLVERS

    }

    # Don't add "perl" to requires.  perl dependencies are
    # handles by ARCHITECTURE.
    my %prereqs = %{$self->{PREREQ_PM}};
    delete $prereqs{perl};

    # Build up REQUIRE
    foreach my $prereq (sort keys %prereqs) {
        my $name = $prereq;
        $name .= '::' unless $name =~ /::/;
        my $version = $prereqs{$prereq};

        my %attrs = ( NAME => $name );
        $attrs{VERSION} = $version if $version;
        my $attrs = join " ", map { qq[$_="$attrs{$_}"] } sort keys %attrs;
        push @ppd_chunks, qq(        <REQUIRE $attrs />\n);
    }

    my $archname = $Config{archname};
    if ($] >= 5.008) {
        # archname did not change from 5.6 to 5.8, but those versions may
        # not be not binary compatible so now we append the part of the
        # version that changes when binary compatibility may change
        $archname .= "-$Config{PERL_REVISION}.$Config{PERL_VERSION}";
    }
    push @ppd_chunks, sprintf <<'PPD_OUT', $archname;
        <ARCHITECTURE NAME="%s" />
PPD_OUT

    if ($self->{PPM_INSTALL_SCRIPT}) {
        if ($self->{PPM_INSTALL_EXEC}) {
            push @ppd_chunks, sprintf qq{        <INSTALL EXEC="%s">%s</INSTALL>\n},
                  $self->{PPM_INSTALL_EXEC}, $self->{PPM_INSTALL_SCRIPT};
        }
        else {
            push @ppd_chunks, sprintf qq{        <INSTALL>%s</INSTALL>\n},
                  $self->{PPM_INSTALL_SCRIPT};
        }
    }

    if ($self->{PPM_UNINSTALL_SCRIPT}) {
        if ($self->{PPM_UNINSTALL_EXEC}) {
            push @ppd_chunks, sprintf qq{        <UNINSTALL EXEC="%s">%s</UNINSTALL>\n},
                  $self->{PPM_UNINSTALL_EXEC}, $self->{PPM_UNINSTALL_SCRIPT};
        }
        else {
            push @ppd_chunks, sprintf qq{        <UNINSTALL>%s</UNINSTALL>\n},
                  $self->{PPM_UNINSTALL_SCRIPT};
        }
    }

    my ($bin_location) = $self->{BINARY_LOCATION} || '';
    $bin_location =~ s/\\/\\\\/g;

    push @ppd_chunks, sprintf <<'PPD_XML', $bin_location;
        <CODEBASE HREF="%s" />
    </IMPLEMENTATION>
</SOFTPKG>
PPD_XML

    my @ppd_cmds = $self->stashmeta(join('', @ppd_chunks), $ppd_file);

    return sprintf <<'PPD_OUT', join "\n\t", @ppd_cmds;
# Creates a PPD (Perl Package Description) for a binary distribution.
ppd :
	%s
PPD_OUT

}

=item prefixify

  $MM->prefixify($var, $prefix, $new_prefix, $default);

Using either $MM->{uc $var} || $Config{lc $var}, it will attempt to
replace it's $prefix with a $new_prefix.

Should the $prefix fail to match I<AND> a PREFIX was given as an
argument to WriteMakefile() it will set it to the $new_prefix +
$default.  This is for systems whose file layouts don't neatly fit into
our ideas of prefixes.

This is for heuristics which attempt to create directory structures
that mirror those of the installed perl.

For example:

    $MM->prefixify('installman1dir', '/usr', '/home/foo', 'man/man1');

this will attempt to remove '/usr' from the front of the
$MM->{INSTALLMAN1DIR} path (initializing it to $Config{installman1dir}
if necessary) and replace it with '/home/foo'.  If this fails it will
simply use '/home/foo/man/man1'.

=cut

sub prefixify {
    my($self,$var,$sprefix,$rprefix,$default) = @_;

    my $path = $self->{uc $var} ||
               $Config_Override{lc $var} || $Config{lc $var} || '';

    $rprefix .= '/' if $sprefix =~ m|/$|;

    warn "  prefixify $var => $path\n" if $Verbose >= 2;
    warn "    from $sprefix to $rprefix\n" if $Verbose >= 2;

    if( $self->{ARGS}{PREFIX} &&
        $path !~ s{^\Q$sprefix\E\b}{$rprefix}s )
    {

        warn "    cannot prefix, using default.\n" if $Verbose >= 2;
        warn "    no default!\n" if !$default && $Verbose >= 2;

        $path = $self->catdir($rprefix, $default) if $default;
    }

    print "    now $path\n" if $Verbose >= 2;
    return $self->{uc $var} = $path;
}


=item processPL (o)

Defines targets to run *.PL files.

=cut

sub processPL {
    my $self = shift;
    my $pl_files = $self->{PL_FILES};

    return "" unless $pl_files;

    my $m = '';
    foreach my $plfile (sort keys %$pl_files) {
        my $list = ref($pl_files->{$plfile})
                     ?  $pl_files->{$plfile}
                     : [$pl_files->{$plfile}];

        foreach my $target (@$list) {
            if( $Is{VMS} ) {
                $plfile = vmsify($self->eliminate_macros($plfile));
                $target = vmsify($self->eliminate_macros($target));
            }

            # Normally a .PL file runs AFTER pm_to_blib so it can have
            # blib in its @INC and load the just built modules.  BUT if
            # the generated module is something in $(TO_INST_PM) which
            # pm_to_blib depends on then it can't depend on pm_to_blib
            # else we have a dependency loop.
            my $pm_dep;
            my $perlrun;
            if( defined $self->{PM}{$target} ) {
                $pm_dep  = '';
                $perlrun = 'PERLRUN';
            }
            else {
                $pm_dep  = 'pm_to_blib';
                $perlrun = 'PERLRUNINST';
            }

            $m .= <<MAKE_FRAG;

all :: $target
	\$(NOECHO) \$(NOOP)

$target :: $plfile $pm_dep
	\$($perlrun) $plfile $target
MAKE_FRAG

        }
    }

    return $m;
}
    
=item realclean (o)

Defines the realclean target.

=cut

sub realclean {
    my($self, %attribs) = @_;

    my @dirs  = qw($(DISTVNAME));
    my @files = qw($(FIRST_MAKEFILE) $(MAKEFILE_OLD));

    # Special exception for the perl core where INST_* is not in blib.
    # This cleans up the files built from the ext/ directory (all XS).
    if( $self->{PERL_CORE} ) {
        push @dirs, qw($(INST_AUTODIR) $(INST_ARCHAUTODIR));
        push @files, values %{$self->{PM}};
    }

    if( $self->has_link_code ){
        push @files, qw($(OBJECT));
    }

    if( $attribs{FILES} ) {
        if( ref $attribs{FILES} ) {
            push @dirs, @{ $attribs{FILES} };
        }
        else {
            push @dirs, split /\s+/, $attribs{FILES};
        }
    }

    # Occasionally files are repeated several times from different sources
    { my(%f) = map { ($_ => 1) } @files;  @files = sort keys %f; }
    { my(%d) = map { ($_ => 1) } @dirs;   @dirs  = sort keys %d; }

    my $rm_cmd  = join "\n\t", map { "$_" }
                    $self->split_command('- $(RM_F)',  @files);
    my $rmf_cmd = join "\n\t", map { "$_" }
                    $self->split_command('- $(RM_RF)', @dirs);

    my $m = sprintf <<'MAKE', $rm_cmd, $rmf_cmd;
# Delete temporary files (via clean) and also delete dist files
realclean purge :: realclean_subdirs
	%s
	%s
MAKE

    $m .= "\t$attribs{POSTOP}\n" if $attribs{POSTOP};

    return $m;
}


=item realclean_subdirs_target

  my $make_frag = $MM->realclean_subdirs_target;

Returns the realclean_subdirs target.  This is used by the realclean
target to call realclean on any subdirectories which contain Makefiles.

=cut

sub realclean_subdirs_target {
    my $self = shift;
    my @m = <<'EOF';
# so clean is forced to complete before realclean_subdirs runs
realclean_subdirs : clean
EOF
    return join '', @m, "\t\$(NOECHO) \$(NOOP)\n" unless @{$self->{DIR}};
    foreach my $dir (@{$self->{DIR}}) {
        foreach my $makefile ('$(MAKEFILE_OLD)', '$(FIRST_MAKEFILE)' ) {
            my $subrclean .= $self->oneliner(_sprintf562 <<'CODE', $dir, $makefile);
chdir '%1$s';  system '$(MAKE) $(USEMAKEFILE) %2$s realclean' if -f '%2$s';
CODE
            push @m, "\t- $subrclean\n";
        }
    }
    return join '', @m;
}


=item signature_target

    my $target = $mm->signature_target;

Generate the signature target.

Writes the file SIGNATURE with "cpansign -s".

=cut

sub signature_target {
    my $self = shift;

    return <<'MAKE_FRAG';
signature :
	cpansign -s
MAKE_FRAG

}


=item distsignature_target

    my $make_frag = $mm->distsignature_target;

Generates the distsignature target to add SIGNATURE to the MANIFEST in the
distdir.

=cut

sub distsignature_target {
    my $self = shift;

    my $add_sign = $self->oneliner(<<'CODE', ['-MExtUtils::Manifest=maniadd']);
eval { maniadd({q{SIGNATURE} => q{Public-key signature (added by MakeMaker)}}) }
    or die "Could not add SIGNATURE to MANIFEST: ${'@'}"
CODE

    my $sign_dist        = $self->cd('$(DISTVNAME)' => 'cpansign -s');

    # cpansign -s complains if SIGNATURE is in the MANIFEST yet does not
    # exist
    my $touch_sig        = $self->cd('$(DISTVNAME)' => '$(TOUCH) SIGNATURE');
    my $add_sign_to_dist = $self->cd('$(DISTVNAME)' => $add_sign );

    return sprintf <<'MAKE', $add_sign_to_dist, $touch_sig, $sign_dist
distsignature : distmeta
	$(NOECHO) %s
	$(NOECHO) %s
	%s

MAKE

}

=item specify_shell

Specify SHELL if needed - not done on Unix.

=cut

sub specify_shell {
  return '';
}

=item quote_paren

Backslashes parentheses C<()> in command line arguments.
Doesn't handle recursive Makefile C<$(...)> constructs,
but handles simple ones.

=cut

sub quote_paren {
    my $arg = shift;
    $arg =~ s{\$\((.+?)\)}{\$\\\\($1\\\\)}g;	# protect $(...)
    $arg =~ s{(?<!\\)([()])}{\\$1}g;		# quote unprotected
    $arg =~ s{\$\\\\\((.+?)\\\\\)}{\$($1)}g;	# unprotect $(...)
    return $arg;
}

=item replace_manpage_separator

  my $man_name = $MM->replace_manpage_separator($file_path);

Takes the name of a package, which may be a nested package, in the
form 'Foo/Bar.pm' and replaces the slash with C<::> or something else
safe for a man page file name.  Returns the replacement.

=cut

sub replace_manpage_separator {
    my($self,$man) = @_;

    $man =~ s,/+,::,g;
    return $man;
}


=item cd

=cut

sub cd {
    my($self, $dir, @cmds) = @_;

    # No leading tab and no trailing newline makes for easier embedding
    my $make_frag = join "\n\t", map { "cd $dir && $_" } @cmds;

    return $make_frag;
}

=item oneliner

=cut

sub oneliner {
    my($self, $cmd, $switches) = @_;
    $switches = [] unless defined $switches;

    # Strip leading and trailing newlines
    $cmd =~ s{^\n+}{};
    $cmd =~ s{\n+$}{};

    my @cmds = split /\n/, $cmd;
    $cmd = join " \n\t  -e ", map $self->quote_literal($_), @cmds;
    $cmd = $self->escape_newlines($cmd);

    $switches = join ' ', @$switches;

    return qq{\$(ABSPERLRUN) $switches -e $cmd --};
}


=item quote_literal

    my $safe_text = $MM->quote_literal($text);
    my $safe_text = $MM->quote_literal($text, \%options);

This will quote $text so it is interpreted literally in the shell.

For example, on Unix this would escape any single-quotes in $text and
put single-quotes around the whole thing.

If $options{allow_variables} is true it will leave C<'$(FOO)'> make
variables untouched.  If false they will be escaped like any other
C<$>.  Defaults to true.

Quotes macro literal value suitable for being used on a command line so
that when expanded by make, will be received by command as given to
this method:

  my $quoted = $mm->quote_literal(q{it isn't});
  # returns:
  #   'it isn'\''t'
  print MAKEFILE "target:\n\techo $quoted\n";
  # when run "make target", will output:
  #   it isn't

=cut

sub quote_literal {
    my($self, $text, $opts) = @_;
    $opts->{allow_variables} = 1 unless defined $opts->{allow_variables};

    # Quote single quotes
    $text =~ s{'}{'\\''}g;

    $text = $opts->{allow_variables}
      ? $self->escape_dollarsigns($text) : $self->escape_all_dollarsigns($text);

    return "'$text'";
}


=item escape_newlines

=cut

sub escape_newlines {
    my($self, $text) = @_;

    $text =~ s{\n}{\\\n}g;

    return $text;
}


=item max_exec_len

Using POSIX::ARG_MAX.  Otherwise falling back to 4096.

=cut

sub max_exec_len {
    my $self = shift;

    if (!defined $self->{_MAX_EXEC_LEN}) {
        if (my $arg_max = eval { require POSIX;  &POSIX::ARG_MAX }) {
            $self->{_MAX_EXEC_LEN} = $arg_max;
        }
        else {      # POSIX minimum exec size
            $self->{_MAX_EXEC_LEN} = 4096;
        }
    }

    return $self->{_MAX_EXEC_LEN};
}


=item static (o)

Defines the static target.

=cut

sub static {
# --- Static Loading Sections ---

    my($self) = shift;
    '
## $(INST_PM) has been moved to the all: target.
## It remains here for awhile to allow for old usage: "make static"
static :: $(FIRST_MAKEFILE) $(INST_STATIC)
	$(NOECHO) $(NOOP)
';
}

sub static_lib {
    my($self) = @_;
    return '' unless $self->has_link_code;
    my(@m);
    my @libs;
    if ($self->{XSMULTI}) {
	for my $ext ($self->_xs_list_basenames) {
	    my ($v, $d, $f) = File::Spec->splitpath($ext);
	    my @d = File::Spec->splitdir($d);
	    shift @d if $d[0] eq 'lib';
	    my $instdir = $self->catdir('$(INST_ARCHLIB)', 'auto', @d, $f);
	    my $instfile = $self->catfile($instdir, "$f\$(LIB_EXT)");
	    my $objfile = "$ext\$(OBJ_EXT)";
	    push @libs, [ $objfile, $instfile, $instdir ];
	}
    } else {
	@libs = ([ qw($(OBJECT) $(INST_STATIC) $(INST_ARCHAUTODIR)) ]);
    }
    push @m, map { $self->xs_make_static_lib(@$_); } @libs;
    join "\n", @m;
}

=item xs_make_static_lib

Defines the recipes for the C<static_lib> section.

=cut

sub xs_make_static_lib {
    my ($self, $from, $to, $todir) = @_;
    my @m = sprintf '%s: %s $(MYEXTLIB) %s$(DFSEP).exists'."\n", $to, $from, $todir;
    push @m, "\t\$(RM_F) \"\$\@\"\n";
    push @m, $self->static_lib_fixtures;
    push @m, $self->static_lib_pure_cmd($from);
    push @m, "\t\$(CHMOD) \$(PERM_RWX) \$\@\n";
    push @m, $self->static_lib_closures($todir);
    join '', @m;
}

=item static_lib_closures

Records C<$(EXTRALIBS)> in F<extralibs.ld> and F<$(PERL_SRC)/ext.libs>.

=cut

sub static_lib_closures {
    my ($self, $todir) = @_;
    my @m = sprintf <<'MAKE_FRAG', $todir;
	$(NOECHO) $(ECHO) "$(EXTRALIBS)" > %s$(DFSEP)extralibs.ld
MAKE_FRAG
    # Old mechanism - still available:
    push @m, <<'MAKE_FRAG' if $self->{PERL_SRC} && $self->{EXTRALIBS};
	$(NOECHO) $(ECHO) "$(EXTRALIBS)" >> $(PERL_SRC)$(DFSEP)ext.libs
MAKE_FRAG
    @m;
}

=item static_lib_fixtures

Handles copying C<$(MYEXTLIB)> as starter for final static library that
then gets added to.

=cut

sub static_lib_fixtures {
    my ($self) = @_;
    # If this extension has its own library (eg SDBM_File)
    # then copy that to $(INST_STATIC) and add $(OBJECT) into it.
    return unless $self->{MYEXTLIB};
    "\t\$(CP) \$(MYEXTLIB) \"\$\@\"\n";
}

=item static_lib_pure_cmd

Defines how to run the archive utility.

=cut

sub static_lib_pure_cmd {
    my ($self, $from) = @_;
    my $ar;
    if (exists $self->{FULL_AR} && -x $self->{FULL_AR}) {
        # Prefer the absolute pathed ar if available so that PATH
        # doesn't confuse us.  Perl itself is built with the full_ar.
        $ar = 'FULL_AR';
    } else {
        $ar = 'AR';
    }
    sprintf <<'MAKE_FRAG', $ar, $from;
	$(%s) $(AR_STATIC_ARGS) "$@" %s
	$(RANLIB) "$@"
MAKE_FRAG
}

=item staticmake (o)

Calls makeaperl.

=cut

sub staticmake {
    my($self, %attribs) = @_;
    my(@static);

    my(@searchdirs)=($self->{PERL_ARCHLIB}, $self->{SITEARCHEXP},  $self->{INST_ARCHLIB});

    # And as it's not yet built, we add the current extension
    # but only if it has some C code (or XS code, which implies C code)
    if (@{$self->{C}}) {
	@static = $self->catfile($self->{INST_ARCHLIB},
				 "auto",
				 $self->{FULLEXT},
				 "$self->{BASEEXT}$self->{LIB_EXT}"
				);
    }

    # Either we determine now, which libraries we will produce in the
    # subdirectories or we do it at runtime of the make.

    # We could ask all subdir objects, but I cannot imagine, why it
    # would be necessary.

    # Instead we determine all libraries for the new perl at
    # runtime.
    my(@perlinc) = ($self->{INST_ARCHLIB}, $self->{INST_LIB}, $self->{PERL_ARCHLIB}, $self->{PERL_LIB});

    $self->makeaperl(MAKE	=> $self->{MAKEFILE},
		     DIRS	=> \@searchdirs,
		     STAT	=> \@static,
		     INCL	=> \@perlinc,
		     TARGET	=> $self->{MAP_TARGET},
		     TMP	=> "",
		     LIBPERL	=> $self->{LIBPERL_A}
		    );
}

=item subdir_x (o)

Helper subroutine for subdirs

=cut

sub subdir_x {
    my($self, $subdir) = @_;

    my $subdir_cmd = $self->cd($subdir,
      '$(MAKE) $(USEMAKEFILE) $(FIRST_MAKEFILE) all $(PASTHRU)'
    );
    return sprintf <<'EOT', $subdir_cmd;

subdirs ::
	$(NOECHO) %s
EOT

}

=item subdirs (o)

Defines targets to process subdirectories.

=cut

sub subdirs {
# --- Sub-directory Sections ---
    my($self) = shift;
    my(@m);
    # This method provides a mechanism to automatically deal with
    # subdirectories containing further Makefile.PL scripts.
    # It calls the subdir_x() method for each subdirectory.
    foreach my $dir (@{$self->{DIR}}){
	push @m, $self->subdir_x($dir);
####	print "Including $dir subdirectory\n";
    }
    if (@m){
	unshift @m, <<'EOF';

# The default clean, realclean and test targets in this Makefile
# have automatically been given entries for each subdir.

EOF
    } else {
	push(@m, "\n# none")
    }
    join('',@m);
}

=item test (o)

Defines the test targets.

=cut

sub test {
    my($self, %attribs) = @_;
    my $tests = $attribs{TESTS} || '';
    if (!$tests && -d 't' && defined $attribs{RECURSIVE_TEST_FILES}) {
        $tests = $self->find_tests_recursive;
    }
    elsif (!$tests && -d 't') {
        $tests = $self->find_tests;
    }
    # have to do this because nmake is broken
    $tests =~ s!/!\\!g if $self->is_make_type('nmake');
    # note: 'test.pl' name is also hardcoded in init_dirscan()
    my @m;
    my $default_testtype = $Config{usedl} ? 'dynamic' : 'static';
    push @m, <<EOF;
TEST_VERBOSE=0
TEST_TYPE=test_\$(LINKTYPE)
TEST_FILE = test.pl
TEST_FILES = $tests
TESTDB_SW = -d

testdb :: testdb_\$(LINKTYPE)
	\$(NOECHO) \$(NOOP)

test :: \$(TEST_TYPE)
	\$(NOECHO) \$(NOOP)

# Occasionally we may face this degenerate target:
test_ : test_$default_testtype
	\$(NOECHO) \$(NOOP)

EOF

    for my $linktype (qw(dynamic static)) {
        my $directdeps = "$linktype pure_all";
        push @m, "subdirs-test_$linktype :: $directdeps\n";
        foreach my $dir (@{ $self->{DIR} }) {
            my $test = $self->cd($dir, "\$(MAKE) test_$linktype \$(PASTHRU)");
            push @m, "\t\$(NOECHO) $test\n";
        }
        push @m, "\n";
        if ($tests or -f "test.pl") {
            for my $testspec ([ '', '' ], [ 'db', ' $(TESTDB_SW)' ]) {
                my ($db, $switch) = @$testspec;
                my ($command, $deps);
                # if testdb, build all but don't test all
                $deps = $db eq 'db' ? $directdeps : "subdirs-test_$linktype";
                if ($linktype eq 'static' and $self->needs_linking) {
                    my $target = File::Spec->rel2abs('$(MAP_TARGET)');
                    $command = qq{"$target" \$(MAP_PERLINC)};
                    $deps .= ' $(MAP_TARGET)';
                } else {
                    $command = '$(FULLPERLRUN)' . $switch;
                }
                push @m, "test${db}_$linktype :: $deps\n";
                if ($db eq 'db') {
                    push @m, $self->test_via_script($command, '$(TEST_FILE)')
                } else {
                    push @m, $self->test_via_script($command, '$(TEST_FILE)')
                        if -f "test.pl";
                    push @m, $self->test_via_harness($command, '$(TEST_FILES)')
                        if $tests;
                }
                push @m, "\n";
            }
        } else {
            push @m, _sprintf562 <<'EOF', $linktype;
testdb_%1$s test_%1$s :: subdirs-test_%1$s
	$(NOECHO) $(ECHO) 'No tests defined for $(NAME) extension.'

EOF
        }
    }

    join "", @m;
}


=item tool_xsubpp (o)

Determines typemaps, xsubpp version, prototype behaviour.

=cut

sub tool_xsubpp {
    my($self) = shift;
    return "" unless $self->needs_linking;

    my $xsdir;
    my @xsubpp_dirs = @INC;

    # Make sure we pick up the new xsubpp if we're building perl.
    unshift @xsubpp_dirs, $self->{PERL_LIB} if $self->{PERL_CORE};

    my $foundxsubpp = 0;
    foreach my $dir (@xsubpp_dirs) {
        $xsdir = $self->catdir($dir, 'ExtUtils');
        if( -r $self->catfile($xsdir, "xsubpp") ) {
            $foundxsubpp = 1;
            last;
        }
    }
    die "ExtUtils::MM_Unix::tool_xsubpp : Can't find xsubpp" if !$foundxsubpp;

    my $tmdir   = $self->catdir($self->{PERL_LIB},"ExtUtils");
    my(@tmdeps) = $self->catfile($tmdir,'typemap');
    if( $self->{TYPEMAPS} ){
        foreach my $typemap (@{$self->{TYPEMAPS}}){
            if( ! -f  $typemap ) {
                warn "Typemap $typemap not found.\n";
            }
            else {
                $typemap = vmsify($typemap) if $Is{VMS};
                push(@tmdeps, $typemap);
            }
        }
    }
    push(@tmdeps, "typemap") if -f "typemap";
    # absolutised because with deep-located typemaps, eg "lib/XS/typemap",
    # if xsubpp is called from top level with
    #     $(XSUBPP) ... -typemap "lib/XS/typemap" "lib/XS/Test.xs"
    # it says:
    #     Can't find lib/XS/type map in (fulldir)/lib/XS
    # because ExtUtils::ParseXS::process_file chdir's to .xs file's
    # location. This is the only way to get all specified typemaps used,
    # wherever located.
    my @tmargs = map { '-typemap '.$self->quote_literal(File::Spec->rel2abs($_)) } @tmdeps;
    $_ = $self->quote_dep($_) for @tmdeps;
    if( exists $self->{XSOPT} ){
        unshift( @tmargs, $self->{XSOPT} );
    }

    if ($Is{VMS}                          &&
        $Config{'ldflags'}               &&
        $Config{'ldflags'} =~ m!/Debug!i &&
        (!exists($self->{XSOPT}) || $self->{XSOPT} !~ /linenumbers/)
       )
    {
        unshift(@tmargs,'-nolinenumbers');
    }


    $self->{XSPROTOARG} = "" unless defined $self->{XSPROTOARG};
    my $xsdirdep = $self->quote_dep($xsdir);
    # -dep for use when dependency not command

    return qq{
XSUBPPDIR = $xsdir
XSUBPP = "\$(XSUBPPDIR)\$(DFSEP)xsubpp"
XSUBPPRUN = \$(PERLRUN) \$(XSUBPP)
XSPROTOARG = $self->{XSPROTOARG}
XSUBPPDEPS = @tmdeps $xsdirdep\$(DFSEP)xsubpp
XSUBPPARGS = @tmargs
XSUBPP_EXTRA_ARGS =
};
}


=item top_targets (o)

Defines the targets all, subdirs, config, and O_FILES

=cut

sub top_targets {
# --- Target Sections ---

    my($self) = shift;
    my(@m);

    push @m, $self->all_target, "\n" unless $self->{SKIPHASH}{'all'};

    push @m, sprintf <<'EOF';
pure_all :: config pm_to_blib subdirs linkext
	$(NOECHO) $(NOOP)

	$(NOECHO) $(NOOP)

subdirs :: $(MYEXTLIB)
	$(NOECHO) $(NOOP)

config :: $(FIRST_MAKEFILE) blibdirs
	$(NOECHO) $(NOOP)
EOF

    push @m, '
$(O_FILES) : $(H_FILES)
' if @{$self->{O_FILES} || []} && @{$self->{H} || []};

    push @m, q{
help :
	perldoc ExtUtils::MakeMaker
};

    join('',@m);
}

=item writedoc

Obsolete, deprecated method. Not used since Version 5.21.

=cut

sub writedoc {
# --- perllocal.pod section ---
    my($self,$what,$name,@attribs)=@_;
    my $time = gmtime($ENV{SOURCE_DATE_EPOCH} || time);
    print "=head2 $time: $what C<$name>\n\n=over 4\n\n=item *\n\n";
    print join "\n\n=item *\n\n", map("C<$_>",@attribs);
    print "\n\n=back\n\n";
}

=item xs_c (o)

Defines the suffix rules to compile XS files to C.

=cut

sub xs_c {
    my($self) = shift;
    return '' unless $self->needs_linking();
    '
.xs.c:
	$(XSUBPPRUN) $(XSPROTOARG) $(XSUBPPARGS) $(XSUBPP_EXTRA_ARGS) $*.xs > $*.xsc
	$(MV) $*.xsc $*.c
';
}

=item xs_cpp (o)

Defines the suffix rules to compile XS files to C++.

=cut

sub xs_cpp {
    my($self) = shift;
    return '' unless $self->needs_linking();
    '
.xs.cpp:
	$(XSUBPPRUN) $(XSPROTOARG) $(XSUBPPARGS) $*.xs > $*.xsc
	$(MV) $*.xsc $*.cpp
';
}

=item xs_o (o)

Defines suffix rules to go from XS to object files directly. This was
originally only intended for broken make implementations, but is now
necessary for per-XS file under C<XSMULTI>, since each XS file might
have an individual C<$(VERSION)>.

=cut

sub xs_o {
    my ($self) = @_;
    return '' unless $self->needs_linking();
    my $m_o = $self->{XSMULTI} ? $self->xs_obj_opt('$*$(OBJ_EXT)') : '';
    my $frag = '';
    # dmake makes noise about ambiguous rule
    $frag .= sprintf <<'EOF', $m_o unless $self->is_make_type('dmake');
.xs$(OBJ_EXT) :
	$(XSUBPPRUN) $(XSPROTOARG) $(XSUBPPARGS) $*.xs > $*.xsc
	$(MV) $*.xsc $*.c
	$(CCCMD) $(CCCDLFLAGS) "-I$(PERL_INC)" $(PASTHRU_DEFINE) $(DEFINE) $*.c %s
EOF
    if ($self->{XSMULTI}) {
	for my $ext ($self->_xs_list_basenames) {
	    my $pmfile = "$ext.pm";
	    croak "$ext.xs has no matching $pmfile: $!" unless -f $pmfile;
	    my $version = $self->parse_version($pmfile);
	    my $cccmd = $self->{CONST_CCCMD};
	    $cccmd =~ s/^\s*CCCMD\s*=\s*//;
	    $cccmd =~ s/\$\(DEFINE_VERSION\)/-DVERSION=\\"$version\\"/;
	    $cccmd =~ s/\$\(XS_DEFINE_VERSION\)/-DXS_VERSION=\\"$version\\"/;
            $self->_xsbuild_replace_macro($cccmd, 'xs', $ext, 'INC');
            my $define = '$(DEFINE)';
            $self->_xsbuild_replace_macro($define, 'xs', $ext, 'DEFINE');
            #                             1     2       3     4
            $frag .= _sprintf562 <<'EOF', $ext, $cccmd, $m_o, $define;

%1$s$(OBJ_EXT): %1$s.xs
	$(XSUBPPRUN) $(XSPROTOARG) $(XSUBPPARGS) $*.xs > $*.xsc
	$(MV) $*.xsc $*.c
	%2$s $(CCCDLFLAGS) "-I$(PERL_INC)" $(PASTHRU_DEFINE) %4$s $*.c %3$s
EOF
	}
    }
    $frag;
}

# param gets modified
sub _xsbuild_replace_macro {
    my ($self, undef, $xstype, $ext, $varname) = @_;
    my $value = $self->_xsbuild_value($xstype, $ext, $varname);
    return unless defined $value;
    $_[1] =~ s/\$\($varname\)/$value/;
}

sub _xsbuild_value {
    my ($self, $xstype, $ext, $varname) = @_;
    return $self->{XSBUILD}{$xstype}{$ext}{$varname}
        if $self->{XSBUILD}{$xstype}{$ext}{$varname};
    return $self->{XSBUILD}{$xstype}{all}{$varname}
        if $self->{XSBUILD}{$xstype}{all}{$varname};
    ();
}

=back

=itemspecial_targets

  my $make_frag = $mm->special_targets

Returns a make fragment containing any targets which have special
meaning to make.  For example, .SUFFIXES and .PHONY.

=cut

sub special_targets {
    my $make_frag = <<'MAKE_FRAG';
.SUFFIXES : .xs .c .C .cpp .i .s .cxx .cc $(OBJ_EXT)

.PHONY: all config static dynamic test linkext manifest blibdirs clean realclean disttest distdir pure_all subdirs clean_subdirs makemakerdflt manifypods realclean_subdirs subdirs_dynamic subdirs_pure_nolink subdirs_static subdirs-test_dynamic subdirs-test_static test_dynamic test_static

MAKE_FRAG

    $make_frag .= <<'MAKE_FRAG' if $ENV{CLEARCASE_ROOT};
.NO_CONFIG_REC: Makefile

MAKE_FRAG

    return $make_frag;
}


=head2 Init methods

Methods which help initialize the MakeMaker object and macros.

=over

=item init_ABSTRACT

    $mm->init_ABSTRACT

=cut

sub init_ABSTRACT {
    my $self = shift;

    if( $self->{ABSTRACT_FROM} and $self->{ABSTRACT} ) {
        warn "Both ABSTRACT_FROM and ABSTRACT are set.  ".
             "Ignoring ABSTRACT_FROM.\n";
        return;
    }

    if ($self->{ABSTRACT_FROM}){
        $self->{ABSTRACT} = $self->parse_abstract($self->{ABSTRACT_FROM}) or
            carp "WARNING: Setting ABSTRACT via file ".
                 "'$self->{ABSTRACT_FROM}' failed\n";
    }

    if ($self->{ABSTRACT} && $self->{ABSTRACT} =~ m![[:cntrl:]]+!) {
            warn "WARNING: ABSTRACT contains control character(s),".
                 " they will be removed\n";
            $self->{ABSTRACT} =~ s![[:cntrl:]]+!!g;
            return;
    }
}

=item init_INST (o)

    $mm->init_INST;

Called by EUMM->new.  Sets up all INST_* variables except those related
to XS code.  Those are handled in init_xs.

=cut

sub init_INST {
    my($self) = shift;

    $self->{INST_ARCHLIB} ||= $self->catdir($Curdir,"blib","arch");
    $self->{INST_BIN}     ||= $self->catdir($Curdir,'blib','bin');

    # INST_LIB typically pre-set if building an extension after
    # perl has been built and installed. Setting INST_LIB allows
    # you to build directly into, say $Config{privlibexp}.
    unless ($self->{INST_LIB}){
        if ($self->{PERL_CORE}) {
            $self->{INST_LIB} = $self->{INST_ARCHLIB} = $self->{PERL_LIB};
        } else {
            $self->{INST_LIB} = $self->catdir($Curdir,"blib","lib");
        }
    }

    my @parentdir = split(/::/, $self->{PARENT_NAME});
    $self->{INST_LIBDIR}      = $self->catdir('$(INST_LIB)',     @parentdir);
    $self->{INST_ARCHLIBDIR}  = $self->catdir('$(INST_ARCHLIB)', @parentdir);
    $self->{INST_AUTODIR}     = $self->catdir('$(INST_LIB)', 'auto',
                                              '$(FULLEXT)');
    $self->{INST_ARCHAUTODIR} = $self->catdir('$(INST_ARCHLIB)', 'auto',
                                              '$(FULLEXT)');

    $self->{INST_SCRIPT}  ||= $self->catdir($Curdir,'blib','script');

    $self->{INST_MAN1DIR} ||= $self->catdir($Curdir,'blib','man1');
    $self->{INST_MAN3DIR} ||= $self->catdir($Curdir,'blib','man3');

    return 1;
}


=item init_INSTALL (o)

    $mm->init_INSTALL;

Called by EUMM->new.  Sets up all INSTALL_* variables (except
INSTALLDIRS) and *PREFIX.

=cut

sub init_INSTALL {
    my($self) = shift;

    if( $self->{ARGS}{INSTALL_BASE} and $self->{ARGS}{PREFIX} ) {
        die "Only one of PREFIX or INSTALL_BASE can be given.  Not both.\n";
    }

    if( $self->{ARGS}{INSTALL_BASE} ) {
        $self->init_INSTALL_from_INSTALL_BASE;
    }
    else {
        $self->init_INSTALL_from_PREFIX;
    }
}


=item init_INSTALL_from_PREFIX

  $mm->init_INSTALL_from_PREFIX;

=cut

sub init_INSTALL_from_PREFIX {
    my $self = shift;

    $self->init_lib2arch;

    # There are often no Config.pm defaults for these new man variables so
    # we fall back to the old behavior which is to use installman*dir
    foreach my $num (1, 3) {
        my $k = 'installsiteman'.$num.'dir';

        $self->{uc $k} ||= uc "\$(installman${num}dir)"
          unless $Config{$k};
    }

    foreach my $num (1, 3) {
        my $k = 'installvendorman'.$num.'dir';

        unless( $Config{$k} ) {
            $self->{uc $k}  ||= $Config{usevendorprefix}
                              ? uc "\$(installman${num}dir)"
                              : '';
        }
    }

    $self->{INSTALLSITEBIN} ||= '$(INSTALLBIN)'
      unless $Config{installsitebin};
    $self->{INSTALLSITESCRIPT} ||= '$(INSTALLSCRIPT)'
      unless $Config{installsitescript};

    unless( $Config{installvendorbin} ) {
        $self->{INSTALLVENDORBIN} ||= $Config{usevendorprefix}
                                    ? $Config{installbin}
                                    : '';
    }
    unless( $Config{installvendorscript} ) {
        $self->{INSTALLVENDORSCRIPT} ||= $Config{usevendorprefix}
                                       ? $Config{installscript}
                                       : '';
    }


    my $iprefix = $Config{installprefixexp} || $Config{installprefix} ||
                  $Config{prefixexp}        || $Config{prefix} || '';
    my $vprefix = $Config{usevendorprefix}  ? $Config{vendorprefixexp} : '';
    my $sprefix = $Config{siteprefixexp}    || '';

    # 5.005_03 doesn't have a siteprefix.
    $sprefix = $iprefix unless $sprefix;


    $self->{PREFIX}       ||= '';

    if( $self->{PREFIX} ) {
        @{$self}{qw(PERLPREFIX SITEPREFIX VENDORPREFIX)} =
          ('$(PREFIX)') x 3;
    }
    else {
        $self->{PERLPREFIX}   ||= $iprefix;
        $self->{SITEPREFIX}   ||= $sprefix;
        $self->{VENDORPREFIX} ||= $vprefix;

        # Lots of MM extension authors like to use $(PREFIX) so we
        # put something sensible in there no matter what.
        $self->{PREFIX} = '$('.uc $self->{INSTALLDIRS}.'PREFIX)';
    }

    my $arch    = $Config{archname};
    my $version = $Config{version};

    # default style
    my $libstyle = $Config{installstyle} || 'lib/perl5';
    my $manstyle = '';

    if( $self->{LIBSTYLE} ) {
        $libstyle = $self->{LIBSTYLE};
        $manstyle = $self->{LIBSTYLE} eq 'lib/perl5' ? 'lib/perl5' : '';
    }

    # Some systems, like VOS, set installman*dir to '' if they can't
    # read man pages.
    for my $num (1, 3) {
        $self->{'INSTALLMAN'.$num.'DIR'} ||= 'none'
          unless $Config{'installman'.$num.'dir'};
    }

    my %bin_layouts =
    (
        bin         => { s => $iprefix,
                         t => 'perl',
                         d => 'bin' },
        vendorbin   => { s => $vprefix,
                         t => 'vendor',
                         d => 'bin' },
        sitebin     => { s => $sprefix,
                         t => 'site',
                         d => 'bin' },
        script      => { s => $iprefix,
                         t => 'perl',
                         d => 'bin' },
        vendorscript=> { s => $vprefix,
                         t => 'vendor',
                         d => 'bin' },
        sitescript  => { s => $sprefix,
                         t => 'site',
                         d => 'bin' },
    );

    my %man_layouts =
    (
        man1dir         => { s => $iprefix,
                             t => 'perl',
                             d => 'man/man1',
                             style => $manstyle, },
        siteman1dir     => { s => $sprefix,
                             t => 'site',
                             d => 'man/man1',
                             style => $manstyle, },
        vendorman1dir   => { s => $vprefix,
                             t => 'vendor',
                             d => 'man/man1',
                             style => $manstyle, },

        man3dir         => { s => $iprefix,
                             t => 'perl',
                             d => 'man/man3',
                             style => $manstyle, },
        siteman3dir     => { s => $sprefix,
                             t => 'site',
                             d => 'man/man3',
                             style => $manstyle, },
        vendorman3dir   => { s => $vprefix,
                             t => 'vendor',
                             d => 'man/man3',
                             style => $manstyle, },
    );

    my %lib_layouts =
    (
        privlib     => { s => $iprefix,
                         t => 'perl',
                         d => '',
                         style => $libstyle, },
        vendorlib   => { s => $vprefix,
                         t => 'vendor',
                         d => '',
                         style => $libstyle, },
        sitelib     => { s => $sprefix,
                         t => 'site',
                         d => 'site_perl',
                         style => $libstyle, },

        archlib     => { s => $iprefix,
                         t => 'perl',
                         d => "$version/$arch",
                         style => $libstyle },
        vendorarch  => { s => $vprefix,
                         t => 'vendor',
                         d => "$version/$arch",
                         style => $libstyle },
        sitearch    => { s => $sprefix,
                         t => 'site',
                         d => "site_perl/$version/$arch",
                         style => $libstyle },
    );


    # Special case for LIB.
    if( $self->{LIB} ) {
        foreach my $var (keys %lib_layouts) {
            my $Installvar = uc "install$var";

            if( $var =~ /arch/ ) {
                $self->{$Installvar} ||=
                  $self->catdir($self->{LIB}, $Config{archname});
            }
            else {
                $self->{$Installvar} ||= $self->{LIB};
            }
        }
    }

    my %type2prefix = ( perl    => 'PERLPREFIX',
                        site    => 'SITEPREFIX',
                        vendor  => 'VENDORPREFIX'
                      );

    my %layouts = (%bin_layouts, %man_layouts, %lib_layouts);
    while( my($var, $layout) = each(%layouts) ) {
        my($s, $t, $d, $style) = @{$layout}{qw(s t d style)};
        my $r = '$('.$type2prefix{$t}.')';

        warn "Prefixing $var\n" if $Verbose >= 2;

        my $installvar = "install$var";
        my $Installvar = uc $installvar;
        next if $self->{$Installvar};

        $d = "$style/$d" if $style;
        $self->prefixify($installvar, $s, $r, $d);

        warn "  $Installvar == $self->{$Installvar}\n"
          if $Verbose >= 2;
    }

    # Generate these if they weren't figured out.
    $self->{VENDORARCHEXP} ||= $self->{INSTALLVENDORARCH};
    $self->{VENDORLIBEXP}  ||= $self->{INSTALLVENDORLIB};

    return 1;
}


=item init_from_INSTALL_BASE

    $mm->init_from_INSTALL_BASE

=cut

my %map = (
           lib      => [qw(lib perl5)],
           arch     => [('lib', 'perl5', $Config{archname})],
           bin      => [qw(bin)],
           man1dir  => [qw(man man1)],
           man3dir  => [qw(man man3)]
          );
$map{script} = $map{bin};

sub init_INSTALL_from_INSTALL_BASE {
    my $self = shift;

    @{$self}{qw(PREFIX VENDORPREFIX SITEPREFIX PERLPREFIX)} =
                                                         '$(INSTALL_BASE)';

    my %install;
    foreach my $thing (keys %map) {
        foreach my $dir (('', 'SITE', 'VENDOR')) {
            my $uc_thing = uc $thing;
            my $key = "INSTALL".$dir.$uc_thing;

            $install{$key} ||=
              $self->catdir('$(INSTALL_BASE)', @{$map{$thing}});
        }
    }

    # Adjust for variable quirks.
    $install{INSTALLARCHLIB} ||= delete $install{INSTALLARCH};
    $install{INSTALLPRIVLIB} ||= delete $install{INSTALLLIB};

    foreach my $key (keys %install) {
        $self->{$key} ||= $install{$key};
    }

    return 1;
}


=item init_dirscan (o)

Scans the directory structure and initializes DIR, XS, XS_FILES,
C, C_FILES, O_FILES, H, H_FILES, PL_FILES, EXE_FILES.

Called by EUMM->new.

=cut

sub init_dirscan {	# --- File and Directory Lists (.xs .pm .pod etc)
    my($self) = @_;
    my(%dir, %xs, %c, %o, %h, %pl_files, %pm);

    my %ignore = map {( $_ => 1 )} qw(Makefile.PL Build.PL test.pl t);

    # ignore the distdir
    $Is{VMS} ? $ignore{"$self->{DISTVNAME}.dir"} = 1
            : $ignore{$self->{DISTVNAME}} = 1;

    my $distprefix = $Is{VMS} ? qr/^\Q$self->{DISTNAME}\E-v?[\d\.]+\.dir$/i
                              : qr/^\Q$self->{DISTNAME}\E-v?[\d\.]+$/;

    @ignore{map lc, keys %ignore} = values %ignore if $Is{VMS};

    if ( defined $self->{XS} and !defined $self->{C} ) {
	my @c_files = grep { m/\.c(pp|xx)?\z/i } values %{$self->{XS}};
	my @o_files = grep { m/(?:.(?:o(?:bj)?)|\$\(OBJ_EXT\))\z/i } values %{$self->{XS}};
	%c = map { $_ => 1 } @c_files;
	%o = map { $_ => 1 } @o_files;
    }

    foreach my $name ($self->lsdir($Curdir)){
	next if $name =~ /\#/;
	next if $name =~ $distprefix && -d $name;
	$name = lc($name) if $Is{VMS};
	next if $name eq $Curdir or $name eq $Updir or $ignore{$name};
	next unless $self->libscan($name);
	if (-d $name){
	    next if -l $name; # We do not support symlinks at all
            next if $self->{NORECURS};
	    $dir{$name} = $name if (-f $self->catfile($name,"Makefile.PL"));
	} elsif ($name =~ /\.xs\z/){
	    my($c); ($c = $name) =~ s/\.xs\z/.c/;
	    $xs{$name} = $c;
	    $c{$c} = 1;
	} elsif ($name =~ /\.c(pp|xx|c)?\z/i){  # .c .C .cpp .cxx .cc
	    $c{$name} = 1
		unless $name =~ m/perlmain\.c/; # See MAP_TARGET
	} elsif ($name =~ /\.h\z/i){
	    $h{$name} = 1;
	} elsif ($name =~ /\.PL\z/) {
	    ($pl_files{$name} = $name) =~ s/\.PL\z// ;
	} elsif (($Is{VMS} || $Is{Dos}) && $name =~ /[._]pl$/i) {
	    # case-insensitive filesystem, one dot per name, so foo.h.PL
	    # under Unix appears as foo.h_pl under VMS or fooh.pl on Dos
	    local($/); open(my $pl, '<', $name); my $txt = <$pl>; close $pl;
	    if ($txt =~ /Extracting \S+ \(with variable substitutions/) {
		($pl_files{$name} = $name) =~ s/[._]pl\z//i ;
	    }
	    else {
                $pm{$name} = $self->catfile($self->{INST_LIBDIR},$name);
            }
	} elsif ($name =~ /\.(p[ml]|pod)\z/){
	    $pm{$name} = $self->catfile($self->{INST_LIBDIR},$name);
	}
    }

    $self->{PL_FILES}   ||= \%pl_files;
    $self->{DIR}        ||= [sort keys %dir];
    $self->{XS}         ||= \%xs;
    $self->{C}          ||= [sort keys %c];
    $self->{H}          ||= [sort keys %h];
    $self->{PM}         ||= \%pm;

    my @o_files = @{$self->{C}};
    %o = (%o, map { $_ => 1 } grep s/\.c(pp|xx|c)?\z/$self->{OBJ_EXT}/i, @o_files);
    $self->{O_FILES} = [sort keys %o];
}


=item init_MANPODS (o)

Determines if man pages should be generated and initializes MAN1PODS
and MAN3PODS as appropriate.

=cut

sub init_MANPODS {
    my $self = shift;

    # Set up names of manual pages to generate from pods
    foreach my $man (qw(MAN1 MAN3)) {
        if ( $self->{"${man}PODS"}
             or $self->{"INSTALL${man}DIR"} =~ /^(none|\s*)$/
        ) {
            $self->{"${man}PODS"} ||= {};
        }
        else {
            my $init_method = "init_${man}PODS";
            $self->$init_method();
        }
    }
}


sub _has_pod {
    my($self, $file) = @_;

    my($ispod)=0;
    if (open( my $fh, '<', $file )) {
        while (<$fh>) {
            if (/^=(?:head\d+|item|pod)\b/) {
                $ispod=1;
                last;
            }
        }
        close $fh;
    } else {
        # If it doesn't exist yet, we assume, it has pods in it
        $ispod = 1;
    }

    return $ispod;
}


=item init_MAN1PODS

Initializes MAN1PODS from the list of EXE_FILES.

=cut

sub init_MAN1PODS {
    my($self) = @_;

    if ( exists $self->{EXE_FILES} ) {
	foreach my $name (@{$self->{EXE_FILES}}) {
	    next unless $self->_has_pod($name);

	    $self->{MAN1PODS}->{$name} =
		$self->catfile("\$(INST_MAN1DIR)",
			       basename($name).".\$(MAN1EXT)");
	}
    }
}


=item init_MAN3PODS

Initializes MAN3PODS from the list of PM files.

=cut

sub init_MAN3PODS {
    my $self = shift;

    my %manifypods = (); # we collect the keys first, i.e. the files
                         # we have to convert to pod

    foreach my $name (keys %{$self->{PM}}) {
	if ($name =~ /\.pod\z/ ) {
	    $manifypods{$name} = $self->{PM}{$name};
	} elsif ($name =~ /\.p[ml]\z/ ) {
	    if( $self->_has_pod($name) ) {
		$manifypods{$name} = $self->{PM}{$name};
	    }
	}
    }

    my $parentlibs_re = join '|', @{$self->{PMLIBPARENTDIRS}};

    # Remove "Configure.pm" and similar, if it's not the only pod listed
    # To force inclusion, just name it "Configure.pod", or override
    # MAN3PODS
    foreach my $name (keys %manifypods) {
	if (
            ($self->{PERL_CORE} and $name =~ /(config|setup).*\.pm/is) or
            ( $name =~ m/^README\.pod$/i ) # don't manify top-level README.pod
        ) {
	    delete $manifypods{$name};
	    next;
	}
	my($manpagename) = $name;
	$manpagename =~ s/\.p(od|m|l)\z//;
	# everything below lib is ok
	unless($manpagename =~ s!^\W*($parentlibs_re)\W+!!s) {
	    $manpagename = $self->catfile(
	        split(/::/,$self->{PARENT_NAME}),$manpagename
	    );
	}
	$manpagename = $self->replace_manpage_separator($manpagename);
	$self->{MAN3PODS}->{$name} =
	    $self->catfile("\$(INST_MAN3DIR)", "$manpagename.\$(MAN3EXT)");
    }
}


=item init_PM (o)

Initializes PMLIBDIRS and PM from PMLIBDIRS.

=cut

sub init_PM {
    my $self = shift;

    # Some larger extensions often wish to install a number of *.pm/pl
    # files into the library in various locations.

    # The attribute PMLIBDIRS holds an array reference which lists
    # subdirectories which we should search for library files to
    # install. PMLIBDIRS defaults to [ 'lib', $self->{BASEEXT} ].  We
    # recursively search through the named directories (skipping any
    # which don't exist or contain Makefile.PL files).

    # For each *.pm or *.pl file found $self->libscan() is called with
    # the default installation path in $_[1]. The return value of
    # libscan defines the actual installation location.  The default
    # libscan function simply returns the path.  The file is skipped
    # if libscan returns false.

    # The default installation location passed to libscan in $_[1] is:
    #
    #  ./*.pm		=> $(INST_LIBDIR)/*.pm
    #  ./xyz/...	=> $(INST_LIBDIR)/xyz/...
    #  ./lib/...	=> $(INST_LIB)/...
    #
    # In this way the 'lib' directory is seen as the root of the actual
    # perl library whereas the others are relative to INST_LIBDIR
    # (which includes PARENT_NAME). This is a subtle distinction but one
    # that's important for nested modules.

    unless( $self->{PMLIBDIRS} ) {
        if( $Is{VMS} ) {
            # Avoid logical name vs directory collisions
            $self->{PMLIBDIRS} = ['./lib', "./$self->{BASEEXT}"];
        }
        else {
            $self->{PMLIBDIRS} = ['lib', $self->{BASEEXT}];
        }
    }

    #only existing directories that aren't in $dir are allowed

    # Avoid $_ wherever possible:
    # @{$self->{PMLIBDIRS}} = grep -d && !$dir{$_}, @{$self->{PMLIBDIRS}};
    my (@pmlibdirs) = @{$self->{PMLIBDIRS}};
    @{$self->{PMLIBDIRS}} = ();
    my %dir = map { ($_ => $_) } @{$self->{DIR}};
    foreach my $pmlibdir (@pmlibdirs) {
	-d $pmlibdir && !$dir{$pmlibdir} && push @{$self->{PMLIBDIRS}}, $pmlibdir;
    }

    unless( $self->{PMLIBPARENTDIRS} ) {
	@{$self->{PMLIBPARENTDIRS}} = ('lib');
    }

    return if $self->{PM} and $self->{ARGS}{PM};

    if (@{$self->{PMLIBDIRS}}){
	print "Searching PMLIBDIRS: @{$self->{PMLIBDIRS}}\n"
	    if ($Verbose >= 2);
	require File::Find;
        File::Find::find(sub {
            if (-d $_){
                unless ($self->libscan($_)){
                    $File::Find::prune = 1;
                }
                return;
            }
            return if /\#/;
            return if /~$/;             # emacs temp files
            return if /,v$/;            # RCS files
            return if m{\.swp$};        # vim swap files

	    my $path   = $File::Find::name;
            my $prefix = $self->{INST_LIBDIR};
            my $striplibpath;

	    my $parentlibs_re = join '|', @{$self->{PMLIBPARENTDIRS}};
	    $prefix =  $self->{INST_LIB}
                if ($striplibpath = $path) =~ s{^(\W*)($parentlibs_re)\W}
	                                       {$1}i;

	    my($inst) = $self->catfile($prefix,$striplibpath);
	    local($_) = $inst; # for backwards compatibility
	    $inst = $self->libscan($inst);
	    print "libscan($path) => '$inst'\n" if ($Verbose >= 2);
	    return unless $inst;
	    if ($self->{XSMULTI} and $inst =~ /\.xs\z/) {
		my($base); ($base = $path) =~ s/\.xs\z//;
		$self->{XS}{$path} = "$base.c";
		push @{$self->{C}}, "$base.c";
		push @{$self->{O_FILES}}, "$base$self->{OBJ_EXT}";
	    } else {
		$self->{PM}{$path} = $inst;
	    }
	}, @{$self->{PMLIBDIRS}});
    }
}


=item init_DIRFILESEP (o)

  $MM->init_DIRFILESEP;
  my $dirfilesep = $MM->{DIRFILESEP};

Initializes the DIRFILESEP macro which is the separator between the
directory and filename in a filepath.  ie. / on Unix, \ on Win32 and
nothing on VMS.

For example:

    # instead of $(INST_ARCHAUTODIR)/extralibs.ld
    $(INST_ARCHAUTODIR)$(DIRFILESEP)extralibs.ld

Something of a hack but it prevents a lot of code duplication between
MM_* variants.

Do not use this as a separator between directories.  Some operating
systems use different separators between subdirectories as between
directories and filenames (for example:  VOLUME:[dir1.dir2]file on VMS).

=cut

sub init_DIRFILESEP {
    my($self) = shift;

    $self->{DIRFILESEP} = '/';
}


=item init_main

Initializes AR, AR_STATIC_ARGS, BASEEXT, CONFIG, DISTNAME, DLBASE,
EXE_EXT, FULLEXT, FULLPERL, FULLPERLRUN, FULLPERLRUNINST, INST_*,
INSTALL*, INSTALLDIRS, LIB_EXT, LIBPERL_A, MAP_TARGET, NAME,
OBJ_EXT, PARENT_NAME, PERL, PERL_ARCHLIB, PERL_INC, PERL_LIB,
PERL_SRC, PERLRUN, PERLRUNINST, PREFIX, VERSION,
VERSION_SYM, XS_VERSION.

=cut

sub init_main {
    my($self) = @_;

    # --- Initialize Module Name and Paths

    # NAME    = Foo::Bar::Oracle
    # FULLEXT = Foo/Bar/Oracle
    # BASEEXT = Oracle
    # PARENT_NAME = Foo::Bar
### Only UNIX:
###    ($self->{FULLEXT} =
###     $self->{NAME}) =~ s!::!/!g ; #eg. BSD/Foo/Socket
    $self->{FULLEXT} = $self->catdir(split /::/, $self->{NAME});


    # Copied from DynaLoader:

    my(@modparts) = split(/::/,$self->{NAME});
    my($modfname) = $modparts[-1];

    # Some systems have restrictions on files names for DLL's etc.
    # mod2fname returns appropriate file base name (typically truncated)
    # It may also edit @modparts if required.
    # We require DynaLoader to make sure that mod2fname is loaded
    eval { require DynaLoader };
    if (defined &DynaLoader::mod2fname) {
        $modfname = &DynaLoader::mod2fname(\@modparts);
    }

    ($self->{PARENT_NAME}, $self->{BASEEXT}) = $self->{NAME} =~ m!(?:([\w:]+)::)?(\w+)\z! ;
    $self->{PARENT_NAME} ||= '';

    if (defined &DynaLoader::mod2fname) {
	# As of 5.001m, dl_os2 appends '_'
	$self->{DLBASE} = $modfname;
    } else {
	$self->{DLBASE} = '$(BASEEXT)';
    }


    # --- Initialize PERL_LIB, PERL_SRC

    # *Real* information: where did we get these two from? ...
    my $inc_config_dir = dirname($INC{'Config.pm'});
    my $inc_carp_dir   = dirname($INC{'Carp.pm'});

    unless ($self->{PERL_SRC}){
        foreach my $dir_count (1..8) { # 8 is the VMS limit for nesting
            my $dir = $self->catdir(($Updir) x $dir_count);

            if (-f $self->catfile($dir,"config_h.SH")   &&
                -f $self->catfile($dir,"perl.h")        &&
                -f $self->catfile($dir,"lib","strict.pm")
            ) {
                $self->{PERL_SRC}=$dir ;
                last;
            }
        }
    }

    warn "PERL_CORE is set but I can't find your PERL_SRC!\n" if
      $self->{PERL_CORE} and !$self->{PERL_SRC};

    if ($self->{PERL_SRC}){
	$self->{PERL_LIB}     ||= $self->catdir("$self->{PERL_SRC}","lib");

        $self->{PERL_ARCHLIB} = $self->{PERL_LIB};
        $self->{PERL_INC}     = ($Is{Win32}) ?
            $self->catdir($self->{PERL_LIB},"CORE") : $self->{PERL_SRC};

	# catch a situation that has occurred a few times in the past:
	unless (
		-s $self->catfile($self->{PERL_SRC},'cflags')
		or
		$Is{VMS}
		&&
		-s $self->catfile($self->{PERL_SRC},'vmsish.h')
		or
		$Is{Win32}
	       ){
	    warn qq{
You cannot build extensions below the perl source tree after executing
a 'make clean' in the perl source tree.

To rebuild extensions distributed with the perl source you should
simply Configure (to include those extensions) and then build perl as
normal. After installing perl the source tree can be deleted. It is
not needed for building extensions by running 'perl Makefile.PL'
usually without extra arguments.

It is recommended that you unpack and build additional extensions away
from the perl source tree.
};
	}
    } else {
	# we should also consider $ENV{PERL5LIB} here
        my $old = $self->{PERL_LIB} || $self->{PERL_ARCHLIB} || $self->{PERL_INC};
	$self->{PERL_LIB}     ||= $Config{privlibexp};
	$self->{PERL_ARCHLIB} ||= $Config{archlibexp};
	$self->{PERL_INC}     = $self->catdir("$self->{PERL_ARCHLIB}","CORE"); # wild guess for now
	my $perl_h;

	if (not -f ($perl_h = $self->catfile($self->{PERL_INC},"perl.h"))
	    and not $old){
	    # Maybe somebody tries to build an extension with an
	    # uninstalled Perl outside of Perl build tree
	    my $lib;
	    for my $dir (@INC) {
	      $lib = $dir, last if -e $self->catfile($dir, "Config.pm");
	    }
	    if ($lib) {
              # Win32 puts its header files in /perl/src/lib/CORE.
              # Unix leaves them in /perl/src.
	      my $inc = $Is{Win32} ? $self->catdir($lib, "CORE" )
                                  : dirname $lib;
	      if (-e $self->catfile($inc, "perl.h")) {
		$self->{PERL_LIB}	   = $lib;
		$self->{PERL_ARCHLIB}	   = $lib;
		$self->{PERL_INC}	   = $inc;
		$self->{UNINSTALLED_PERL}  = 1;
		print <<EOP;
... Detected uninstalled Perl.  Trying to continue.
EOP
	      }
	    }
	}
    }

    if ($Is{Android}) {
    	# Android fun times!
    	# ../../perl -I../../lib -MFile::Glob -e1 works
    	# ../../../perl -I../../../lib -MFile::Glob -e1 fails to find
    	# the .so for File::Glob.
    	# This always affects core perl, but may also affect an installed
    	# perl built with -Duserelocatableinc.
    	$self->{PERL_LIB} = File::Spec->rel2abs($self->{PERL_LIB});
    	$self->{PERL_ARCHLIB} = File::Spec->rel2abs($self->{PERL_ARCHLIB});
    }
    $self->{PERL_INCDEP} = $self->{PERL_INC};
    $self->{PERL_ARCHLIBDEP} = $self->{PERL_ARCHLIB};

    # We get SITELIBEXP and SITEARCHEXP directly via
    # Get_from_Config. When we are running standard modules, these
    # won't matter, we will set INSTALLDIRS to "perl". Otherwise we
    # set it to "site". I prefer that INSTALLDIRS be set from outside
    # MakeMaker.
    $self->{INSTALLDIRS} ||= "site";

    $self->{MAN1EXT} ||= $Config{man1ext};
    $self->{MAN3EXT} ||= $Config{man3ext};

    # Get some stuff out of %Config if we haven't yet done so
    print "CONFIG must be an array ref\n"
        if ($self->{CONFIG} and ref $self->{CONFIG} ne 'ARRAY');
    $self->{CONFIG} = [] unless (ref $self->{CONFIG});
    push(@{$self->{CONFIG}}, @ExtUtils::MakeMaker::Get_from_Config);
    push(@{$self->{CONFIG}}, 'shellflags') if $Config{shellflags};
    my(%once_only);
    foreach my $m (@{$self->{CONFIG}}){
        next if $once_only{$m};
        print "CONFIG key '$m' does not exist in Config.pm\n"
                unless exists $Config{$m};
        $self->{uc $m} ||= $Config{$m};
        $once_only{$m} = 1;
    }

# This is too dangerous:
#    if ($^O eq "next") {
#	$self->{AR} = "libtool";
#	$self->{AR_STATIC_ARGS} = "-o";
#    }
# But I leave it as a placeholder

    $self->{AR_STATIC_ARGS} ||= "cr";

    # These should never be needed
    $self->{OBJ_EXT} ||= '.o';
    $self->{LIB_EXT} ||= '.a';

    $self->{MAP_TARGET} ||= "perl";

    $self->{LIBPERL_A} ||= "libperl$self->{LIB_EXT}";

    # make a simple check if we find strict.pm in PERL_LIB
    warn "Warning: PERL_LIB ($self->{PERL_LIB}) seems not to be a perl library directory
        (strict.pm not found)"
        unless -f $self->catfile("$self->{PERL_LIB}","strict.pm") ||
               $self->{NAME} eq "ExtUtils::MakeMaker";
}

=item init_linker (o)

Most platforms have no need of special linker flags.

    $mm->init_linker;

Initialize macros which have to do with linking.

PERL_ARCHIVE: path to libperl.a equivalent to be linked to dynamic
extensions.

PERL_ARCHIVE_AFTER: path to a library which should be put on the
linker command line I<after> the external libraries to be linked to
dynamic extensions.  This may be needed if the linker is one-pass, and
Perl includes some overrides for C RTL functions, such as malloc().

EXPORT_LIST: name of a file that is passed to linker to define symbols
to be exported.

Some OSes do not need these in which case leave it blank.

=cut

sub init_linker {
    my($self) = shift;
    $self->{PERL_ARCHIVE} ||= '';
    $self->{PERL_ARCHIVEDEP} ||= '';
    $self->{PERL_ARCHIVE_AFTER} ||= '';
    $self->{EXPORT_LIST}  ||= '';
}


=begin _protected

=item init_lib2arch

    $mm->init_lib2arch

=end _protected

=cut

sub init_lib2arch {
    my($self) = shift;

    # The user who requests an installation directory explicitly
    # should not have to tell us an architecture installation directory
    # as well. We look if a directory exists that is named after the
    # architecture. If not we take it as a sign that it should be the
    # same as the requested installation directory. Otherwise we take
    # the found one.
    for my $libpair ({l=>"privlib",   a=>"archlib"},
                     {l=>"sitelib",   a=>"sitearch"},
                     {l=>"vendorlib", a=>"vendorarch"},
                    )
    {
        my $lib = "install$libpair->{l}";
        my $Lib = uc $lib;
        my $Arch = uc "install$libpair->{a}";
        if( $self->{$Lib} && ! $self->{$Arch} ){
            my($ilib) = $Config{$lib};

            $self->prefixify($Arch,$ilib,$self->{$Lib});

            unless (-d $self->{$Arch}) {
                print "Directory $self->{$Arch} not found\n"
                  if $Verbose;
                $self->{$Arch} = $self->{$Lib};
            }
            print "Defaulting $Arch to $self->{$Arch}\n" if $Verbose;
        }
    }
}


=item init_PERL (o)

    $mm->init_PERL;

Called by EUMM->new.  Sets up ABSPERL, PERL, FULLPERL and all the
*PERLRUN* permutations.

    PERL is allowed to be miniperl
    FULLPERL must be a complete perl

    ABSPERL is PERL converted to an absolute path

    *PERLRUN contains everything necessary to run perl, find it's
         libraries, etc...

    *PERLRUNINST is *PERLRUN + everything necessary to find the
         modules being built.

=cut

sub init_PERL {
    my($self) = shift;

    my @defpath = ();
    foreach my $component ($self->{PERL_SRC}, $self->path(),
                           $Config{binexp})
    {
	push @defpath, $component if defined $component;
    }

    # Build up a set of file names (not command names).
    my $thisperl = $self->canonpath($^X);
    $thisperl .= $Config{exe_ext} unless
                # VMS might have a file version # at the end
      $Is{VMS} ? $thisperl =~ m/$Config{exe_ext}(;\d+)?$/i
              : $thisperl =~ m/$Config{exe_ext}$/i;

    # We need a relative path to perl when in the core.
    $thisperl = $self->abs2rel($thisperl) if $self->{PERL_CORE};

    my @perls = ($thisperl);
    push @perls, map { "$_$Config{exe_ext}" }
                     ("perl$Config{version}", 'perl5', 'perl');

    # miniperl has priority over all but the canonical perl when in the
    # core.  Otherwise its a last resort.
    my $miniperl = "miniperl$Config{exe_ext}";
    if( $self->{PERL_CORE} ) {
        splice @perls, 1, 0, $miniperl;
    }
    else {
        push @perls, $miniperl;
    }

    $self->{PERL} ||=
        $self->find_perl(5.0, \@perls, \@defpath, $Verbose );

    my $perl = $self->{PERL};
    $perl =~ s/^"//;
    my $has_mcr = $perl =~ s/^MCR\s*//;
    my $perlflags = '';
    my $stripped_perl;
    while ($perl) {
	($stripped_perl = $perl) =~ s/"$//;
	last if -x $stripped_perl;
	last unless $perl =~ s/(\s+\S+)$//;
	$perlflags = $1.$perlflags;
    }
    $self->{PERL} = $stripped_perl;
    $self->{PERL} = 'MCR '.$self->{PERL} if $has_mcr || $Is{VMS};

    # When built for debugging, VMS doesn't create perl.exe but ndbgperl.exe.
    my $perl_name = 'perl';
    $perl_name = 'ndbgperl' if $Is{VMS} &&
      defined $Config{usevmsdebug} && $Config{usevmsdebug} eq 'define';

    # XXX This logic is flawed.  If "miniperl" is anywhere in the path
    # it will get confused.  It should be fixed to work only on the filename.
    # Define 'FULLPERL' to be a non-miniperl (used in test: target)
    unless ($self->{FULLPERL}) {
      ($self->{FULLPERL} = $self->{PERL}) =~ s/\Q$miniperl\E$/$perl_name$Config{exe_ext}/i;
      $self->{FULLPERL} = qq{"$self->{FULLPERL}"}.$perlflags;
    }
    # Can't have an image name with quotes, and findperl will have
    # already escaped spaces.
    $self->{FULLPERL} =~ tr/"//d if $Is{VMS};

    # Little hack to get around VMS's find_perl putting "MCR" in front
    # sometimes.
    $self->{ABSPERL} = $self->{PERL};
    $has_mcr = $self->{ABSPERL} =~ s/^MCR\s*//;
    if( $self->file_name_is_absolute($self->{ABSPERL}) ) {
        $self->{ABSPERL} = '$(PERL)';
    }
    else {
        $self->{ABSPERL} = $self->rel2abs($self->{ABSPERL});

        # Quote the perl command if it contains whitespace
        $self->{ABSPERL} = $self->quote_literal($self->{ABSPERL})
          if $self->{ABSPERL} =~ /\s/;

        $self->{ABSPERL} = 'MCR '.$self->{ABSPERL} if $has_mcr;
    }
    $self->{PERL} = qq{"$self->{PERL}"}.$perlflags;

    # Can't have an image name with quotes, and findperl will have
    # already escaped spaces.
    $self->{PERL} =~ tr/"//d if $Is{VMS};

    # Are we building the core?
    $self->{PERL_CORE} = $ENV{PERL_CORE} unless exists $self->{PERL_CORE};
    $self->{PERL_CORE} = 0               unless defined $self->{PERL_CORE};

    # Make sure perl can find itself before it's installed.
    my $lib_paths = $self->{UNINSTALLED_PERL} || $self->{PERL_CORE}
        ? ( $self->{PERL_ARCHLIB} && $self->{PERL_LIB} && $self->{PERL_ARCHLIB} ne $self->{PERL_LIB} ) ?
            q{ "-I$(PERL_LIB)" "-I$(PERL_ARCHLIB)"} : q{ "-I$(PERL_LIB)"}
        : undef;
    my $inst_lib_paths = $self->{INST_ARCHLIB} ne $self->{INST_LIB}
        ? 'RUN)'.$perlflags.' "-I$(INST_ARCHLIB)" "-I$(INST_LIB)"'
        : 'RUN)'.$perlflags.' "-I$(INST_LIB)"';
    # How do we run perl?
    foreach my $perl (qw(PERL FULLPERL ABSPERL)) {
        my $run  = $perl.'RUN';

        $self->{$run}  = qq{\$($perl)};
        $self->{$run} .= $lib_paths if $lib_paths;

        $self->{$perl.'RUNINST'} = '$('.$perl.$inst_lib_paths;
    }

    return 1;
}


=item init_PERM

  $mm->init_PERM

Called by EUMM->new.  Initializes PERL_*

=cut

sub init_PERM {
    my($self) = shift;

    $self->{PERM_DIR} = 755  unless defined $self->{PERM_DIR};
    $self->{PERM_RW}  = 644  unless defined $self->{PERM_RW};
    $self->{PERM_RWX} = 755  unless defined $self->{PERM_RWX};

    return 1;
}


=item init_xs (o)

    $mm->init_xs

Sets up macros having to do with XS code.  Currently just INST_STATIC,
INST_DYNAMIC and INST_BOOT.

=cut

sub init_xs {
    my $self = shift;

    if ($self->has_link_code()) {
        $self->{INST_STATIC}  =
          $self->catfile('$(INST_ARCHAUTODIR)', '$(BASEEXT)$(LIB_EXT)');
        $self->{INST_DYNAMIC} =
          $self->catfile('$(INST_ARCHAUTODIR)', '$(DLBASE).$(DLEXT)');
        $self->{INST_BOOT}    =
          $self->catfile('$(INST_ARCHAUTODIR)', '$(BASEEXT).bs');
	if ($self->{XSMULTI}) {
	    my @exts = $self->_xs_list_basenames;
	    my (@statics, @dynamics, @boots);
	    for my $ext (@exts) {
		my ($v, $d, $f) = File::Spec->splitpath($ext);
		my @d = File::Spec->splitdir($d);
		shift @d if defined $d[0] and $d[0] eq 'lib';
		my $instdir = $self->catdir('$(INST_ARCHLIB)', 'auto', @d, $f);
		my $instfile = $self->catfile($instdir, $f);
		push @statics, "$instfile\$(LIB_EXT)";

                # Dynamic library names may need special handling.
                my $dynfile = $instfile;
                eval { require DynaLoader };
                if (defined &DynaLoader::mod2fname) {
                    $dynfile = $self->catfile($instdir, &DynaLoader::mod2fname([@d, $f]));
                }

		push @dynamics, "$dynfile.\$(DLEXT)";
		push @boots, "$instfile.bs";
	    }
	    $self->{INST_STATIC} = join ' ', @statics;
	    $self->{INST_DYNAMIC} = join ' ', @dynamics;
	    $self->{INST_BOOT} = join ' ', @boots;
	}
    } else {
        $self->{INST_STATIC}  = '';
        $self->{INST_DYNAMIC} = '';
        $self->{INST_BOOT}    = '';
    }
}

=item init_VERSION (o)

    $mm->init_VERSION

Initialize macros representing versions of MakeMaker and other tools

MAKEMAKER: path to the MakeMaker module.

MM_VERSION: ExtUtils::MakeMaker Version

MM_REVISION: ExtUtils::MakeMaker version control revision (for backwards
             compat)

VERSION: version of your module

VERSION_MACRO: which macro represents the version (usually 'VERSION')

VERSION_SYM: like version but safe for use as an RCS revision number

DEFINE_VERSION: -D line to set the module version when compiling

XS_VERSION: version in your .xs file.  Defaults to $(VERSION)

XS_VERSION_MACRO: which macro represents the XS version.

XS_DEFINE_VERSION: -D line to set the xs version when compiling.

Called by EUMM->new.

=cut

sub init_VERSION {
    my($self) = shift;

    $self->{MAKEMAKER}  = $ExtUtils::MakeMaker::Filename;
    $self->{MM_VERSION} = $ExtUtils::MakeMaker::VERSION;
    $self->{MM_REVISION}= $ExtUtils::MakeMaker::Revision;
    $self->{VERSION_FROM} ||= '';

    if ($self->{VERSION_FROM}){
        $self->{VERSION} = $self->parse_version($self->{VERSION_FROM});
        if( $self->{VERSION} eq 'undef' ) {
            carp("WARNING: Setting VERSION via file ".
                 "'$self->{VERSION_FROM}' failed\n");
        }
    }

    if (defined $self->{VERSION}) {
        if ( $self->{VERSION} !~ /^\s*v?[\d_\.]+\s*$/ ) {
          require version;
          my $normal = eval { version->new( $self->{VERSION} ) };
          $self->{VERSION} = $normal if defined $normal;
        }
        $self->{VERSION} =~ s/^\s+//;
        $self->{VERSION} =~ s/\s+$//;
    }
    else {
        $self->{VERSION} = '';
    }


    $self->{VERSION_MACRO}  = 'VERSION';
    ($self->{VERSION_SYM} = $self->{VERSION}) =~ s/\W/_/g;
    $self->{DEFINE_VERSION} = '-D$(VERSION_MACRO)=\"$(VERSION)\"';


    # Graham Barr and Paul Marquess had some ideas how to ensure
    # version compatibility between the *.pm file and the
    # corresponding *.xs file. The bottom line was, that we need an
    # XS_VERSION macro that defaults to VERSION:
    $self->{XS_VERSION} ||= $self->{VERSION};

    $self->{XS_VERSION_MACRO}  = 'XS_VERSION';
    $self->{XS_DEFINE_VERSION} = '-D$(XS_VERSION_MACRO)=\"$(XS_VERSION)\"';

}


=item init_tools

    $MM->init_tools();

Initializes the simple macro definitions used by tools_other() and
places them in the $MM object.  These use conservative cross platform
versions and should be overridden with platform specific versions for
performance.

Defines at least these macros.

  Macro             Description

  NOOP              Do nothing
  NOECHO            Tell make not to display the command itself

  SHELL             Program used to run shell commands

  ECHO              Print text adding a newline on the end
  RM_F              Remove a file
  RM_RF             Remove a directory
  TOUCH             Update a file's timestamp
  TEST_F            Test for a file's existence
  TEST_S            Test the size of a file
  CP                Copy a file
  CP_NONEMPTY       Copy a file if it is not empty
  MV                Move a file
  CHMOD             Change permissions on a file
  FALSE             Exit with non-zero
  TRUE              Exit with zero

  UMASK_NULL        Nullify umask
  DEV_NULL          Suppress all command output

=cut

sub init_tools {
    my $self = shift;

    $self->{ECHO}     ||= $self->oneliner('binmode STDOUT, qq{:raw}; print qq{@ARGV}', ['-l']);
    $self->{ECHO_N}   ||= $self->oneliner('print qq{@ARGV}');

    $self->{TOUCH}    ||= $self->oneliner('touch', ["-MExtUtils::Command"]);
    $self->{CHMOD}    ||= $self->oneliner('chmod', ["-MExtUtils::Command"]);
    $self->{RM_F}     ||= $self->oneliner('rm_f',  ["-MExtUtils::Command"]);
    $self->{RM_RF}    ||= $self->oneliner('rm_rf', ["-MExtUtils::Command"]);
    $self->{TEST_F}   ||= $self->oneliner('test_f', ["-MExtUtils::Command"]);
    $self->{TEST_S}   ||= $self->oneliner('test_s', ["-MExtUtils::Command::MM"]);
    $self->{CP_NONEMPTY} ||= $self->oneliner('cp_nonempty', ["-MExtUtils::Command::MM"]);
    $self->{FALSE}    ||= $self->oneliner('exit 1');
    $self->{TRUE}     ||= $self->oneliner('exit 0');

    $self->{MKPATH}   ||= $self->oneliner('mkpath', ["-MExtUtils::Command"]);

    $self->{CP}       ||= $self->oneliner('cp', ["-MExtUtils::Command"]);
    $self->{MV}       ||= $self->oneliner('mv', ["-MExtUtils::Command"]);

    $self->{MOD_INSTALL} ||=
      $self->oneliner(<<'CODE', ['-MExtUtils::Install']);
install([ from_to => {@ARGV}, verbose => '$(VERBINST)', uninstall_shadows => '$(UNINST)', dir_mode => '$(PERM_DIR)' ]);
CODE
    $self->{DOC_INSTALL} ||= $self->oneliner('perllocal_install', ["-MExtUtils::Command::MM"]);
    $self->{UNINSTALL}   ||= $self->oneliner('uninstall', ["-MExtUtils::Command::MM"]);
    $self->{WARN_IF_OLD_PACKLIST} ||=
      $self->oneliner('warn_if_old_packlist', ["-MExtUtils::Command::MM"]);
    $self->{FIXIN}       ||= $self->oneliner('MY->fixin(shift)', ["-MExtUtils::MY"]);
    $self->{EQUALIZE_TIMESTAMP} ||= $self->oneliner('eqtime', ["-MExtUtils::Command"]);

    $self->{UNINST}     ||= 0;
    $self->{VERBINST}   ||= 0;

    $self->{SHELL}              ||= $Config{sh};

    # UMASK_NULL is not used by MakeMaker but some CPAN modules
    # make use of it.
    $self->{UMASK_NULL}         ||= "umask 0";

    # Not the greatest default, but its something.
    $self->{DEV_NULL}           ||= "> /dev/null 2>&1";

    $self->{NOOP}               ||= '$(TRUE)';
    $self->{NOECHO}             = '@' unless defined $self->{NOECHO};

    $self->{FIRST_MAKEFILE}     ||= $self->{MAKEFILE} || 'Makefile';
    $self->{MAKEFILE}           ||= $self->{FIRST_MAKEFILE};
    $self->{MAKEFILE_OLD}       ||= $self->{MAKEFILE}.'.old';
    $self->{MAKE_APERL_FILE}    ||= $self->{MAKEFILE}.'.aperl';

    # Not everybody uses -f to indicate "use this Makefile instead"
    $self->{USEMAKEFILE}        ||= '-f';

    # Some makes require a wrapper around macros passed in on the command
    # line.
    $self->{MACROSTART}         ||= '';
    $self->{MACROEND}           ||= '';

    return;
}


=item init_others

    $MM->init_others();

Initializes the macro definitions having to do with compiling and
linking used by tools_other() and places them in the $MM object.

If there is no description, its the same as the parameter to
WriteMakefile() documented in ExtUtils::MakeMaker.

=cut

sub init_others {
    my $self = shift;

    $self->{LD_RUN_PATH} = "";

    $self->{LIBS} = $self->_fix_libs($self->{LIBS});

    # Compute EXTRALIBS, BSLOADLIBS and LDLOADLIBS from $self->{LIBS}
    foreach my $libs ( @{$self->{LIBS}} ){
        $libs =~ s/^\s*(.*\S)\s*$/$1/; # remove leading and trailing whitespace
        my(@libs) = $self->extliblist($libs);
        if ($libs[0] or $libs[1] or $libs[2]){
            # LD_RUN_PATH now computed by ExtUtils::Liblist
            ($self->{EXTRALIBS},  $self->{BSLOADLIBS},
             $self->{LDLOADLIBS}, $self->{LD_RUN_PATH}) = @libs;
            last;
        }
    }

    if ( $self->{OBJECT} ) {
        $self->{OBJECT} = join(" ", @{$self->{OBJECT}}) if ref $self->{OBJECT};
        $self->{OBJECT} =~ s!\.o(bj)?\b!\$(OBJ_EXT)!g;
    } elsif ( ($self->{MAGICXS} || $self->{XSMULTI}) && @{$self->{O_FILES}||[]} ) {
        $self->{OBJECT} = join(" ", @{$self->{O_FILES}});
        $self->{OBJECT} =~ s!\.o(bj)?\b!\$(OBJ_EXT)!g;
    } else {
        # init_dirscan should have found out, if we have C files
        $self->{OBJECT} = "";
        $self->{OBJECT} = '$(BASEEXT)$(OBJ_EXT)' if @{$self->{C}||[]};
    }
    $self->{OBJECT} =~ s/\n+/ \\\n\t/g;

    $self->{BOOTDEP}  = (-f "$self->{BASEEXT}_BS") ? "$self->{BASEEXT}_BS" : "";
    $self->{PERLMAINCC} ||= '$(CC)';
    $self->{LDFROM} = '$(OBJECT)' unless $self->{LDFROM};

    # Sanity check: don't define LINKTYPE = dynamic if we're skipping
    # the 'dynamic' section of MM.  We don't have this problem with
    # 'static', since we either must use it (%Config says we can't
    # use dynamic loading) or the caller asked for it explicitly.
    if (!$self->{LINKTYPE}) {
       $self->{LINKTYPE} = $self->{SKIPHASH}{'dynamic'}
                        ? 'static'
                        : ($Config{usedl} ? 'dynamic' : 'static');
    }

    return;
}


# Lets look at $self->{LIBS} carefully: It may be an anon array, a string or
# undefined. In any case we turn it into an anon array
sub _fix_libs {
    my($self, $libs) = @_;

    return !defined $libs       ? ['']          :
           !ref $libs           ? [$libs]       :
           !defined $libs->[0]  ? ['']          :
                                  $libs         ;
}


=item tools_other

    my $make_frag = $MM->tools_other;

Returns a make fragment containing definitions for the macros init_others()
initializes.

=cut

sub tools_other {
    my($self) = shift;
    my @m;

    # We set PM_FILTER as late as possible so it can see all the earlier
    # on macro-order sensitive makes such as nmake.
    for my $tool (qw{ SHELL CHMOD CP MV NOOP NOECHO RM_F RM_RF TEST_F TOUCH
                      UMASK_NULL DEV_NULL MKPATH EQUALIZE_TIMESTAMP
                      FALSE TRUE
                      ECHO ECHO_N
                      UNINST VERBINST
                      MOD_INSTALL DOC_INSTALL UNINSTALL
                      WARN_IF_OLD_PACKLIST
                      MACROSTART MACROEND
                      USEMAKEFILE
                      PM_FILTER
                      FIXIN
                      CP_NONEMPTY
                    } )
    {
        next unless defined $self->{$tool};
        push @m, "$tool = $self->{$tool}\n";
    }

    return join "", @m;
}



=item init_platform

    $mm->init_platform

Initialize any macros which are for platform specific use only.

A typical one is the version number of your OS specific module.
(ie. MM_Unix_VERSION or MM_VMS_VERSION).

=cut

sub init_platform {
    return '';
}


=item init_MAKE

    $mm->init_MAKE

Initialize MAKE from either a MAKE environment variable or $Config{make}.

=cut

sub init_MAKE {
    my $self = shift;

    $self->{MAKE} ||= $ENV{MAKE} || $Config{make};
}

=item init_DEST

  $mm->init_DEST

Defines the DESTDIR and DEST* variables paralleling the INSTALL*.

=cut

sub init_DEST {
    my $self = shift;

    # Initialize DESTDIR
    $self->{DESTDIR} ||= '';

    # Make DEST variables.
    foreach my $var ($self->installvars) {
        my $destvar = 'DESTINSTALL'.$var;
        $self->{$destvar} ||= '$(DESTDIR)$(INSTALL'.$var.')';
    }
}


=item init_dist

  $mm->init_dist;

Defines a lot of macros for distribution support.

  macro         description                     default

  TAR           tar command to use              tar
  TARFLAGS      flags to pass to TAR            cvf

  ZIP           zip command to use              zip
  ZIPFLAGS      flags to pass to ZIP            -r

  COMPRESS      compression command to          gzip --best
                use for tarfiles
  SUFFIX        suffix to put on                .gz
                compressed files

  SHAR          shar command to use             shar

  PREOP         extra commands to run before
                making the archive
  POSTOP        extra commands to run after
                making the archive

  TO_UNIX       a command to convert linefeeds
                to Unix style in your archive

  CI            command to checkin your         ci -u
                sources to version control
  RCS_LABEL     command to label your sources   rcs -Nv$(VERSION_SYM): -q
                just after CI is run

  DIST_CP       $how argument to manicopy()     best
                when the distdir is created

  DIST_DEFAULT  default target to use to        tardist
                create a distribution

  DISTVNAME     name of the resulting archive   $(DISTNAME)-$(VERSION)
                (minus suffixes)

=cut

sub init_dist {
    my $self = shift;

    $self->{TAR}      ||= 'tar';
    $self->{TARFLAGS} ||= 'cvf';
    $self->{ZIP}      ||= 'zip';
    $self->{ZIPFLAGS} ||= '-r';
    $self->{COMPRESS} ||= 'gzip --best';
    $self->{SUFFIX}   ||= '.gz';
    $self->{SHAR}     ||= 'shar';
    $self->{PREOP}    ||= '$(NOECHO) $(NOOP)'; # eg update MANIFEST
    $self->{POSTOP}   ||= '$(NOECHO) $(NOOP)'; # eg remove the distdir
    $self->{TO_UNIX}  ||= '$(NOECHO) $(NOOP)';

    $self->{CI}       ||= 'ci -u';
    $self->{RCS_LABEL}||= 'rcs -Nv$(VERSION_SYM): -q';
    $self->{DIST_CP}  ||= 'best';
    $self->{DIST_DEFAULT} ||= 'tardist';

    ($self->{DISTNAME} = $self->{NAME}) =~ s{::}{-}g unless $self->{DISTNAME};
    $self->{DISTVNAME} ||= $self->{DISTNAME}.'-'.$self->{VERSION};
}

=back

=head2 Tools

A grab bag of methods to generate specific macros and commands.

=over

=item manifypods

Defines targets and routines to translate the pods into manpages and
put them into the INST_* directories.

=cut

sub manifypods {
    my $self          = shift;

    my $POD2MAN_macro = $self->POD2MAN_macro();
    my $manifypods_target = $self->manifypods_target();

    return <<END_OF_TARGET;

$POD2MAN_macro

$manifypods_target

END_OF_TARGET

}


=item POD2MAN_macro

  my $pod2man_macro = $self->POD2MAN_macro

Returns a definition for the POD2MAN macro.  This is a program
which emulates the pod2man utility.  You can add more switches to the
command by simply appending them on the macro.

Typical usage:

    $(POD2MAN) --section=3 --perm_rw=$(PERM_RW) podfile1 man_page1 ...

=cut

sub POD2MAN_macro {
    my $self = shift;

# Need the trailing '--' so perl stops gobbling arguments and - happens
# to be an alternative end of line separator on VMS so we quote it
    return <<'END_OF_DEF';
POD2MAN_EXE = $(PERLRUN) "-MExtUtils::Command::MM" -e pod2man "--"
POD2MAN = $(POD2MAN_EXE)
END_OF_DEF
}


=item test_via_harness

  my $command = $mm->test_via_harness($perl, $tests);

Returns a $command line which runs the given set of $tests with
Test::Harness and the given $perl.

Used on the t/*.t files.

=cut

sub test_via_harness {
    my($self, $perl, $tests) = @_;

    return qq{\t$perl "-MExtUtils::Command::MM" "-MTest::Harness" }.
           qq{"-e" "undef *Test::Harness::Switches; test_harness(\$(TEST_VERBOSE), '\$(INST_LIB)', '\$(INST_ARCHLIB)')" $tests\n};
}

=item test_via_script

  my $command = $mm->test_via_script($perl, $script);

Returns a $command line which just runs a single test without
Test::Harness.  No checks are done on the results, they're just
printed.

Used for test.pl, since they don't always follow Test::Harness
formatting.

=cut

sub test_via_script {
    my($self, $perl, $script) = @_;
    return qq{\t$perl "-I\$(INST_LIB)" "-I\$(INST_ARCHLIB)" $script\n};
}


=item tool_autosplit

Defines a simple perl call that runs autosplit. May be deprecated by
pm_to_blib soon.

=cut

sub tool_autosplit {
    my($self, %attribs) = @_;

    my $maxlen = $attribs{MAXLEN} ? '$$AutoSplit::Maxlen=$attribs{MAXLEN};'
                                  : '';

    my $asplit = $self->oneliner(sprintf <<'PERL_CODE', $maxlen);
use AutoSplit; %s autosplit($$ARGV[0], $$ARGV[1], 0, 1, 1)
PERL_CODE

    return sprintf <<'MAKE_FRAG', $asplit;
# Usage: $(AUTOSPLITFILE) FileToSplit AutoDirToSplitInto
AUTOSPLITFILE = %s

MAKE_FRAG

}


=item arch_check

    my $arch_ok = $mm->arch_check(
        $INC{"Config.pm"},
        File::Spec->catfile($Config{archlibexp}, "Config.pm")
    );

A sanity check that what Perl thinks the architecture is and what
Config thinks the architecture is are the same.  If they're not it
will return false and show a diagnostic message.

When building Perl it will always return true, as nothing is installed
yet.

The interface is a bit odd because this is the result of a
quick refactoring.  Don't rely on it.

=cut

sub arch_check {
    my $self = shift;
    my($pconfig, $cconfig) = @_;

    return 1 if $self->{PERL_SRC};

    my($pvol, $pthinks) = $self->splitpath($pconfig);
    my($cvol, $cthinks) = $self->splitpath($cconfig);

    $pthinks = $self->canonpath($pthinks);
    $cthinks = $self->canonpath($cthinks);

    my $ret = 1;
    if ($pthinks ne $cthinks) {
        print "Have $pthinks\n";
        print "Want $cthinks\n";

        $ret = 0;

        my $arch = (grep length, $self->splitdir($pthinks))[-1];

        print <<END unless $self->{UNINSTALLED_PERL};
Your perl and your Config.pm seem to have different ideas about the
architecture they are running on.
Perl thinks: [$arch]
Config says: [$Config{archname}]
This may or may not cause problems. Please check your installation of perl
if you have problems building this extension.
END
    }

    return $ret;
}

=back

=head2 File::Spec wrappers

ExtUtils::MM_Any is a subclass of File::Spec.  The methods noted here
override File::Spec.

=over

=item catfile

File::Spec <= 0.83 has a bug where the file part of catfile is not
canonicalized.  This override fixes that bug.

=cut

sub catfile {
    my $self = shift;
    return $self->canonpath($self->SUPER::catfile(@_));
}

=back    

=head2 Misc

Methods I can't really figure out where they should go yet.

=over

=item find_tests

  my $test = $mm->find_tests;

Returns a string suitable for feeding to the shell to return all
tests in t/*.t.

=cut

sub find_tests {
    my($self) = shift;
    return -d 't' ? 't/*.t' : '';
}

=item find_tests_recursive

  my $tests = $mm->find_tests_recursive;

Returns a string suitable for feeding to the shell to return all
tests in t/ but recursively. Equivalent to

  my $tests = $mm->find_tests_recursive_in('t');

=cut

sub find_tests_recursive {
    my $self = shift;
    return $self->find_tests_recursive_in('t');
}

=item find_tests_recursive_in

  my $tests = $mm->find_tests_recursive_in($dir);

Returns a string suitable for feeding to the shell to return all
tests in $dir recursively.

=cut

sub find_tests_recursive_in {
    my($self, $dir) = @_;
    return '' unless -d $dir;

    require File::Find;

    my $base_depth = grep { $_ ne '' } File::Spec->splitdir( (File::Spec->splitpath($dir))[1] );
    my %depths;

    my $wanted = sub {
        return unless m!\.t$!;
        my ($volume,$directories,$file) =
            File::Spec->splitpath( $File::Find::name  );
        my $depth = grep { $_ ne '' } File::Spec->splitdir( $directories );
        $depth -= $base_depth;
        $depths{ $depth } = 1;
    };

    File::Find::find( $wanted, $dir );

    return join ' ',
        map { $dir . '/*' x $_ . '.t' }
        sort { $a <=> $b }
        keys %depths;
}

=item extra_clean_files

    my @files_to_clean = $MM->extra_clean_files;

Returns a list of OS specific files to be removed in the clean target in
addition to the usual set.

=cut

# An empty method here tickled a perl 5.8.1 bug and would return its object.
sub extra_clean_files {
    return;
}

=item installvars

    my @installvars = $mm->installvars;

A list of all the INSTALL* variables without the INSTALL prefix.  Useful
for iteration or building related variable sets.

=cut

sub installvars {
    return qw(PRIVLIB SITELIB  VENDORLIB
              ARCHLIB SITEARCH VENDORARCH
              BIN     SITEBIN  VENDORBIN
              SCRIPT  SITESCRIPT  VENDORSCRIPT
              MAN1DIR SITEMAN1DIR VENDORMAN1DIR
              MAN3DIR SITEMAN3DIR VENDORMAN3DIR
             );
}


=item libscan

  my $wanted = $self->libscan($path);

Takes a path to a file or dir and returns an empty string if we don't
want to include this file in the library.  Otherwise it returns the
the $path unchanged.

Mainly used to exclude version control administrative directories from
installation.

=cut

sub libscan {
    my($self,$path) = @_;
    my($dirs,$file) = ($self->splitpath($path))[1,2];
    return '' if grep /^(?:RCS|CVS|SCCS|\.svn|_darcs)$/,
                     $self->splitdir($dirs), $file;

    return $path;
}


=item platform_constants

    my $make_frag = $mm->platform_constants

Returns a make fragment defining all the macros initialized in
init_platform() rather than put them in constants().

=cut

sub platform_constants {
    return '';
}

=item post_constants (o)

Returns an empty string per default. Dedicated to overrides from
within Makefile.PL after all constants have been defined.

=cut

sub post_constants {
    "";
}

=item post_initialize (o)

Returns an empty string per default. Used in Makefile.PLs to add some
chunk of text to the Makefile after the object is initialized.

=cut

sub post_initialize {
    "";
}

=item postamble (o)

Returns an empty string. Can be used in Makefile.PLs to write some
text to the Makefile at the end.

=cut

sub postamble {
    "";
}

=begin private

=item _PREREQ_PRINT

    $self->_PREREQ_PRINT;

Implements PREREQ_PRINT.

Refactored out of MakeMaker->new().

=end private

=cut

sub _PREREQ_PRINT {
    my ($self) = @_;

    require Data::Dumper;
    my @what = ('PREREQ_PM');
    push @what, 'MIN_PERL_VERSION' if $self->{MIN_PERL_VERSION};
    push @what, 'BUILD_REQUIRES'   if $self->{BUILD_REQUIRES};
    print Data::Dumper->Dump([@{$self}{@what}], \@what);
    exit 0;
}


=begin private

=item _PRINT_PREREQ

  $mm->_PRINT_PREREQ;

Implements PRINT_PREREQ, a slightly different version of PREREQ_PRINT
added by Redhat to, I think, support generating RPMs from Perl modules.

Should not include BUILD_REQUIRES as RPMs do not include them.

Refactored out of MakeMaker->new().

=end private

=cut

sub _PRINT_PREREQ {
    my ($self) = @_;

    my $prereqs= $self->{PREREQ_PM};
    my @prereq = map { [$_, $prereqs->{$_}] } keys %$prereqs;

    if ( $self->{MIN_PERL_VERSION} ) {
        push @prereq, ['perl' => $self->{MIN_PERL_VERSION}];
    }

    print join(" ", map { "perl($_->[0])>=$_->[1] " }
                 sort { $a->[0] cmp $b->[0] } @prereq), "\n";
    exit 0;
}


=begin private

=item _perl_header_files

  my $perl_header_files= $self->_perl_header_files;

returns a sorted list of header files as found in PERL_SRC or $archlibexp/CORE.

Used by perldepend() in MM_Unix and MM_VMS via _perl_header_files_fragment()

=end private

=cut

sub _perl_header_files {
    my ($self) = @_;

    my $header_dir = $self->{PERL_SRC} || $ENV{PERL_SRC} || $self->catdir($Config{archlibexp}, 'CORE');
    opendir my $dh, $header_dir
        or die "Failed to opendir '$header_dir' to find header files: $!";

    # we need to use a temporary here as the sort in scalar context would have undefined results.
    my @perl_headers= sort grep { /\.h\z/ } readdir($dh);

    closedir $dh;

    return @perl_headers;
}

=begin private

=item _perl_header_files_fragment ($o, $separator)

  my $perl_header_files_fragment= $self->_perl_header_files_fragment("/");

return a Makefile fragment which holds the list of perl header files which
XS code depends on $(PERL_INC), and sets up the dependency for the $(OBJECT) file.

The $separator argument defaults to "". MM_VMS will set it to "" and MM_UNIX to "/"
in perldepend(). This reason child subclasses need to control this is that in
VMS the $(PERL_INC) directory will already have delimiters in it, but in
UNIX $(PERL_INC) will need a slash between it an the filename. Hypothetically
win32 could use "\\" (but it doesn't need to).

=end private

=back

=cut

sub _perl_header_files_fragment {
    my ($self, $separator)= @_;
    $separator ||= "";
    return join("\\\n",
                "PERL_HDRS = ",
                map {
                    sprintf( "        \$(PERL_INCDEP)%s%s            ", $separator, $_ )
                } $self->_perl_header_files()
           ) . "\n\n"
           . "\$(OBJECT) : \$(PERL_HDRS)\n";
}


=head1 AUTHOR

Michael G Schwern <schwern@pobox.com> and the denizens of
makemaker@perl.org with code from ExtUtils::MM_Unix and
ExtUtils::MM_Win32.


=cut

1;

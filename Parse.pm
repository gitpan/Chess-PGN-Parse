############################################################
#
# Chess:PGN::Parse - a parser for PGN games
#
# Copyright (c) 2002 by Giuseppe Maxia
# Produced under the GPL (Golden Perl Laziness) 
# Distributed under the GPL (GNU General Public License) 
#
############################################################

# StringHandle 
# Utility package to read input from string, imitating
# a file handle.
package StringHandle;
use overload 
    '<>' => sub {
        return shift @{$_[0]};
    };
    
sub new {
    my $class = shift;
    return bless [split /^/m, $_[0]], $class;
}
sub close { }
 
package Chess::PGN::Parse;

use strict;
use warnings;
require 5.006;
use FileHandle;

require Exporter;

our @ISA = qw(Exporter);
our @EXPORT = qw(shrink_epd expand_epd STR NAG);
our @EXPORT_OK = qw();

our $VERSION = '0.06'; # 16-Feb-2002

=head1 NAME

Chess::PGN::Parse - reads and parses PGN (Portable Game Notation) Chess files

=head1 SYNOPSIS

	use Chess::PGN::Parse;
	my $pgnfile = "kk_2001.pgn";
	my $pgn = new Chess::PGN::Parse $pgnfile 
		or die "can't open $pgnfile\n";
	while ($pgn->read_game()) {
		print $pgn->white, ", " , $pgn->black, ", ", 
			$pgn->result, ", ",
			$pgn->game, "\n";
	}


	use Chess::PGN::Parse;
    my $text ="";
    {
        local $/ = undef;
        open PGN "< $pgnfile" or die;
        $text = <PGN>;
        close $text;
    }
    # reads from string instead of a file
	my $pgn = new Chess::PGN::Parse undef, $text; 
	while ($pgn->read_game()) {
		print $pgn->white, ", " , $pgn->black, ", ", 
			$pgn->result, ", ",
			$pgn->game, "\n";
	}

	use Chess::PGN::Parse;
	my $pgnfile = "kk_2001.pgn";
	my $pgn = new Chess::PGN::Parse $pgnfile 
		or die "can't open $pgnfile\n";
	my @games = $pgn->smart_read_all();


=head1 DESCRIPTION

Chess::PGN::Parse offers a range of methods to read and manipulate Portable Game Notation files.
PGN files contain chess games produced by chess programs following a standard format (http://www.schachprobleme.de/chessml/faq/pgn/). It is among the preferred means of chess games distribution. Being a public, well established standard, PGN is understood by many chess archive programs.
Parsing simple PGN files is not difficult. However, dealing with some of the intricacies of the Standard is less than trivial. This module offers a clean handle toward reading and parsing complex PGN files.

A PGN file has several B<tags>, which are key/values pairs at the header of each game, in the format 
	[key "value"]

After the header, the game follows. A string of numbered chess moves, optionally interrupted by braced comments and recursive parenthesized variants and comments. While dealing with simple braced comments is straightforward, parsing nested comments can give you more than a headache.

Chess::PGN::Parse most immediate methods are:
	read_game() reads one game, separating the tags and the game text.

	parse_game() parse the current game, and stores the moves into an 
		array and optionally saves the comments into an array of hashes
		for furter usage. It can deal with nested comments and recursive
		variations.

	quick_parse_game() Same as the above, but doesn't save the comments, 
		which are just stripped	from the text. It can't deal with nested
		comments. Should be the preferred method when we know that we are
		dealing with simple PGNs.

	smart_parse_game() Best of the above methods. A  preliminary check
		will call parse_game() or quick_parse_game(), depending on the
		presence of nested comments in the game.

	read_all(), quick_read_all(), smart_read_all() will read all the records
		in the current PGN file and return an array of hashes with all the
		parsed details from the games.

=head2 EXPORT

new, STR, read_game, tags, event, site, white, black, round, date, result, game , NAG, moves

=head2 DEPENDENCIES

FileHandle

=head1 Class methods

=over 4

=item new()

Create a new Chess::PGN::Parse object (requires file name)
	my $pgn = Chess::PGN::Parse->new "filename.pgn" 
		or die "no such file \n";

=cut

my @SevenTagsRoster = qw(Event Site Date Round White Black Result);

sub new {
	my $class = shift;
	my $filename = shift;
    my $fh = undef;
    if (defined $filename) {
	    $fh = new FileHandle "< $filename" 
            || return undef;
    }
    else {
        my $text = shift;
        $fh = new StringHandle $text;
    }
	my $self = 	bless  {
		GameMoves =>[],		# game moves
		GameComments =>{},	# comments with reference to the move
		gamedescr => {},	# will contain the PGN tags
		GameErrors => {},	# will contain the parsing errors
		fh	=> \$fh			# filehandle to the PGN file
	}, $class;
	return $self;
}

=for internal use
	the object destroyer cleans possible hanging references

=cut

sub DESTROY {
	my $self = shift;
	undef $self->{GameComments};
	undef $self->{GameErrors};
	undef $self->{gamedescr};
	undef $self->{GameMoves};
	${$self->{fh}}->close();
	undef $self->{fh};
}
my %SymbolicAnnotationGlyph = (
'$1' => '!',
'$2' => '?',
'$3' => '!!',
'$4' => '??',
'$5' => '!?',
'$6' => '?!'
);

my %NumericAnnotationGlyph = ();

=item NAG()
returns the corresponding Numeric Annotation Glyph

=cut

sub NAG {
	my $item = shift;
	return undef unless $item =~ /\$?(\d+)/;
	return undef if ($1 > 139) or ($1 < 0);
	unless (scalar keys %NumericAnnotationGlyph) {
		local $/ = undef;
		eval <DATA>;
	}
	my $nag_ref = \%NumericAnnotationGlyph;
	if (($1 > 0) and ($1 <=6)) {
		$nag_ref = \%SymbolicAnnotationGlyph
	}
	if ($item =~ /^\$/) {
		return $nag_ref->{$item}
	}
	else {
		return $nag_ref->{'$'.$item}
	}
}

=item STR()

returns the Seven Tags Roster array

	@array = $pgn->STR();
	@array = PGNParser::STR();

=cut

sub STR {
	return @SevenTagsRoster;
}

=item event()

returns the Event tag

=item site()

returns the Site tag

=item date()

returns the Date tag

=item white()

returns the White tag

=item black()

returns the Black tag

=item result()

returns the result tag

=item round()

returns the Round tag

=item game()

returns the unparsed game moves

=item time()

returns the Time tag

=item eco()

returns the ECO tag

=item eventdate()

returns the EventDate tag

=item moves()

returns an array reference to the game moves (no numbers)

=cut

=item comments()

returns a hash reference to the game comments (the key is the move number and the value are the comments for such move)

=cut

=item errors()

returns a hash reference to the game errors (the key is the move number and the value are the errors for such move)

=cut

sub event {
	my $self = shift;
	return $self->{gamedescr}{Event}
}

sub site {
	my $self = shift;
	return $self->{gamedescr}{Site}
}

sub date {
	my $self = shift;
	return $self->{gamedescr}{Date}
}

sub white {
	my $self = shift;
	return $self->{gamedescr}{White}
}

sub black {
	my $self = shift;
	return $self->{gamedescr}{Black}
}

sub result {
	my $self = shift;
	return $self->{gamedescr}{Result}
}

sub round {
	my $self = shift;
	return $self->{gamedescr}{Round}
}

sub time {
	my $self = shift;
	return $self->{gamedescr}{Time}
}

sub eventdate {
	my $self = shift;
	return $self->{gamedescr}{EventDate}
}

sub eco {
	my $self = shift;
	return $self->{gamedescr}{ECO}
}

sub game {
	my $self = shift;
	return $self->{gamedescr}{Game}
}

sub moves {
	my $self = shift;
	return $self->{GameMoves};
}

sub errors {
	my $self = shift;
	return $self->{GameErrors};
}

sub comments {
	my $self = shift;
	return $self->{GameComments};
}

=for internal use
initialize the pgn object fields.

=cut

sub _init {
	my $self = shift;
	for (keys %{$self->{gamedescr}}) {
		$self->{gamedescr}{$_} = ""
	}
	delete $self->{gamedescr}{FirstMove} 
		if exists $self->{gamedescr}{FirstMove};
	undef $self->{GameMoves};
	undef $self->{GameComments};
}

=item tags()
	
returns a hash reference to all the parsed tags

	$hash_ref = $pgn->tags();

=cut

sub tags {
	my $self = shift;
	return \%{$self->{gamedescr}};
}

=item read_all()

Will read and parse all the games in the current file and return a reference to an array of hashes.
Each hash item contains both the raw data and the parsed moves and comments

Same parameters as for parse_game(). Default : discard comments

	my $games_ref = $pgn->read_all();

=cut

sub read_all {
	my $self=shift;
	my $params = shift;
	my @games =(); 
	while ($self->read_game()) {
		$self->parse_game($params);
		my %gd = %{$self->{gamedescr}};
		$gd{GameComments} = $self->{GameComments};
		$gd{GameErrors} = $self->{GameErrors};
		$gd{GameMoves} = $self->{GameMoves};
		push @games, \%gd;
	}
	return \@games;
}

=item quick_read_all()

Will read and quick parse all the games in the current file and return a reference to an array of hashes.
Each hash item contains both the raw data and the parsed moves
Comments are discarded. Same parameters as for quick_parse_game().

	my $games_ref = $pgn->quick_read_all();

=cut

sub quick_read_all {
	my $self=shift;
	my $params = shift;
	my @games =(); 
	while ($self->read_game()) {
		$self->quick_parse_game($params);
		my %gd = %{$self->{gamedescr}};
		$gd{GameMoves} = $self->{GameMoves};
		push @games, \%gd;
	}
	return \@games;
}

=item smart_read_all()

Will read and quick parse all the games in the current file and return a reference to an array of hashes.
Each hash item contains both the raw data and the parsed moves
Comments are discarded. Calls smart_read_game() to decide which method is best to parse each given game.

	my $games_ref = $pgn->smart_read_all();

=cut

sub smart_read_all {
	my $self=shift;
	my $params = shift;
	my @games =(); 
	while ($self->read_game()) {
		$self->smart_parse_game($params);
		my %gd = %{$self->{gamedescr}};
		$gd{GameMoves} = $self->{GameMoves};
		push @games, \%gd;
	}
	return \@games;
}

=item read_game()

reads the next game from the given PGN file.
Returns TRUE (1) if successful (= a game was read)
or FALSE (0) if no more games are available or
an unexpected EOF occurred before the end of parsing
	
	while ($pgn->read_game()) {
		do_something_smart;
	}

=cut

sub read_game {
	my $self = shift;
	my $fh = ${$self->{fh}};
	$self->_init();
	my $block = 1;
    #return 0 if eof($fh); # changed in version 0.06
	while (<$fh>) {
        return 0 unless defined $_; # 0.06
		chomp;
		$block = 0 if /^\s*$/;   
		last unless $block;
		last unless /\[(\w+)/;
		my $tag = $1;
		last unless /\"(.*)\"/; 
		my $value = $1;
		$self->{gamedescr}{$tag} = $value; 
	}
	$block = 1;
    #return 0 if eof($fh); # changed in version 0.06
    return 0 unless defined $_; # 0.06
	while (<$fh>) {    
        return 0 unless defined $_; # 0.06
		$block = 0 if /^\s*$/; 
		last unless $block; 
		$self->{gamedescr}{Game} .= $_;
	}
	return 1;
}

=for internal use

 _get_tags() returns a list of tags depending on the parameters

 _get_format() returns a format to be used when printing tags

 _get_formatted_tag() returns a tag formatted according to the
 given template.

=cut

sub _get_tags {
    my $self = shift;
    my $params = shift;
    my @newtags=();
    my %seen = (Game =>1);
    if (exists $params->{all_tags} 
        and ($params->{all_tags} =~ /^(:?[Yy][Ee][Ss]|1)$/)) 
    {
        for (@SevenTagsRoster) {
            push @newtags, $_;
            $seen{$_}++;
        }
        for (sort {lc $a cmp lc $b} keys %{$self->{gamedescr}}) {
            push @newtags, $_ unless $seen{$_};
        }
    }
    elsif (exists $params->{tags}) {
        for (@{$params->{tags}}) {
            push @newtags, $_;
        }
    }
    else {
        @newtags = @SevenTagsRoster;
    }
    return @newtags;
}


sub _get_left_right {
    my $pattern = shift;
    my $format = shift;
    my $left = shift;
    my $right = shift;
    if (defined $pattern) {
        if (length($pattern) == 1) {
             $format = $pattern . $format .$pattern;
        }
        elsif (length($pattern) == 2) {
            my @chars = split //, $pattern;
            $left = $chars[0];
            $right= $chars[1];
        }
        elsif ($pattern =~ /^(.*)\|(.*)$/) { 
            $left = $1;
            $right = $2;
        }
    }
    $format = $left . $format . $right; 
    return $format;
}

sub _get_format {
    my $params = shift;
    my $format = _get_left_right($params->{quotes}, "#value#",'"','"');
    $format = _get_left_right($params->{brackets},'#tag# '.$format,'[',']');
    return $format;
}

sub _formatted_tag {
    my ($format, $tag, $value) = @_;
    my $subst = $format;
    $subst =~ s/#tag#/$tag/;
    $subst =~ s/#value#/$value/;
    return $subst;
}

=item standard_PGN()

 returns a string containing all current PGN tags, including
 the game.
 Parameters are passed through a hash reference. None is
 required.

 tags => [tag list], # default is the Seven Tags Roster.
                     # You may specify only the tags you want to 
                     # print 
                     # tags => [qw(White Black Result)]
 
 all_tags => 'no',   # default 'no'. If yes (or 1), it outputs all the tags
                     # if 'tags' and 'all_tags' are used, 'all_tags' 
                     # prevails

 nl => '\n',         # default '\n'. Tag separator. Can be changed
                     # according to your needs.
                     # nl => '<br>\n' is a good candidate for HTML 
                     # output.
 
 brackets => '[]',   # default '[]'. Output tags within brackets.
                     # Bracketing can be as creative as you want.
                     # If the left and rigth bracketing sequence are
                     # longer than one character, they must be separated
                     # by a pipe (|) symbol.
                     # '()', '(|)\t,'{|}\n' and '{}' are valid 
                     # sequences.
                     # 
                     # '<h1>|</h1>' will output HTML header 1
                     # '<b>{</b>|<b>}</b>\n' will enclose each tag
                     # between bold braces.
 
 quotes => '"',      # default '"'. Quote tags values.
                     # As for brackets, quotes can be specified in
                     # pairs: '<>' and '<|>' are equivalent.
                     # If the quoting sequence is more than one char,
                     # the pipe symbol is needed to separate the left
                     # quote from the right one.
                     # '<i>|</i>' will produce HTML italicized text.
                     
 game => 'yes',      # default 'yes'. Output the game text 
                     # If the game was parsed, returns a clean list
                     # of moves, else the unparsed text

=cut

my %switchcolor = ('w' => 'b', 'b' => 'w');
sub standard_PGN {
    my $self = shift;
    my $params = shift;
    my %seen =(Game =>1);
    my @tags = $self->_get_tags($params);
    my $out ="";
    my $nl ="\n";
    $nl = $params->{nl} if exists $params->{nl};
    my $format = _get_format($params);
    for (@tags) {
        $self->{gamedescr}{$_}="?" unless exists $self->{gamedescr}{$_};
        #$out .= qq/[$_ "$self->{gamedescr}{$_}"]\n/;
        $out .= _formatted_tag($format, $_, $self->{gamedescr}{$_});
        $out .= $nl;
    }
    return $out 
        if (exists $params->{game} 
            and (lc $params->{game} !~ /$(:?[Yy][Ee][Ss]|1)$/)); 
    if (defined $self->{GameMoves}) { # if parsed
        my $count = 1;
        my $color = 'w';
        my $len = 0;
        for (@{$self->moves}) { # 
            if ($color eq 'w') {
                $out .= " " and $len++ if $len and ($count > 1);
                $out .= "$count.";
                $len += length($count) +1;
                $count++;
            }
            else {
                $out .= ' ';
                $len++;
            }
            $out .= "$_";
            $len += length($_);
            $color = $switchcolor{$color};
            if ($len >= 75) {
                $len =0;
                $out .= $nl;
            }
        }
        $out .=" $self->{gamedescr}{Result}$nl";
    }
    else { # not parsed - returns game text
        $out .= $self->{gamedescr}{Game};
    }
    return $out;
}

=item smart_parse_game()

Parses the current game, returning the moves only. 
Uses by default quick_parse_game(), unless recursive comments are found in the source game.

=cut

sub smart_parse_game {
	my $self = shift;
	my $params = shift;
	if ($self->{gamedescr}{Game} =~ /\(/) {
		$self->parse_game($params)
	}
	else {
		$self->quick_parse_game($params)
	}
}

=item quick_parse_game()

Parses the current game, returning the moves only.
Comments are discarded.
This function does FAIL on Recursive Annotated Variation or nested comments.
Parameters  (passed as a hash reference): check_moves = 'yes'|'no'. Default : no. If requested, each move is checked against a RegEx, to filter off possible unbraced comments.

=cut

# ==============================================
# These two regular expressions were produced by 
# Damian Conway's module Regexp::Common
# ----------------------------------------------
# On the author's suggestion, these lines 
# 
# use Regexp::Common;
# print "$RE{balanced}{-parens=>'()'}\n";
# print "$RE{balanced}{-parens=>'{}'}\n";
#
# produced the RegEx code, which was edited
# and inserted here for efficiency reasons.
# ==============================================

our $RE_parens = qr/
	(?:(?:(?:[(](?:(?>[^)(]+)
	|(??{$RE_parens}))*[)]))
	|(?:(?!)))
	/x;

our $RE_brace = qr/
	(?:(?:(?:[{](?:(?>[^}{]+)
	|(??{$RE_brace}))*[}]))
	|(?:(?!)))
	/x;

# ==============================================

# regular expressions for game parsing
my $REresult    = qr{(:?1\-0|0\-1|1\/2\-1\/2|\*)};
my $REmove      = qr{[KQRBN]?[a-h]?[1-8]?x?[a-h][1-8](:?\=[QRBN])?};
#  piece              ^^^^^ 
#  unambiguous column or line ^^^   ^^^   
#  capture                               ^ 
#  destination square                       ^^^  ^^^
#  promotion                                             ^ ^^^^^
my $REcastling  = qr/O\-O(:?\-O)?/;
my $REcheck     = qr/(:?(:?\#|\+(\+)?))?/;
my $REanymove   = qr/(:?$REmove|$REcastling)$REcheck/;
my $RENAG       = qr/\$\d+/;
my $REnumber    = qr/\d+\.(:?\.\.)?/;
my $REescape    = qr/^\%[^\n]*\n/;
my $REeolcomment= qr/;.*$/;
my $RERAV       = $RE_parens;
my $REcomment   = $RE_brace;

sub quick_parse_game {
	my $self = shift;
	my $params = shift; # hash reference to parameters
	$self->{gamedescr}{Game} =~ s/$REeolcomment//mg; # rm EOL comments
	$self->{gamedescr}{Game} =~ s/$REescape//mgo; # rm escaped lines
	$self->{gamedescr}{Game} =~ 
		s/$REcomment//g;  # remove comments
	$self->{gamedescr}{Game} =~ 
		s/$RERAV//g;  	 # remove RAV
	return 0 
		if $self->{gamedescr}{Game} =~ 
			/\(/; # the game still contains RAV
	return 0 
		if $self->{gamedescr}{Game} =~ 
			/\{/; # undetected nested comments
	$self->{gamedescr}{Game} =~ s/\n/ /g;		  # remove newlines
	$self->{gamedescr}{Game} =~ 
		s/\r/ /g;		  # remove return chars (DOS)
	$self->{gamedescr}{Game} =~ s/$RENAG//go;	  # remove NAG
    $self->{gamedescr}{Game} =~ s/\d+\.//g;       # remove numbers
    $self->{gamedescr}{Game} =~ s/\.\.(:?\.)?//g; # remove "..."
	$self->{gamedescr}{Game} =~ s/$REresult\s*\Z//o;
	my $REfilter = qr/\S/;
	if (exists $params->{check_moves} 
		and ($params->{check_moves} =~ /^(:?yes|1)$/)) 
	{
		$REfilter = $REanymove;
	}
	return undef unless $self->{gamedescr}{Game}; # discards empty games
	$self->{GameMoves} = 
		[grep { m/$REfilter/o } split /\s+/, $self->{gamedescr}{Game}];
}

=item parse_game()

Parses the current game (after read_game() was called).
Accepts parameters as hash reference.

	$pgn->parse_game(); # default save_comments => 'no'

	$pgn->parse_game({save_comments => 'yes'});
	$pgn->parse_game({save_comments => 'yes', log_errors = 'yes'});

parse_game() implements a finite state machine on two assumptions:

	1. No moves or move numbers are truncated at the end of a line;
	2. the possible states in a PGN game are:

		a. move number
		b. move
		c. braced comment
		d. EOL comment
		e. Numeric Annotation Glyph
		f. Recursive Annotated Variation
		g. Result
		h. unbraced comments (barewords, "!?+-=")

	items from "a" to "g" are actively parsed and recognized. 
	Anything unrecognized goes into the "h" state and discarded
	(or stored, if log_errors was requested)

=cut


sub parse_game {
	my $self = shift;
	my $params = shift;
	my $save_comments = (exists $params->{save_comments})
		and ($params->{save_comments} =~ /^(:?yes|1)$/);
	my $log_errors = (exists $params->{log_errors}) 
		and ($params->{log_errors} =~ /^(:?yes|1)$/);
	return undef unless $self->{gamedescr}{Game};
	my $movecount = 0;
	my $color = 'b';
	$self->{gamedescr}{Game} =~ s/0\-0\-0/O-O-O/g;
	$self->{gamedescr}{Game} =~ s/0\-0/O-O/g;
	$self->{gamedescr}{Game} =~ s/$REresult\s*\Z//o;

	PARSER:
	{
		$self->{gamedescr}{Game} =~ m/\G($REnumber)\s*/mgc && do {
			my $num=$1;
			if (( $num =~ tr/\.//d) > 1) {
				$color = 'w';
			}
			if ($movecount == 0) {
				$movecount = $num;
				$self->{gamedecr}{FirstMove} = $num.$switchcolor{$color}
					unless $num.$switchcolor{$color} eq '1w';
			}
			elsif ($movecount == ($num -1)) {
				$movecount++;
			}
			elsif ($movecount != $num) {
				$self->{GameErrors}->{$movecount.$color} 
					.= " invalid move sequence ($num <=> $movecount)";
				$movecount++;
			}
			redo
		};
		$self->{gamedescr}{Game} =~ m/\G($REanymove)\s*/mgc && do { 
			push @{$self->{GameMoves}}, $1; 
			$color = $switchcolor{$color};
			redo
		};
		$self->{gamedescr}{Game} =~ 
		m/\G($REcomment
			|$REeolcomment
			|$RERAV
			|$RENAG|$REescape)\s*/mgcx 
		&& do 
		{
			if ($save_comments) { 
				$self->{GameComments}->{$movecount.$color} .= " " . $1;
				$self->{GameComments}->{$movecount.$color} =~ tr/\r//d;
				$self->{GameComments}->{$movecount.$color} =~ s/\n/ /g;
			}
			redo
		};
		$self->{gamedescr}{Game} =~ m/\G(\S+\s*)/mgc && do {
			if ($log_errors) {
				$self->{GameErrors}->{$movecount.$color} .= " " . $1;
				$self->{GameErrors}->{$movecount.$color} =~ tr/\r//d;
				$self->{GameErrors}->{$movecount.$color} =~ s/\n/ /g;
			}	
			redo
		};
	}
	return 1;
}

=item shrink_epd()

Given a EPD (Extended Position Description) string, shrink_epd() will convert it into a bit string, which reduces the original by about 50%.
It can be restored to the original string by expand_epd()

=cut

# K k   0001 1001 001
# Q q   0010 1010 010
# R r   0011 1011 011
# B b   0100 1100 100
# N n   0101 1101 101
# P p   0110 1110 110
# E     0000 0000 000
#                 111
# rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq d3 (38 bytes)
# 1011 1101 1100 1010 1001 1100 1101 1011	   4
# 1110 1110 1110 1110 1110 1110 1110 1110      4
# 11111000                                     1
# 11111000                                     1
# 11110011 0110 11110100                       2.5
# 11111000                                     1
# 0110 0110 0110 11110001 0110 0110 0110 0110  4.5
# 0011 0101 0100 0010 0001 0100 0101 0011      4
#                                             22
my %pieces2bits = (
	K =>  1,	# 0001
	k =>  9,	# 1001
	Q =>  2,	# 0010
	q => 10,	# 1010
	R =>  3,	# 0011
	r => 11,	# 1011
	B =>  4,	# 0100
	b => 12,	# 1100
	N =>  5,	# 0101
	n => 13,	# 1101
	P =>  6,	# 0110
	p => 14,	# 1110
	1 =>  0,	# 0000
	2 =>  7,	# 0111
	3 =>  8,    # 1000
	4 => 0xF4,  # 1111 0100
	5 => 0xF5,  # 1111 0101
	6 => 0xF6,  # 1111 0110
	7 => 0xF7,  # 1111 0111
	8 => 0xF8   # 1111 1000
);

my %castling2bits = (
	'KQkq' => 15, # 1111   F  KQkq
	'KQk'  => 14, # 1110   E  KQk-
	'KQq'  => 13, # 1101   D  KQ-q
	'KQ'   => 12, # 1100   C  KQ--
	'Kkq'  => 11, # 1011   B  K-kq
	'Kk'   => 10, # 1010   A  K-k-
	'Kq'   =>  9, # 1001   9  K--q
	'K'    =>  8, # 1000   8  K---
	'Qkq'  =>  7, # 0111   7  -Qkq
	'Qk'   =>  6, # 0110   6  -Qk-
	'Qq'   =>  5, # 0101   5  -Q-q
	'Q'    =>  4, # 0100   4  -Q--
	'kq'   =>  3, # 0011   3  --kq
	'k'    =>  2, # 0010   2  --k-
	'q'    =>  1, # 0001   1  ---q
	'-'    =>  0  # 0111   0  ----
);

my %ep2bits = (
	'-' => 0,
	'a' => 1,
	'b' => 2,
	'c' => 3,
	'd' => 4,
	'e' => 5,
	'f' => 6,
	'g' => 7,
	'h' => 8
);
my %color2bits = ('w' =>  0, 'b' =>  1 );
my %bits2color = ( 0  => 'w', 1  => 'b');

my %bits2pieces   = map { $pieces2bits{$_}, $_ } keys %pieces2bits;
my %bits2castling = map { $castling2bits{$_}, $_ } keys %castling2bits;
my %bits2ep       = map { $ep2bits{$_}, $_ } keys %ep2bits;

sub shrink_epd {
	my $source = shift;
	my $piece ="";
	my $vecstring = "";
	my $offset =0;
	my ($fen, $color, $castling, $ep) = split / /, $source;
	while ($fen =~ /(.)/g) {
		next if $1 eq '/';
		$piece =  $pieces2bits{$1};
		if ($piece < 0x0F) {
			vec($vecstring, $offset++, 4) = $piece;
		}
		else {
			vec($vecstring, $offset++, 4) = 0x0F;
			vec($vecstring, $offset++, 4) = $1;
		}
	}
	vec($vecstring, $offset++, 4) = $color2bits{$color}; 
	vec($vecstring, $offset++, 4) = $castling2bits{$castling};
	vec($vecstring, $offset++, 4) = $ep2bits{substr($ep,0,1)};
	return $vecstring;
}

=item expand_epd()

given a EPD bitstring created by shrink_epd(), expand_epd() will restore the original text.

=cut

sub expand_epd {
	my $vecstring = shift;
	my $piece = -1;
	my $asciistr="";
	my $offset =0;
	my $rowsum =0;
	my $overall_sum =0;
	while ($offset < length($vecstring)*2) {
		$piece = vec($vecstring, $offset++, 4);
		if ($piece == 0x0F) {
			$piece = hex("F" . vec($vecstring,$offset++,4));
		}
		$piece = $bits2pieces{$piece};
		$asciistr .= $piece;
		if ($piece =~ /[1-8]/) {
			$rowsum += $piece
		}
		else {
			$rowsum++;
		}
		if ($rowsum == 8) {
			$overall_sum += $rowsum;
			$rowsum =0;
			last if ($overall_sum >= 64);
			$asciistr .='/';
		}
	}
	my $color = $bits2color{vec($vecstring,$offset++,4)};
	$asciistr .= ' '. $color;
	$asciistr .= ' '. $bits2castling{vec($vecstring,$offset++,4)};
	my $ep = $bits2ep{vec($vecstring,$offset++,4)}; 
	if ($ep ne '-') {
		$ep .= $color eq 'w' ? '6' : '3';
	}
	$asciistr .= ' ' . $ep;
	return $asciistr;
}

=back

=head1 AUTHOR

Giuseppe Maxia, gmax@karma.oltrelinux.com

=head1 THANKS

Thanks to 
- Hugh S. Myers for advice, support, testing and brainstorming;
- Damian Conway for the recursive Regular Expressions used to parse comments;
- all people at PerlMonks (www.perlmonks.org) for advice and good developing environment.

=head1 COPYRIGHT

The Chess::PGN::Parse module is Copyright (c) 2002 Giuseppe Maxia,
Sardinia, Italy. All rights reserved.
 
You may distribute this software under the terms of either the GNU
General Public License version 2 or the Artistic License, as
specified in the Perl README file.
The embedded and encosed documentation is released under 
the GNU FDL Free Documentation License 1.1

=cut

1;
__DATA__
%NumericAnnotationGlyph = (
'$0' => 'null annotation',
'$1' => 'good move (traditional "!")',
'$2' => 'poor move (traditional "?")',
'$3' => 'very good move (traditional "!!")',
'$4' => 'very poor move (traditional "??")',
'$5' => 'speculative move (traditional "!?")',
'$6' => 'questionable move (traditional "?!")',
'$7' => 'forced move (all others lose quickly)',
'$8' => 'singular move (no reasonable alternatives)',
'$9' => 'worst move',
'$10' => 'drawish position',
'$11' => 'equal chances, quiet position',
'$12' => 'equal chances, active position',
'$13' => 'unclear position',
'$14' => 'White has a slight advantage',
'$15' => 'Black has a slight advantage',
'$16' => 'White has a moderate advantage',
'$17' => 'Black has a moderate advantage',
'$18' => 'White has a decisive advantage',
'$19' => 'Black has a decisive advantage',
'$20' => 'White has a crushing advantage (Black should resign)',
'$21' => 'Black has a crushing advantage (White should resign)',
'$22' => 'White is in zugzwang',
'$23' => 'Black is in zugzwang',
'$24' => 'White has a slight space advantage',
'$25' => 'Black has a slight space advantage',
'$26' => 'White has a moderate space advantage',
'$27' => 'Black has a moderate space advantage',
'$28' => 'White has a decisive space advantage',
'$29' => 'Black has a decisive space advantage',
'$30' => 'White has a slight time (development) advantage',
'$31' => 'Black has a slight time (development) advantage',
'$32' => 'White has a moderate time (development) advantage',
'$33' => 'Black has a moderate time (development) advantage',
'$34' => 'White has a decisive time (development) advantage',
'$35' => 'Black has a decisive time (development) advantage',
'$36' => 'White has the initiative',
'$37' => 'Black has the initiative',
'$38' => 'White has a lasting initiative',
'$39' => 'Black has a lasting initiative',
'$40' => 'White has the attack',
'$41' => 'Black has the attack',
'$42' => 'White has insufficient compensation for material deficit',
'$43' => 'Black has insufficient compensation for material deficit',
'$44' => 'White has sufficient compensation for material deficit',
'$45' => 'Black has sufficient compensation for material deficit',
'$46' => 'White has more than adequate compensation for material deficit',
'$47' => 'Black has more than adequate compensation for material deficit',
'$48' => 'White has a slight center control advantage',
'$49' => 'Black has a slight center control advantage',
'$50' => 'White has a moderate center control advantage',
'$51' => 'Black has a moderate center control advantage',
'$52' => 'White has a decisive center control advantage',
'$53' => 'Black has a decisive center control advantage',
'$54' => 'White has a slight kingside control advantage',
'$55' => 'Black has a slight kingside control advantage',
'$56' => 'White has a moderate kingside control advantage',
'$57' => 'Black has a moderate kingside control advantage',
'$58' => 'White has a decisive kingside control advantage',
'$59' => 'Black has a decisive kingside control advantage',
'$60' => 'White has a slight queenside control advantage',
'$61' => 'Black has a slight queenside control advantage',
'$62' => 'White has a moderate queenside control advantage',
'$63' => 'Black has a moderate queenside control advantage',
'$64' => 'White has a decisive queenside control advantage',
'$65' => 'Black has a decisive queenside control advantage',
'$66' => 'White has a vulnerable first rank',
'$67' => 'Black has a vulnerable first rank',
'$68' => 'White has a well protected first rank',
'$69' => 'Black has a well protected first rank',
'$70' => 'White has a poorly protected king',
'$71' => 'Black has a poorly protected king',
'$72' => 'White has a well protected king',
'$73' => 'Black has a well protected king',
'$74' => 'White has a poorly placed king',
'$75' => 'Black has a poorly placed king',
'$76' => 'White has a well placed king',
'$77' => 'Black has a well placed king',
'$78' => 'White has a very weak pawn structure',
'$79' => 'Black has a very weak pawn structure',
'$80' => 'White has a moderately weak pawn structure',
'$81' => 'Black has a moderately weak pawn structure',
'$82' => 'White has a moderately strong pawn structure',
'$83' => 'Black has a moderately strong pawn structure',
'$84' => 'White has a very strong pawn structure',
'$85' => 'Black has a very strong pawn structure',
'$86' => 'White has poor knight placement',
'$87' => 'Black has poor knight placement',
'$88' => 'White has good knight placement',
'$89' => 'Black has good knight placement',
'$90' => 'White has poor bishop placement',
'$91' => 'Black has poor bishop placement',
'$92' => 'White has good bishop placement',
'$93' => 'Black has good bishop placement',
'$94' => 'White has poor rook placement',
'$95' => 'Black has poor rook placement',
'$96' => 'White has good rook placement',
'$97' => 'Black has good rook placement',
'$98' => 'White has poor queen placement',
'$99' => 'Black has poor queen placement',
'$100' => 'White has good queen placement',
'$101' => 'Black has good queen placement',
'$102' => 'White has poor piece coordination',
'$103' => 'Black has poor piece coordination',
'$104' => 'White has good piece coordination',
'$105' => 'Black has good piece coordination',
'$106' => 'White has played the opening very poorly',
'$107' => 'Black has played the opening very poorly',
'$108' => 'White has played the opening poorly',
'$109' => 'Black has played the opening poorly',
'$110' => 'White has played the opening well',
'$111' => 'Black has played the opening well',
'$112' => 'White has played the opening very well',
'$113' => 'Black has played the opening very well',
'$114' => 'White has played the middlegame very poorly',
'$115' => 'Black has played the middlegame very poorly',
'$116' => 'White has played the middlegame poorly',
'$117' => 'Black has played the middlegame poorly',
'$118' => 'White has played the middlegame well',
'$119' => 'Black has played the middlegame well',
'$120' => 'White has played the middlegame very well',
'$121' => 'Black has played the middlegame very well',
'$122' => 'White has played the ending very poorly',
'$123' => 'Black has played the ending very poorly',
'$124' => 'White has played the ending poorly',
'$125' => 'Black has played the ending poorly',
'$126' => 'White has played the ending well',
'$127' => 'Black has played the ending well',
'$128' => 'White has played the ending very well',
'$129' => 'Black has played the ending very well',
'$130' => 'White has slight counterplay',
'$131' => 'Black has slight counterplay',
'$132' => 'White has moderate counterplay',
'$133' => 'Black has moderate counterplay',
'$134' => 'White has decisive counterplay',
'$135' => 'Black has decisive counterplay',
'$136' => 'White has moderate time control pressure',
'$137' => 'Black has moderate time control pressure',
'$138' => 'White has severe time control pressure',
'$139' => 'Black has severe time control pressure'
);

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

BEGIN { $| = 1; print "1..5\n"; }

use Chess::PGN::Parse;
use strict;
my $loaded = 1;
print "ok 1 (loaded)\n";

my $pgn1 = new Chess::PGN::Parse "examples/kk_2001.pgn" 
	or die "creation failed\n";
		
my $created1 = 1;
print "ok 2 (creation)\n";

my @array = $pgn1->quick_read_all();
my $parsed1 = @array;
print "ok 3 (read and parse -- quick)\n";

my $pgn2 = new Chess::PGN::Parse "examples/kk_game4_test.pgn"
	or die "creation failed\n";
my $created2 = 1;
print "ok 4 (creation)\n";

@array = $pgn2->read_all({save_comments => 'yes', log_errors => 'yes' });
my $parsed2 = @array;
print "ok 5 (read and parse -- complete)\n";

END {
	print "not ok 1\n" unless $loaded;
	print "not ok 2\n" unless $created1;
	print "not ok 3\n" unless $parsed1;
	print "not ok 4\n" unless $created2;
	print "not ok 5\n" unless $parsed2;
}

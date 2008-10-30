use strict;
use warnings;

my $linguistics = $ARGV[0];
my $types = $ARGV[1];
my $lexicon = $ARGV[2];
my $ltdb = $ARGV[3];

my $lastupdate_ltdb = 0;
if(-e $ltdb){
    # Get the last modification time.
    $lastupdate_ltdb = (stat($ltdb))[9];
}

my $lexicon_updated = 0;
if(-e $lexicon){
    if((stat($lexicon))[9] > $lastupdate_ltdb){
	`perl lexicon2db.perl $lexicon $ltdb`;
	$lexicon_updated = 1;
    }
}else{
    die "Lexicon file ($lexicon) not found.";
}

if(-e $types){
    if((stat($types))[9] > $lastupdate_ltdb){
	`perl types2db.perl $types $ltdb`;
    }
}else{
    die "Types file ($types) not found.";
}

my $linguistics_updated = 0;
if(-e $linguistics){
    if((stat($linguistics))[9] > $lastupdate_ltdb){
	`perl linguistics2db.perl $linguistics $ltdb`;
	$linguistics_updated = 1;
    }
}else{
    die "Linguistics file ($linguistics) not found.";
}

if($lexicon_updated || $linguistics_updated){
    print "true\n";
}else{
    print "false\n";
}


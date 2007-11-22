#!/usr/bin/perl -w
#
#
use strict;
use utf8;
use Encode;
binmode(STDOUT, ":utf8");
binmode(STDERR, ":utf8");


my $revfile = $ARGV[0] || "../lex/Jacy.rev";

open (IN, "<:encoding(utf8)", $revfile); 

while (<IN>) {
    chomp;
    my @list = split /\t/;
    my $carg = $list[5];
    if ($carg eq "\\N") {
	## No CARG
    } elsif ($carg =~ /^"(.*)"$/) {
	## CARG is string
	my $string = $1;
        if ($string =~ /^_([^_]+)_([avnrjxcpmsqx])_(.*)_rel$/ ) {
	    my $lemma = $1;
	    my $pos = $2;
	    my $sense = $3;
	    print "ENTRY - $pos\t$lemma\t$sense\n";
	    ##
	} else {
	     print "WARNING - not RMRS conformant: $string\n";
	}


    } else {
	## CARG is type
	##print "$carg\n";
    }
}


## ToDo rename ersatz to _ersatz or something

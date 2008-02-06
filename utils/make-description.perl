#!/usr/bin/perl   -w
#
# extract the linguistic description xml file and clean it up a little
#
# ./make-description.perl | iconv --from utf8 --to euc-jp > lextypes.xml
#
#
use strict;

my @types = (@ARGV) || ("lex-types.tdl");


### Header
print "<?xml version='1.0' encoding='utf-8' ?>\n<linguistics>\n\n";

for my $typefile (@ARGV) {

open(TDL, $typefile) || die "Can't open $typefile\n";

my $state = "out"; 
my $oldstate = "out";
my $typename = "";
my %items = ();
my $i_id =0;


### Not so robust way of extracting the 
while (<TDL>) {
    chomp;
    $oldstate = $state;
    if ($_ =~ m/^;\s*<([-a-z\/]+)(>|\s)/) { 
	$state = $1;
    };
    ##print "\n :: $oldstate :: $state :: ";
    if ($state eq "type"  || $oldstate ne "out") {
	if ($state eq "type") {
	    print "\n\n";
	} elsif ($state eq "/type") {
	    print  "</$oldstate>\n";
	    $state = "out";
	    $oldstate = "out";
	}elsif((($state ne $oldstate) && ($oldstate ne "type"))
	       || $state eq "ex"|| $state eq "nex") {
	    print "</$oldstate>\n";
	} else {
	    print "\n";
	}
	my $description = $_;
	$description =~ s/^;\s*//; ## clean the front
	$description =~ s/\s+$//;  ## clean the end
	print $description;
	#if($state ne $oldstate && $state !~ /type/
	#    || $state eq "ex") {
	#    print "</$state>";
	#}
	#print "\n";
	## Extract the type name
        if ($state eq "type") {
	    $description =~ /val="(.*)"/;
	    $typename=$1;
	}
        ## Extract the example sentences
	if ($state eq "ex" || $state eq "nex") {
	    my $item = $description;
	    $item =~ s/^<n?ex>//;
	    if ($item) {
		$items{$i_id}{"i-item"} = $item;
		$items{$i_id}{"i-comment"} = $typename;
		if ($state eq "nex") {
		    $items{$i_id}{"i-wf"} = 2;
		} else {
		    $items{$i_id}{"i-wf"} = 1;
		}		    
		$i_id++;
	    }
	}
    }
#   if ($_ eq "/type") { 
#       print "</$state>\n";
#       $oldstate = "out";
#       $state = "out";
#   };

}
}
### Footer

print "\n</linguistics>\n";

#
# print out the example sentences
#
# FIXME: finish the format
# foreach $i_id (sort { $a <=> $b } keys %items) {
#     print join "@",
#     $i_id,
#     $items{$i_id}{"i-item"},
#     $items{$i_id}{"i-wf"},
#     $items{$i_id}{"i-comment"};
#     print "\n";
# }

#
# Todo:
#
# (aa) make the item file a proper item file!
# (a) parse sentences and identify the word in question
#   - mark it in the xml
#   - test that the word is in the correct parse
# (b) mark cross-references and external links
#
#

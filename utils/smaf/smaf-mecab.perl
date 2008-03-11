#!/usr/bin/perl



my $sent = $ARGV[0] || "今日もしないね";
my $cost = $ARGV[1] || 4000;




use MeCab;
use Encode;
use utf8;

my %Node;
my %NodeDist;


my $m = new MeCab::Tagger ("-l3"); # return the lattice
#my $sent = "今日もしないとね";  #example1
#my $n = $m->parseToNode ("東京都");  #
#my $sent = "東京";
$euc=$sent;
## fixme use unicode properly
Encode::from_to($euc, "utf-8", "euc-jp");
my $n = $m->parseToNode ($euc);  #

## fixme use unicode properly
$length = scalar split //,$sent;

#print "S Length :$length:\n";

# go through once, to the end
for (; $n->{next}; $n = $n->{next} ) {};

# search backwords, depth first
&search ($n);



### print header
$date = `date --rfc-3339=seconds`;
chomp $date;

print "<?xml version='1.0' encoding='UTF-8'?>\n";
print " <!DOCTYPE saf SYSTEM 'saf.dtd'>\n";
print " <saf addressing='char'>\n";
print "   <olac:olac xmlns:olac='http://www.language-archives.org/OLAC/1.0/' xmlns='http://purl.org/dc/elements/1.1/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://www.language-archives.org/OLAC/1.0/ http://www.language-archives.org/OLAC/1.0/olac.xsd'>\n";
print "    <creator>mecab2saf.perl</creator>\n";
print "    <created>$date</created>\n";
print "  </olac:olac>\n";

print "<fsm init='N0' final='N$length' >";
### print lattice

for my $id (sort {$a <=> $b} keys %Node) {
    $cto = $length - $NodeDist{$id};
    $cfrom = $cto - $NodeD{$id};
#    $cfrom = $length - $NodeDist{$id}
    $surface = $Node{$id}->{surface};
    Encode::from_to($surface, "euc-jp", "utf-8");
    $features =  $Node{$id}->{feature};
    Encode::from_to($features, "euc-jp", "utf-8");
#    ($pos1, $pos2, $pos3);
    $width = scalar split //, $surface;
    $cfrom = $cto - $width;
    if ($width) {
#	print "<edge type='mecab' ";
	print "<edge type='token' ";
	print "cfrom='$cfrom' ";
	print "cto='$cto' ";
	print "source='N$cfrom' ";
	print "target='N$cto' ";
	print "id='M$id'";
	print ">";
	print  &xml_escape($surface); ### escape me
#	print " ($features)"; ### escape me
#	print "<slot name='surface'>$surface</slot>"; ### escape me
	print "</edge>\n";
    }
}
### print footer
print " </fsm>\n</saf>\n";


    
sub search ()
{
    my $rnode = shift @_;
    my $rid   = $rnode->{id};   # Right Node ID
    my $bcost = $rnode->{cost}; # Right Node cost
    $Node{$rid} = $rnode;       # mark nodes we have seen
#    print "searched: $rid\n";
    # 右 Node に接続するすべての Path について..
    for (my $p = $rnode->{lpath}; $p; $p = $p->{lnext}) {
	my $lnode = $p->{lnode};	
	if ($lnode->{isbest} || $p->{cost} <= $cost) {  # look for costs below this
	my $lid   = $lnode->{id};
#	print "\n$rid is good, linked to $lid\n";
	my $utfsurf = $rnode->{surface};
	Encode::from_to($utfsurf, "euc-jp", "utf-8");
	my $width = scalar split //, $utfsurf;
	if ($rid) {
	    $NodeDist{$lid} = $NodeDist{$rid} + $width;
	}else {
	    $NodeDist{$lid} = 0;
	}
# 	print "$lid [", $length - $NodeDist{$lid}, " - ",
# 	 $length - $NodeDist{$rid},"]  ($utfsurf)\n"; 
	    next if (defined $Node{$lid}); # ignore ones we've done already
	    &search ($lnode);  # recurse 
    

	}
    }
}


# for my $id (sort {$a <=> $b} keys %Node) {
#     print "Id :", $id, ":\n";
#     print "Cost :",   $Node{$id}->{cost}, ":\n";  
#     print "Wcost :",  $Node{$id}->{wcost}, ":\n";
#     print "Surface :",  $Node{$id}->{surface}, ":\n";
#     print "Feature :",  $Node{$id}->{feature}, ":\n";
#     print "Lpath :",  $Node{$id}->{lpath}, ":\n";
# #    print "Rpath :",  $Node{$id}->{rpath}->{id}, ":\n";

# #    print $Node{$id}->{isbest} ? "* " : "  ";  # "*" if it is part of the best parse 
#     # nodeid TAB surface TAB POS1,POS2,POS3,POS5,Paradigm,Inflection,Base,Kana,Pronunciation,
# #    print $id, "\t", $Node{$id}->{surface}, "\t", $Node{$id}->{feature}, "\n";
# }

sub xml_escape {
   my $str = shift;
   $str =~ s/\&/&amp;/g;
   $str =~ s/\>/&gt;/g;
   $str =~ s/\</&lt;/g;
   $str =~ s/\"/&quot;/g;
   $str =~ s/\'/&apos;/g;
   return $str;
}


#<edge cfrom='0' cto='1' type='mecab' id='t1' source='v0' target='v1'>
#    <slot name='surface'>ii</slot>
#    <slot name='pos'>NNP</slot>
#    <slot name='base'>ii</slot>
#    <slot name='pron'>NNP</slot>
#  </edge>


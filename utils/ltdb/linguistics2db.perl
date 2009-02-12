use strict;
use warnings;
use DBI;
use XML::DOM;

my $linguistics = $ARGV[0];
my $dbname = $ARGV[1];

my $tablename = "linguistics_tbl";
my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});

$dbh->do(qq{
    DROP TABLE IF EXISTS $tablename;
}) || die $dbh->errstr;

$dbh->do(qq{
    CREATE TABLE $tablename (
			     type primary key,
			     name,
			     description,
			     criteria,
			     reference,
			     todo
			     );
}) || die $dbh->errstr;

my $insert = $dbh->prepare(
			   "INSERT INTO $tablename VALUES (?,?,?,?,?,?)"
			   );

my $parser = new XML::DOM::Parser;
my $doc = $parser->parsefile ($linguistics);
my $typenodes = $doc->getElementsByTagName ("type");
my $n = $typenodes->getLength;
for(my $i=0; $i<$n; $i++){
    my $typenode = $typenodes->item($i);
    my $type = $typenode->getAttribute("val");
    my @children = $typenode->getChildNodes;
    my $name = "";
    my $description ="";
    my $criteria ="";
    my $exe ="";
    my $reference="";
    my $todo="";
    for(my $j=0; $j<scalar(@children); $j++){
	if($children[$j]->getNodeName eq "name"){
	    $name = $children[$j]->getFirstChild->getData if $children[$j]->getFirstChild;
	    $name =~ s/^[\s\n]*(.+)[\s\n]*$/$1/m;
	}elsif($children[$j]->getNodeName eq "description"){
	    $description = $children[$j]->getFirstChild->getData if $children[$j]->getFirstChild;
	    $description =~ s/^[\s\n]*(.+)[\s\n]*$/$1/m;
	}elsif($children[$j]->getNodeName eq "reference"){
	    $reference = $children[$j]->getFirstChild->getData if $children[$j]->getFirstChild;
	    $reference = &bib2html($reference);
	}elsif($children[$j]->getNodeName eq "todo"){
	    $todo = $children[$j]->getFirstChild->getData if $children[$j]->getFirstChild;
	    $todo =~ s/^[\s\n]*(.+)[\s\n]*$/$1/m;
	}elsif($children[$j]->getNodeName eq "ex"){
	    $exe = $children[$j]->getFirstChild->getData if $children[$j]->getFirstChild;
	    $exe =~ s/^[\s\n]*(.+)[\s\n]*$/$1/m;
	    $criteria .= "ex: $exe\n";
	}elsif($children[$j]->getNodeName eq "nex"){
	    $exe = $children[$j]->getFirstChild->getData if $children[$j]->getFirstChild;
	    $exe =~ s/^[\s\n]*(.+)[\s\n]*$/$1/m;
	    $criteria .= "nex: $exe\n";
	}
    }
    if ($criteria) {
	$criteria = &format_criteria($criteria);
    }
    if ($description) {
	$description = &link($description);
    }
    if ($todo) {
	$todo =  &link($todo);
    }

    $insert->execute($type, $name, $description, $criteria, $reference, $todo);
}

sub bib2html{
    my $b_input = shift;
    my $pid = $$;
    open(BIBIN, ">bib2html_$pid.bib");
    print BIBIN $b_input;
    close BIBIN;
    `bib2html bib2html_$pid.bib`;
    open(BIBHTML, "bib2html_$pid.html");
    my $bibflg = 0;
    my $b_output;
    while(<BIBHTML>){
	if(/<\/TABLE><\/TD><\/TR><\/TABLE><\/CENTER><P><HR>/){
	    $bibflg = 1;
	}elsif($bibflg && /<HR>/){
	    last;
	}elsif($bibflg){
	    $b_output .= $_;
	}
    }
    close BIBHTML;
    unlink "bib2html_$pid.bib";
    unlink "bib2html_$pid.html";
    return $b_output;
}

sub format_criteria{
    my $c_input = shift @_;
    my @c_sentences = split /\n/, $c_input;
    my $c_out = "<table><tr><th colspan='2'>Test Sentences</th></tr>";
    for my $sentence (@c_sentences) {
	#print "SEN $sentence ::\n";
	$c_out .= "<tr>";
	my($type, $text) = split /: /, $sentence; #/;
	if($type eq "nex"){
	    $c_out .= "<td>x</td>";
	}else{
	    $c_out .= "<td></td>";
	}
	$c_out .= "<td>$text</td>";
	$c_out .= "</tr>";
    }
    $c_out .= "</table>";
    $c_out;
}

sub link {
    my ($text) = @_;
    my @tmp = $text =~ /([-_+a-zA-Z0-9]+(-lex|_le))/g;
    for my $tmp (@tmp) {
	if ($tmp ne "-lex" && $tmp ne "_le") {
	    ## FIXME test against list of types
	    ##print STDERR "'$tmp'\n";
	    $text =~ s/\Q$tmp\E/<a href='description.cgi?type=$tmp'>$tmp<\/a>/g;
	}
    }
    return $text;
}
  


$dbh->commit;


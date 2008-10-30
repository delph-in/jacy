#!/usr/local/bin/perl
use strict;
use warnings;
use CGI;
use DBI;

my $query = new CGI;
my $dbroot = "/var/www/html/lextypedb/db";
my $cssdir = "/lextypedb";
my $cgidir = "/cgi-bin/lextypedb_tools";
my $charset = "utf-8";
my $version = "???";
if(-e "params"){
    open(PARAM, "params");
    while(<PARAM>){
	chomp;
	(my $para, my $val) = split /=/;#/
	if($para eq "dbroot"){
	    $dbroot = $val;
	}elsif($para eq "charset"){
	    $charset = $val;
	}elsif($para eq "cssdir"){
	    $cssdir = $val;
	}elsif($para eq "cgidir"){
	    $cgidir = $val;
        }elsif($para eq "version"){
	    $version = $val;
	} 
    }
    close(PARAM);
}

#Receive the lextype param.
my $lextype = $query->param("lextype");

my $dbname = $dbroot."/"."lt.db";
my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});

#Retrieve the linguistic discussion
my $linguistics_table = "linguistics_tbl";
my $sth = $dbh->prepare(
    "select name,definition,criteria,reference,todo from $linguistics_table where type=\'$lextype\'"
);
$sth->execute;
my $linguisitcs;
my $name;
my $todo;
while(my @row = $sth->fetchrow_array){
    $name = $row[0];
    my $definition = $row[1];
    #my $criteria = &format_criteria($row[2]);
    my $criteria = $row[2];
    #my $reference = "<xmp>".$row[3]."</xmp>";
    my $reference = $row[3];
    $todo = $row[4];
    $linguisitcs = $definition."<p />".$criteria."<p />".$reference;
}

#Retrieve example words, the number of types, and the number of tokens.
my $list_table = "list_tbl";
$sth = $dbh->prepare(
    "select words,typefreq,tokenfreq from $list_table where lextype=\'$lextype\'"
);
$sth->execute;
my @words;
my @word_ids;
my $typefreq;
my $tokenfreq;
while(my @row = $sth->fetchrow_array){
    my @word_and_id_tmp = split /@/, $row[0];
    my @examples_a = ();
    foreach (@word_and_id_tmp){
	my @word_and_id = split /:/, $_; #/
	push(@word_ids, $word_and_id[0]);
	push(@words, $word_and_id[1]);
    }
    $typefreq = $row[1];
    $tokenfreq = $row[2];
}
my $words_s = join(",&nbsp;&nbsp;", @words);
my @words_and_ids;
for(my $i=0; $i<scalar(@words); $i++){
    push(@words_and_ids, $words[$i]." (".$word_ids[$i].")");
}
my $words_and_ids_s = join(",&nbsp;&nbsp;", @words_and_ids);

#Retrieve three example sentences for each example word.
my $sentences;
my $treebank_table = "treebank_tbl";
$sth = $dbh->prepare(
    "select sid from $treebank_table where lextype=\'$lextype\' and wordid=? limit 3"
);
my $sth2 = $dbh->prepare(
    "select wordid,orth from $treebank_table where sid=?"
);
for(my $i=0; $i<scalar(@word_ids); $i++){
    $sentences .= "<h4>Examples for ".$words[$i]." (".$word_ids[$i].")</h4>";
    my @sids;
    $sth->execute($word_ids[$i]);
    while(my @row = $sth->fetchrow_array){
	push(@sids, $row[0]);
    }
    $sentences .= "<ul>";
    foreach my $sid (@sids){
	my $sentence = "";
	$sth2->execute($sid);
	while(my @row = $sth2->fetchrow_array){
	    if($row[0] eq $word_ids[$i]){
		$sentence .= "<match>".$row[1]."</match>";
	    }else{
		$sentence .= $row[1];
	    }
	    $sentence .= " ";
	}
	$sentences .= "<li>".$sentence;
    }
    $sentences .= "</ul>";
}

#Retrieve the TDL definition for the lextype.
my $tdl_definition;
my $types_table = "types_tbl";
$sth = $dbh->prepare(
    "select parents,cat,val,cont,definition from $types_table where type=\'$lextype\'"
);
$sth->execute;
while(my @row = $sth->fetchrow_array){
    my @supertype_a = split / /, $row[0];

    my $definition = $row[4];
    $definition =~ s/<br \/>/\n/g;
    $definition =~ s/^/<xmp>/;
    $definition =~ s/$/<\/xmp>/;
    $tdl_definition .= $definition;

    $tdl_definition .= "<table>";
    $tdl_definition .= "<tr><th>Supertypes</th><th>Head Category</th><th>Valence</th><th>Content</th></tr>";
    $tdl_definition .= "<tr>";
    $tdl_definition .= "<td>";
    for(my $i=0; $i<scalar(@supertype_a); $i++){
	$tdl_definition .= "<a href=\'$cgidir/moreSupertypes.cgi?supertype=$supertype_a[$i]\'>$supertype_a[$i]</a>&nbsp;&nbsp;";
    }
    $tdl_definition .= "</td>";
    $tdl_definition .= "<td>$row[1]</td>";
    $tdl_definition .= "<td>$row[2]</td>";
    $tdl_definition .= "<td>$row[3]</td>";
    $tdl_definition .= "</tr>";
    $tdl_definition .= "</table>";
}

# # Retrieve the Other Lexicon Mapping information for the lextype.
# my $otherlex_table = "otherlex_tbl";
# $sth = $dbh->prepare(
#     "select lexicon, otherlextype, dice from $otherlex_table where lextype=\'$lextype\'"
# );
# $sth->execute;
# my %lexMap_tmp;
# while(my @row = $sth->fetchrow_array){
#     $lexMap_tmp{$row[0]}{$row[1]} = $row[2];
# }
# my %lexMap;
# foreach my $lxcn (keys(%lexMap_tmp)){
#     my @sorted_types = sort {
# 	$lexMap_tmp{$lxcn}{$b} <=> $lexMap_tmp{$lxcn}{$a}
#     } keys %{$lexMap_tmp{$lxcn}};
#     push(@{$lexMap{$lxcn}}, ($sorted_types[0], sprintf("%.3f",$lexMap_tmp{$lxcn}{$sorted_types[0]})));
# }

# # Retrieve the GOLD Mapping information for the lextype.
# my $gold;
# my $gold_table = "gold_tbl";
# $sth = $dbh->prepare(
#     "select gold from $gold_table where lextype=\'$lextype\'"
# );
# $sth->execute;
# while(my @row = $sth->fetchrow_array){
#     $gold = $row[0];
# }

# Message -------------------------------------
print $query->header(-type  =>  'text/html;charset='.$charset),
    $query->start_html(-title=>$lextype.' (description)',
		       -style=>{'src' => $cssdir.'/lextypedb.css'});
print <<"HTML_VIEW";
<div id="outline">
<div id="header">
<div id="menu">
<a href=$cssdir/index.html>Home</a>
&nbsp;&nbsp;
<a href="$cgidir/list.cgi">List</a>
&nbsp;&nbsp;
<a href=$cssdir/admin.html>Admin</a>
</div> <!-- end of menu -->
</div> <!-- end of header -->
<div id="confusing">
<form name="frm1" action="$cgidir/searchWord.cgi" method="POST">
Word Search:&nbsp;<input type="text" name="confusing">
<input type="submit" value="Submit" name="submitbtn">
<input type="reset" value="Reset" />
</form>
</div> <!-- end of confusing -->
<div id="contents">
<h1>$name&nbsp;&nbsp;$lextype&nbsp;&nbsp;($words_s)</h1>
<h2>Linguistic Discussion</h2>
$linguisitcs
<h2>Examples</h2>
<h3>Example Words&nbsp;&nbsp;($typefreq)</h3>
$words_and_ids_s
<h3>Example Sentences&nbsp;&nbsp;($tokenfreq)</h3>
$sentences
<div align=right>
<a href="$cgidir/moreExamples.cgi?lextype=$lextype">More Examples</a>
</div>
<h2>TDL Summary</h2>
<h3>TDL Definition</h3>
$tdl_definition
<h3>TODO</h3>
$todo

</div> <!-- end of contents -->
<div id="c-both"><br></div>
</div> <!-- end of outline -->
HTML_VIEW
print $query->end_html;
exit;

# Error report -----------------------------------
sub error {
  my ($mes) = @_;

  print $query->header(-type  =>  'text/html;charset='.$charset),
  $query->start_html(-title=>'Creation Error',
		     -style=>{'src' => $cssdir.'/lextypedb.css'});

  print <<"HTML_VIEW";
  <div id="outline">
  <div id="header">
  <div id="menu">
  <a href=$cssdir/index.html>Home</a>
  &nbsp;&nbsp;
  List
  &nbsp;&nbsp;
  <a href=$cssdir/admin.html>Admin</a>
  </div> <!-- end of menu -->
  </div> <!-- end of header -->
  <div id="contents">
  <h1>Lexical Type Database:  ERROR</h1>
  <p>$mes</p>
  </div> <!-- end of contents -->
  <div id="c-both"><br></div>
  </div> <!-- end of outline -->
HTML_VIEW
  print $query->end_html;
  exit;
}
__END__

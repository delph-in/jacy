#!/usr/local/bin/perl -w
use strict;
use CGI;

my $query = new CGI;
my $dbroot = "/var/www/html/lextypedb/db";
my $cssdir = "/lextypedb";
my $cgidir = "/cgi-bin/lextypedb_tools";
my $charset = "utf-8";
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
       }
    }
    open(PARAM, "params");
}
## only show $limit sentences
my $limit = 50;

#Receive params.
my $lextype = $query->param("lextype");
my $wid = $query->param("wid");
my $orth = $query->param("orth");

#Retrieve all sentences that contain a word
#whose lextype is $lextype and wid is $wid.
use DBI;
my $treebank_table = "treebank_tbl";
my $dbname = $dbroot."/"."lt.db";
my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});
#First, retrieve all the sentence ids.
my $sth = $dbh->prepare(
			"select sid from $treebank_table where lextype=\'$lextype\' and wordid=\'$wid\' limit $limit"
			);
$sth->execute;
my @sid_a;
while(my @row = $sth->fetchrow_array){
    push(@sid_a, $row[0]);
}
#Next, retrieve the sentence itself for each sentence id.
$sth = $dbh->prepare(
		     "select wordid,orth from $treebank_table where sid=?"
		     );
my $total = 0;
my %all_sentences;
foreach my $sid (@sid_a){
    my @sentence = ();
    $sth->execute($sid);
     while(my @row = $sth->fetchrow_array){
 	my $orth = $row[1];
 	if ($row[0] eq $wid) {
 	    push  @sentence, "<match>$orth</match>";
 	} else {
	    push  @sentence, $orth; 
 	}
     }
    my $string = join " ", @sentence;
    $string .= "\n";
    $all_sentences{$sid} = $string;
    $total++;
}
#
#Finally, prepare the output table that contains all sentences.
my $out = "<table>";
 $out .= "<tr><th>Sentences</th><th>ex</th></tr>";
 foreach my $sid (keys %all_sentences){
     $out .= "<tr><td>$all_sentences{$sid}</td>";
     $out .= "<td><a href = '$cssdir/db/trees/$sid.html'>$sid</a></td></tr>";
 }
$out .= "</table>";

# Message -------------------------------------
print $query->header(-type  =>  'text/html;charset='.$charset),
    $query->start_html(-title=>$lextype.' (sentences)',
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
<form name="frm1" action="$cgidir/confusingTypes.cgi" method="POST">
Confusing Types:&nbsp;<input type="text" name="confusing">
<input type="submit" value="Submit" name="submitbtn">
<input type="reset" value="Reset" />
</form>
</div> <!-- end of confusing -->
<div id="contents">
<h1>$lextype&nbsp;(sentences)</h1>
First $total sentences containing "$orth" (max $limit).
<div align=center>$out</div>
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
  <a href="$cgidir/list.cgi">List</a>
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

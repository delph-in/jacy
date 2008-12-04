#!/usr/local/bin/perl
use strict;
use warnings;
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
    close(PARAM);
}
#Receive the lextype param.
my $lextype = $query->param("lextype");

#Retrieve all words that belong to the lexical type.
use DBI;
my $lexicon_table = "lex_and_freq_tbl";
my $dbname = $dbroot."/"."lt.db";
my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});
### sort in descending frequency 
my $sth = $dbh->prepare(
    "select wordid,orth,freq from $lexicon_table where lextype=? order by freq + 0 desc"
);
$sth->execute($lextype);
my $total = 0;
my $out = "<table>";
$out .= "<tr><th>Orthography</th><th>ID</th><th>Freq.</th><th></th></tr>";
while(my  @row = $sth->fetchrow_array){
    $out .= "<tr>";
    $out .= "<td>".$row[1]."</td>";
    $out .= "<td>".$row[0]."</td>";
    $out .= "<td>".$row[2]."</td>";
    $out .= "<td><a href=\"$cgidir/moreSentences.cgi?lextype=$lextype&wid=$row[0]&orth=$row[1]\">";
    $out .= "sentences</a></td>";
    $out .= "</tr>";
    $total++;
}
$out .= "</table>";

# Message -------------------------------------
print $query->header(-type  =>  'text/html;charset='.$charset),
    $query->start_html(-title=>$lextype.' (words)',
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
<h1>$lextype&nbsp;(words)</h1>
$total words are found.
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

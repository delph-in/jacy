#!/usr/local/bin/perl -w
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
    open(PARAM, "params");
}

#Retrieve all lexical types.
my $list_table = 'list_tbl';
my $dbname = $dbroot."/"."lt.db";
my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});

my $sth = $dbh->prepare(
    "select lextype, name, words,typefreq, tokenfreq from $list_table"
);

$sth->execute;

my $total=0;
my $out = "<table>";
$out .= "<tr><th>Lexical Type</th><th>Linguistic Name</th><th>Example (Lexicon, Corpus)</th></tr>";
while(my @row = $sth->fetchrow_array){
    my @word_and_id_tmp = split /@/, $row[2];
    my @examples_a = ();
     foreach (@word_and_id_tmp){
	 my ($lid, $word) = split /:/, $_; #/
	 push(@examples_a, "<a title ='$lid'>$word</a>");
     }
    my $examples = join(",&nbsp;&nbsp;", @examples_a) || "<br>";
    my $name = $row[1] || "<br>";
    $out .= "<tr>";
    $out .= "<td><a href=\"$cgidir/description.cgi?lextype=$row[0]\">".$row[0]."</a></td>";
    $out .= "<td>".$name."</td>";
    $out .= "<td>".$examples."&nbsp;&nbsp;(".$row[3].",&nbsp;".$row[4].")"."</td>";
    $out .= "</tr>\n";
    $total++;
}
$out .= "</table>";
$dbh->commit;

# Message -------------------------------------
print
    $query->start_html(-title=>'Lexical Type Database: ',$version,
		       -style=>{'src' => $cssdir.'/lextypedb.css'});
print <<"HTML_VIEW";
<div id="outline">
<div id="header">
<div id="menu">
<a href=$cssdir/index.html>Home</a>
&nbsp;&nbsp;
List
</div> <!-- end of menu -->
</div> <!-- end of header -->
<div id="confusing">
<form name="frm1" action="$cgidir/searchWord.cgi" method="POST">
Word Search:&nbsp;<input type="text" name="confusing">
<input type="submit" value="Submit" name="submitbtn">
<input type="reset" value="Clear" />
</form>
</div> <!-- end of confusing -->
<div id="contents">

<h1>Lexical Type Database: $version</h1>
$total Lexical Types are found.
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

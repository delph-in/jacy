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

#Receive params.
my $supertype = $query->param("supertype");

#Retrieve the TDL definition for the supertype.
use DBI;
my $tdl_definition;
my $types_table = "types_tbl";
my $dbname = $dbroot."/"."lt.db";
my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});
my $sth = $dbh->prepare(
			"select parents,cat,val,cont,definition from $types_table where type=\'$supertype\'"
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

# Message -------------------------------------
print $query->header(-type  =>  'text/html;charset='.$charset),
    $query->start_html(-title=>$supertype.' (supertype)',
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
<h1>$supertype&nbsp;(supertype)</h1>
<h3>TDL Definition</h3>
$tdl_definition
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

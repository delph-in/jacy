use strict;
use warnings;
use DBI;

my $dbname = $ARGV[0];
my $lex_and_freq_table = 'lex_and_freq_tbl';
my $linguistics_table = 'linguistics_tbl';
my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});

# Get all lexical types.
my @lextypes;
my $sth = $dbh->prepare(
    "select distinct lextype from $lex_and_freq_table order by lextype"
);
$sth->execute;
while(my @row = $sth->fetchrow_array){
    push(@lextypes, $row[0]);
}

# Get the number of types, the number of tokens, and
# the most frequent three example words for each lextype.
my @list_table_a;
$sth = $dbh->prepare(
    "select wordid,orth,freq from $lex_and_freq_table where lextype=?"
);

# Make sure that $linguistics_table exists.
my $ling_flg = 0;
my $sth2 = $dbh->prepare(
    "SELECT name FROM sqlite_master WHERE type='table' AND name = '$linguistics_table' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='table' AND name = '$linguistics_table' ORDER BY name"
);
$sth2->execute;
while(my @row = $sth2->fetchrow_array){
    if($row[0] eq $linguistics_table){
	$ling_flg = 1;
	last;
    }
}

$sth2 = $dbh->prepare(
    "select name from $linguistics_table where type=?"
);
foreach my $lt (@lextypes){
    my $numOfTypes=0;
    my $numOfTokens=0;
    my %allwords;
    $sth->execute($lt);
    while(my @row = $sth->fetchrow_array){
	$numOfTypes++;
	$numOfTokens += $row[2];
	push(@{$allwords{$row[0]}}, ($row[1],$row[2]));
    }
    my @sorted_wordids = sort {
	${$allwords{$b}}[1] <=> ${$allwords{$a}}[1]
    } keys %allwords;
    my @mostFrequentWords = ();
    for(my $i=0; $i<scalar(@sorted_wordids) && $i<3; $i++){
	# wordid:orth
	$mostFrequentWords[$i] = $sorted_wordids[$i].":".$allwords{$sorted_wordids[$i]}->[0];
    }
    # wordid:orth@wordid:orth@wordid:orth
    my $mostFrequentWords_s = join("@", @mostFrequentWords);
    # get the linguistic name
    my $linguistic_name;
    if($ling_flg){
	$sth2->execute($lt);
	while(my @row = $sth2->fetchrow_array){
	    $linguistic_name = $row[0];
	}
    }else{
	$linguistic_name = "";
    }
    push(@list_table_a, [($lt, $linguistic_name, $mostFrequentWords_s, $numOfTypes, $numOfTokens)]);
}

# Create the Lextype List Table.
my $list_table = 'list_tbl';

$dbh->do(qq{
    DROP TABLE IF EXISTS $list_table;
}) || die $dbh->errstr;

$dbh->do(qq{
    CREATE TABLE $list_table (
			      lextype primary key,
			      name,
			      words,
			      typefreq,
			      tokenfreq
			      );
}) || die $dbh->errstr;
my $insert = $dbh->prepare(
			   "INSERT INTO $list_table VALUES (?,?,?,?,?)"
			   );
foreach my $a_ref (@list_table_a){
    $insert->execute($a_ref->[0],$a_ref->[1],$a_ref->[2],$a_ref->[3],$a_ref->[4]);
}

$sth -> finish();
$dbh->commit;

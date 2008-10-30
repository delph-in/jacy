use strict;
use warnings;
use DBI;

my $dbname = shift @ARGV;
my @tbresults = @ARGV;
my @tbdata;

my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});
my $tb_tablename = 'treebank_tbl';
my $lex_tablename = 'lex_and_freq_tbl';

# Reading the lexicon table.
my $sth = $dbh->prepare(
			"select lextype,wordid from $lex_tablename"
			);
$sth->execute;
my %lexicon;
while(my @row = $sth->fetchrow_array){
    $lexicon{$row[1]} = $row[0];
}

# Reading treebank result files.
my @all_tb_data;
foreach my $tbresult (@tbresults){
    open TB, $tbresult or die "Can't open $tbresult: $!";
    my $sid = 'undefined';
    my %processedSentences;
    while(<TB>){
	chomp;
	# Extracting a parse result
	/^(.*)?\@.*?\@.*?\@.*?\@.*?\@.*?\@.*?\@.*?\@.*?\@.*?\@(.+)\@.*?\@.*?\@.*?\@$/;
	next if defined $processedSentences{$1};
	my $sid = $tbresult.'-'.$1; ### assume it is unique
	$processedSentences{$1} = 1;
	my @tmp = $2 =~ /(\([^\(]+?\([^\(]+?\)\))/g;
	
	# Extracting pairs of a word-id and an orthography
	foreach (@tmp){
	    my $tbline;
	    $tbline->{sid} = $sid;
	    /\"(.+?)\"/;#"
	    $tbline->{orth} = $1;
	    my @tmp2 = split / /;
	    # $tmp2[1]:word id
	    $tbline->{wordid} = $tmp2[1];
	    $tbline->{lextype} = $lexicon{$tmp2[1]};

	    push @all_tb_data, $tbline;
	}
    }
    close TB;
}

$dbh->do(qq{
    DROP TABLE IF EXISTS $tb_tablename;
}) || die $dbh->errstr;

$dbh->do(qq{
    CREATE TABLE $tb_tablename (
				sid,
				wordid,
				orth,
				lextype
				);
}) || die $dbh->errstr;

my $insert = $dbh->prepare(
			   "INSERT INTO $tb_tablename VALUES (?,?,?,?)"
			   );

foreach (@all_tb_data){
    my $lextype = defined $_->{lextype} ? $_->{lextype} : 'undefined';
    #print STDERR "HOGE:[$_->{sid}, $_->{wordid}, $_->{orth}, $lextype]\n";
    $insert->execute($_->{sid}, $_->{wordid}, $_->{orth}, $lextype);
}
##
## add in the frequencies
##
## Word
$sth = $dbh->prepare(
    "SELECT wordid, count(wordid) FROM $tb_tablename GROUP BY wordid"
    );
$sth->execute;

my $update = $dbh->prepare(
    "UPDATE $lex_tablename SET freq=? WHERE wordid=?"
    );

while(my @row = $sth->fetchrow_array){
    my ($wordid, $count) = @row;
    ##print STDERR "$wordid: $count\n";
    $update->execute($count, $wordid);
}
# ## Type
# $sth = $dbh->prepare(
#     "SELECT lextype, count(lextype) FROM $tb_tablename GROUP BY lextype"
#     );
# $sth->execute;
# my $type_tablename = "list_tbl";

# $update = $dbh->prepare(
#     "UPDATE $type_tablename SET tokenfreq=? WHERE lextype=?"
#     );

# while(my @row = $sth->fetchrow_array){
#     my ($lextype, $count) = @row;
#     ##print STDERR "$lextype: $count\n";
#     $update->execute($count, $lextype);
# }


$dbh->commit;

use strict;
use DBI;

my $infile = $ARGV[0];
my $dbname = $ARGV[1];
my $tablename = "lex_and_freq_tbl";
my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});

$dbh->do(qq{
    DROP TABLE IF EXISTS $tablename;
}) || die $dbh->errstr;

$dbh->do(qq{
    CREATE TABLE $tablename (
			     wordid primary key,
			     lextype,
			     orth,
			     freq
			     );
}) || die $dbh->errstr;

my $insert = $dbh->prepare(
			   "INSERT INTO $tablename VALUES (?,?,?,?)"
			   );

open(FILE, $infile) or die "Can't open $infile: $!";
while(<FILE>){
    chomp;
    (my $wordid, my $lextype, my $orth) = split /\t/;
    $insert->execute($wordid, $lextype, $orth, '0');
}
close FILE;

$dbh->commit;

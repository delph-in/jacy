use strict;
use warnings;
use DBI;
use XML::DOM;

my $types_file = $ARGV[0];
my $dbname = $ARGV[1];

my $parser = new XML::DOM::Parser;
my $doc = $parser->parsefile ($types_file);

my $types = $doc->getElementsByTagName ("type");
my $n = $types->getLength;

my @typeinfo_a;
for (my $i = 0; $i < $n; $i++){
    my $typeinfo;

    my $type = $types->item ($i);

    my $name = $type->getAttributeNode ("name");
    $typeinfo->{type} = defined $name ? $name->getValue : '';

    my $parents = $type->getAttributeNode ("parents");
    $typeinfo->{parents} = defined $parents ? $parents->getValue : '';

    my $children = $type->getAttributeNode ("children");
    $typeinfo->{children} = defined $children ? $children->getValue : '';

    my $cat = $type->getAttributeNode ("cat");
    $typeinfo->{cat} = defined $cat ? $cat->getValue : '';

    my $val = $type->getAttributeNode ("val");
    $typeinfo->{val} = defined $val ? $val->getValue : '';

    my $cont = $type->getAttributeNode ("cont");
    $typeinfo->{cont} = defined $cont ? $cont->getValue : '';

    my $def = $type->getFirstChild->getData;
    $def =~ s/^\n*(.+?)\n*$/$1/gm;
    $def =~ s/\n/<br \/>/gm;
    $typeinfo->{definition} = defined $def ? $def : '';

    push @typeinfo_a, $typeinfo;
}

my $tablename = "types_tbl";
my $dbh = DBI->connect("dbi:SQLite:dbname=$dbname", "", "", {AutoCommit => 0});

$dbh->do(qq{
    DROP TABLE IF EXISTS $tablename;
}) || die $dbh->errstr;

$dbh->do(qq{
    CREATE TABLE $tablename (
			     type,
			     parents,
                             children, 
			     cat,
			     val,
			     cont,
			     definition
			     );
}) || die $dbh->errstr;

my $insert = $dbh->prepare(
			   "INSERT INTO $tablename VALUES (?,?,?,?,?,?,?)"
			   );

foreach my $typeinfo (@typeinfo_a){
    $insert->execute(
		     $typeinfo->{type},
		     $typeinfo->{parents},
		     $typeinfo->{children},
		     $typeinfo->{cat},
		     $typeinfo->{val},
		     $typeinfo->{cont},
		     $typeinfo->{definition}
		     );
}

$dbh->commit;

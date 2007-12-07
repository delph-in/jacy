#!/usr/bin/perl
$grammardir = "/home/bond/delphin/grammars/japanese";
$lexdir =     "$grammardir/lex";

@lexicons = qw (
		kyoudai-all.tdl
		adjadv-lex.tdl
		aux-stem-lex.tdl
		funct-lex.tdl
		idiom-lex.tdl
		light-verbs-lex.tdl
		noun-lex.tdl
		numbers-lex.tdl
		p-lex.tdl
		pn-lex.tdl
		verbstem-lex.tdl
		vn-lex.tdl
		v-ends-lex.tdl		ambiguous-lex.tdl
		);
@lrules = qw ( infl.tdl );
@rules =  qw ( japgram.tdl ambiguous-rules.tdl );

@ltypes = qw ( lex-types.tdl v-lex-types.tdl);
@rest   = qw ( tdl-built-ins.tdl
	       matrix.tdl
	       mrsbasic.tdl
	       newlexsem-types.tdl
	       values.tdl
	       fundamentals.tdl
	       rule-types.tdl
	       principles.tdl );


for $file (@ltypes) {
#    print "$grammardir/$file\n";    
open(IN, "$grammardir/$file");
    while (<IN>) {
	if(/^[^;]+:[<=]/) {  $nolextypes++ }
    }
}
for $file (@lexicons) {
#    print "$lexdir/$file\n";    
open(IN, "$lexdir/$file");
    while (<IN>) {
	if(/^[^;]+:[<=]/) {  $nolexitems++ }
    }
}



print "Lextypes:  $nolextypes\n"; 
print "Lexical Entries:  $nolexitems\n"; 

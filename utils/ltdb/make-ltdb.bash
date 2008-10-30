#!/bin/bash
#
# ToDo: parametrize special values (HEAD VALENCE CONTENT)
#

#lkbdir=/home/bond/delphin/lkb
lkbdir=/home/bond/logon/lingo/lkb

grammardir=/home/bond/svn/jacy; 
ltypes=("$grammardir/lex-types.tdl $grammardir/v-lex-types.tdl")
treebanks=`ls -d /home/bond/svn/jacy/gold/tc*`

#grammardir=/home/bond/logon/lingo/erg;
#ltypes="$grammardir/letypes.tdl"
#grammardir=/home/bond/logon.old/ntnu.old/norsource; 
#ltypes="$grammardir/norsk.tdl"
#grammardir=/home/bond/logon/dfki/gg
#ltypes="$grammardir/le-types.tdl"
#grammardir=/home/bond/grammars/matrix
#ltypes="$grammardir/pseudo_japanese.tdl"

echo 
echo "Creating a lextypedb for the grammar stored at: $grammardir"
echo

### Constants
LTDB_FILE="lt.db"
LINGUISTICS_FILE="linguistics.xml"
TYPES_FILE="types.xml"
LEXICON_FILE="lex.tab"
TB_FILE="result"


### I really don't want to do this!
if [ -f  $grammardir/Version.lsp ]; then
    versionfile=$grammardir/Version.lsp
else
    versionfile=$grammardir/Version.lisp
fi

version=`perl -ne 'if (/^\(defparameter\s+\*grammar-version\*\s+\"(.*)\s+\((.*)\)\"/) {print "$1_$2"}' $versionfile`
if [ -z "$version" ]; then
    echo "Don't know the version, will use 'something'"
    version=something
fi
outdir=$PWD/$version

HTML_DIR=$HOME/public_html/ltdb/$version
CGI_DIR=$HOME/public_html/cgi-bin/$version

### make the output directory
echo "Writing output to $outdir"
mkdir -p $outdir

### dump the lexicon


### dump  the lex-types
echo "Dumping lex-type definitions and lexicon" 

#lisp="/home/bond/logon/franz/linux.x86.64/alisp -I bclim.dxl"
lisp="/home/bond/logon/franz/linux.x86.32/alisp -I base.dxl"
#lisp="/usr/bin/sbcl --dynamic-space-size 4000"  ## and change exit to quit

unset DISPLAY

{ 
 cat 2>&1 <<- LISP
  (load "$lkbdir/src/general/loadup")
  (compile-system "lkb" :force t)
  (lkb::read-script-file-aux  "$grammardir/lkb/script")
  (lkb::lkb-load-lisp "." "patch-lextypedb.lsp")
  (lkb::output-types :xml "$outdir/$TYPES_FILE")
  (lkb::output-lex-summary lkb::*lexicon* "$outdir/$LEXICON_FILE")
  (format t "~%All Done!~%")
  #+allegro        (excl:exit)
  #+sbcl           (sb-ext:quit)
LISP
} | $lisp -I $image -qq 1>&1
#} | cat 

###
### Try to validate the types.xml
###
if which nsgmls  &> /dev/null; then
    env SP_BCTF="utf-8" nsgmls -s  $outdir/$TYPES_FILE
else 
    echo
    echo "   $TYPES_FILE not validated, please install nsgmls."
    echo
fi

###
### Dump the type descriptions 
###
echo
echo "Dumping lex-type descriptions"
echo
./make-description.perl $ltypes > $outdir/$LINGUISTICS_FILE

###
### Validate the type desciptions 
###
if which nsgmls  &> /dev/null; then
    env SP_BCTF="utf-8"  nsgmls  -s $outdir/$LINGUISTICS_FILE
else 
    echo
    echo "   $LINGUISTICS_FILE not validated, please install nsgmls."
    echo 
fi
###
### make the databases
###
echo
echo "Creating the databases ..."
echo


if [ -e $outdir/$LEXICO_FILE ];
then
    echo Create the SQL lexicon DB
    perl lexicon2db.perl $outdir/$LEXICON_FILE $outdir/$LTDB_FILE
else
    echo "No lexicon in $outdir/$LEXICON_FILE"
fi

if [ -e $outdir/$TYPES_FILE ];
then
    echo Create the SQL types DB
    perl types2db.perl $outdir/$TYPES_FILE $outdir/$LTDB_FILE
else
    echo "No type file in $outdir/$TYPES_FILE"
fi

if [ -e $outdir/$LINGUISTICS_FILE ];
then
    echo Create the SQL description DB
    perl linguistics2db.perl $outdir/$LINGUISTICS_FILE $outdir/$LTDB_FILE
else
    echo "No description in $outdir/$LINGUISTICS_FILE"
fi

echo Create the treebank DB
# Reading in treebank files.
tbargs=''
for tb in ${treebanks[@]} ; do
    if [ ! -e $tb/$TB_FILE ]; then
 	echo "Couldn't find a treebank at $tb"
 	exit
    fi
    tbargs=$tbargs' '$tb/$TB_FILE
done
perl treebank2db.perl $outdir/$LTDB_FILE $tbargs 

# TODO
# addlinks lt.db gold.tgz otherlex.gz

echo Create the master list
perl mklist.perl $outdir/$LTDB_FILE

echo
echo Install to $CGI_DIR
echo
mkdir -p $CGI_DIR
mkdir -p $HTML_DIR
cp html/*.cgi $CGI_DIR/.

MYPERL=`which perl`
sed -i "1,1 s|#!/usr/local/bin/perl|#!$MYPERL|"  $CGI_DIR/*.cgi

cp $outdir/$LTDB_FILE $CGI_DIR/.
cp html/lextypedb.css $HTML_DIR/.
echo "charset=utf-8" > $CGI_DIR/params
echo "dbroot=$CGI_DIR" >> $CGI_DIR/params
echo "cssdir=http://localhost/~$USERNAME/ltdb/$version" >> $CGI_DIR/params
echo "cgidir=http://localhost/~$USERNAME/cgi-bin/$version" >> $CGI_DIR/params
echo "version=$version" >> $CGI_DIR/params



### All done
echo
echo "Done: take a look at http://localhost/~$USERNAME/cgi-bin/$version/list.cgi"
echo
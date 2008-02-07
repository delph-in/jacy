#!/bin/bash
#
# ToDo: parametrize special values (HEAD VALENCE CONTENT)
#

lkbdir=/home/bond/delphin/lkb

#grammardir=/home/bond/svn/jacy; 
#ltypes="$grammardir/lex-types.tdl $grammardir/v-lex-types.tdl"
#grammardir=/home/bond/logon/lingo/erg;
#ltypes="$grammardir/letypes.tdl"
#grammardir=/home/bond/logon.old/ntnu.old/norsource; 
#ltypes="$grammardir/norsk.tdl"
#grammardir=/home/bond/logon/dfki/gg
#ltypes="$grammardir/le-types.tdl"
grammardir=/home/bond/grammars/matrix
ltypes="$grammardir/pseudo_japanese.tdl"


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


### make the output directory
echo "Writing output to $outdir"
mkdir -p $outdir

### dump the lexicon


### dump  the lex-types
echo "Dumping lex-type definitions and lexicon" 

lisp="/home/bond/logon/franz/linux.x86.64/alisp -I bclim.dxl"
#lisp="/usr/bin/sbcl --dynamic-space-size 4000"  ## and change exit to quit

unset DISPLAY

{ 
 cat 2>&1 <<- LISP
  (load "$lkbdir/src/general/loadup")
  (compile-system "lkb" :force t)
  (lkb::read-script-file-aux  "$grammardir/lkb/script")
  (lkb::lkb-load-lisp "." "patch-lextypedb.lsp")
  (lkb::output-types :xml "$outdir/types.xml")
  (lkb::output-lex-summary lkb::*lexicon* "$outdir/lex.tab")
  (format t "~%All Done!~%")
  #+allegro        (excl:exit)
  #+sbcl           (sb-ext:quit)
LISP
} | $lisp -I $image -qq 1>&1
#} | cat 

env SP_BCTF="utf-8" nsgmls -s  $outdir/types.xml 

### dump the description

echo "Dumping lex-type descriptions"

./make-description.perl $ltypes > $outdir/linguistics.xml

### validate
env SP_BCTF="utf-8"  nsgmls  -s $outdir/linguistics.xml

### make the database

echo "Creating the database"

#makedb.perl $outdir/lex.tab types.xml  linguistics.xml lt.db

#addtrees lt.db file1 file2 

#addlinks lt.db gold.tgz otherlex.gz


### All done
echo "Done"
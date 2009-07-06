#!/bin/bash
#
# ToDo: parametrize special values (HEAD VALENCE CONTENT)
#

unset DISPLAY;
unset LUI;

while [ $# -gt 0 -a "${1#-}" != "$1" ]; do
  case ${1} in
    --grm)
      grm=${2};
      shift 2;
      ;;
  esac
done

source ltdb-conf.bash

echo 
echo "Creating a lextypedb for the grammar stored at: $grammardir"
echo


echo
echo "It will be installed into:"
echo "   $HTML_DIR"
echo "   $CGI_DIR"
echo 
echo "Would you like to continue (y/n)?"
read
case $REPLY in
   no|n) exit 0;;
    *) echo "Keeping on" ;;
esac




### make the output directory
echo "Writing output to $outdir"
mkdir -p $outdir

### dump the lexicon


### dump  the lex-types
echo "Dumping lex-type definitions and lexicon" 



{ 
 cat 2>&1 <<- LISP
  ;(load "$lkbdir/src/general/loadup")
  ;(compile-system "lkb" :force t)
  (lkb::read-script-file-aux  "$grammardir/lkb/script")
  (lkb::lkb-load-lisp "." "patch-lextypedb.lsp")
  (lkb::output-types :xml "$outdir/$TYPES_FILE")
  (lkb::output-lex-summary lkb::*lexicon* "$outdir/$LEXICON_FILE")
  (format t "~%All Done!~%")
  #+allegro        (excl:exit)
  #+sbcl           (sb-ext:quit)
LISP
} | ${LOGONROOT}/bin/logon --binary -I base -locale ja_JP.UTF-8
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
./description2xml.perl $ltypes > $outdir/$LINGUISTICS_FILE

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
    if [ -e $tb/$TB_FILE ]; then
	tbargs=$tbargs' '$tb/$TB_FILE
    elif  [ -e $tb/${TB_FILE}.gz ]; then
	tbargs=$tbargs' '$tb/${TB_FILE}.gz
    else
 	echo "Couldn't find a treebank at $tb"
    fi
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
dbhost=`hostname -f`
echo "charset=utf-8" > $CGI_DIR/params
echo "dbroot=$CGI_DIR" >> $CGI_DIR/params
echo "cssdir=http://$dbhost/~$USERNAME/ltdb/$version" >> $CGI_DIR/params
echo "cgidir=http://$dbhost/~$USERNAME/cgi-bin/$version" >> $CGI_DIR/params
echo "version=$version" >> $CGI_DIR/params
### trees
mkdir -p $HTML_DIR/trees

cp $lkbdir/src/tsdb/css/*.css  $HTML_DIR/.
cp $lkbdir/src/tsdb/js/*.js  $HTML_DIR/.

#
# Make the IndexPage
#

echo "<html><body><h1>Welcome to $version</h1>" > $HTML_DIR/index.html
echo "<ul>  <li>  <a href='../../cgi-bin/$version/list.cgi'>Lexical Type Database for $version</a>"  >> $HTML_DIR/index.html
if [ -n "$grammarurl" ]; then
echo "  <li>  <a href='$grammarurl'>Grammar Home Page</a>"  >> $HTML_DIR/index.html
fi
echo "  <li>  <a href='http://www.delph-in.net/'>DELPH-IN Network</a>"  >> $HTML_DIR/index.html
echo "  <li>  <a href='http://wiki.delph-in.net/moin/FrontPage'>DELPH-IN Wiki</a>"  >> $HTML_DIR/index.html
echo "  <li>  <a href='http://wiki.delph-in.net/moin/LkbLtdb'>Lexical Type Database Wiki</a></ul>"   >> $HTML_DIR/index.html
echo "<p>Created on $now</p>"  >> $HTML_DIR/index.html
echo "</html></body>" >> $HTML_DIR/index.html

### All done
echo
echo "Done: take a look at http://localhost/~$USERNAME/cgi-bin/$version/list.cgi"
echo

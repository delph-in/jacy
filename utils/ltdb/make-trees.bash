#!/bin/bash
#
# ToDo: parametrize special values (HEAD VALENCE CONTENT)
#

unset DISPLAY;
unset LUI;

lkbdir=/home/bond/logon/lingo/lkb

grammardir=/home/bond/svn/jacy; 
treebanks=`ls -d ${grammardir}/gold/*`
grammardir=/home/bond/svn/jp080901/jacy; 

outdir=/home/bond/svn/jacy/utils/ltdb/Jacy_2008-11-05
mkdir $outdir/trees


for tb in ${treebanks[@]} ; do
    if [ ! -e $tb/$TB_FILE ]; then
 	echo "Couldn't find  treebank at $tb"
 	exit
    fi
    tsdbhome=`dirname $tb`
    profile=`basename $tb`
    echo "Export trees from  $profile in $tsdbhome"
    { 
	cat 2>&1 <<- LISP
;  (load "$lkbdir/src/general/loadup")
;  (pushnew :lkb *features*)
;  (compile-system "tsdb" :force t)
  (lkb::read-script-file-aux  "$grammardir/lkb/script")
  (lkb::lkb-load-lisp "." "patch-html.lsp")
  (tsdb::tsdb :home "$tsdbhome")
  (tsdb::export-htmls "$profile" :path "$outdir/trees")
  (format t "~%All Done!~%")
  #+allegro        (excl:exit)
  #+sbcl           (sb-ext:quit)
LISP
    } | ${LOGONROOT}/bin/logon --binary -I base -locale ja_JP.UTF-8
done

HTML_DIR=/home/bond/public_html/ltdb/Jacy_2008-11-05/

cp -rp $outdir/trees $HTML_DIR/.

cp $lkbdir/src/tsdb/css/*.css  $HTML_DIR/.
cp $lkbdir/src/tsdb/js/*.js  $HTML_DIR/.

### All done
echo
echo "Done: take a look at http://localhost/~$USERNAME/cgi-bin/$version/list.cgi"
echo
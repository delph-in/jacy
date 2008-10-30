#!/bin/bash
# from the main grammar directory
unset DISPLAY;
unset LUI;

lkbdir=${LOGONROOT}/lingo/lkb;
grammardir=${LOGONROOT}/dfki/jacy;

### make input file
cut -d@ -f7 $grammardir/tsdb/skeletons/mrs/item |tail -50 > $grammardir/utils/mrs.50.txt

#
# CHEAP
#

# back up quick check file
mv $grammardir/pet/qc.tdl $grammardir/pet/qc.tdl.old

#flop the grammar once
cd $grammardir
flop japanese 

# calculate the quickcheck file
cat $grammardir/utils/mrs.txt | \
cheap -limit=50000 -packing -compute-qc=pet/qc.tdl $grammardir/japanese

# flop the grammar again
cd $grammardir
flop japanese

echo "PET done"
###
### LKB
###
# back up 
mv $grammardir/lkb/checkpaths.lsp $grammardir/lkb/checkpaths.lsp.old

### FIXME should redo the input file at somestage

{ 
 cat 2>&1 <<- LISP
  (load "$lkbdir/src/general/loadup")
  (compile-system "lkb" :force t)
  (lkb::read-script-file-aux  "$grammardir/lkb/script")
  ;;; set an edge limit
  (setf  lkb::*maximum-number-of-edges* '5000) 
  ;; make the checkpaths
  (lkb::with-check-path-list-collection  
   "$grammardir/lkb/checkpaths.lsp" 
    (lkb::parse-sentences 
    "$grammardir/utils/mrs.txt"
    "/tmp/mrs.txt.out"))
  (format t "~%All Done!~%")
  #+allegro        (excl:exit)
  #+sbcl           (sb-ext:quit)
LISP
} | ${LOGONROOT}/bin/logon --source -I base -locale ja_JP.UTF-8

echo "LKB done"

echo "please commit the new files"
echo "svn commit -m 'new quick check paths' pet/qc.tdl lkb/checkpaths.lsp"
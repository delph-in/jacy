#!/bin/bash

# from the main grammar directory
unset DISPLAY;
unset LUI;

if [ ! -d "$LOGONROOT/lingo/lkb" ]; then
    echo "LOGONROOT environment variable is not properly set!"
    echo "  see http://moin.delph-in.net/LogonInstallation"
    exit 1
fi

LKBDIR=${LOGONROOT}/lingo/lkb;
GRMDIR=$( cd "`dirname \"$0\"`/.." && pwd )

{
 cat 2>&1 <<- LISP
  (load "$LKBDIR/src/general/loadup")
  (lkb::read-script-file-aux  "$GRMDIR/lkb/script")
  (setf semi
    (mt::construct-semi       
     :ids t :rules t :descendp t :embedp t
     :semi (mt::read-semi
            "$GRMDIR/etc/matrix.smi"
            :includep nil :finalizep nil :recordp nil)
     :patches "$GRMDIR/etc/patches.lisp"
     :finalizep t))
  (with-open-file
      (stream "$GRMDIR/etc/hierarchy.smi"
       :direction :output :if-exists :supersede)
    (mt::print-semi semi :stream stream :format :hierarchy))
  (with-open-file
      (stream "$GRMDIR/etc/abstract.smi"
       :direction :output :if-exists :supersede)
    (mt::print-semi semi :stream stream :format :compact :filter "^[^_]"))
  (with-open-file
      (stream "$GRMDIR/etc/surface.smi"
       :direction :output :if-exists :supersede)
    (mt::print-semi semi :stream stream :format :compact :filter "^_"))
  #+allegro        (excl:exit)
  #+sbcl           (sb-ext:quit)
LISP
} | ${LOGONROOT}/bin/logon --binary -I base -locale ja_JP.UTF-8

echo "SEM-I created in $GRMDIR/etc/."

;;;
;;; call with 
;;;   time sbcl --dynamic-space-size 2000 --load make-qc.lisp
;;;   time env DISPLAY="" /home/bond/logon/franz/linux.x86.64/alisp -L make-qc.lisp
;;;
;;;
;;; use more memory
;(setf (sb-ext:BYTES-CONSED-BETWEEN-GCS) 200000000)

(load "/home/bond/delphin/lkb/src/general/loadup")
;(load "/home/bond/delphin/lkb.cvs.good/src/general/loadup")
;(compile-system "lkb" :force t)
(compile-system "lkb")
(defparameter grm-dir "/home/bond/logon/dfki/jacy")
;;;
;;; load jacy, with tdl lexicon
;;;
;(setf *features* (remove :psql *features*))
(read-script-file-aux 
 (format nil "~a/lkb/script" grm-dir))
(setf  lkb::*maximum-number-of-edges* '5000) 
;;;
;;; create the check-path
;;;
(lkb::with-check-path-list-collection 
 "/tmp/checkpaths" 
 (lkb::parse-sentences 
  (format  nil "~a/testsuites/kinou1.chasen.500" grm-dir)
  (format  nil "/tmp/kinou1.chasen.out" grm-dir)))

(format t "~%All Done!~%")
(quit)

;; sbcl 500 sentences
;; real    22m39.282s
;; user    22m20.332s
;; sys     0m16.841s
;;; Mean edges: 315.08
;;; Mean parses: 3.64

;; acl 500 sentences


;;; Total CPU time: 1082450 msecs
;;; Mean edges: 315.14
;;; Mean parses: 14.14
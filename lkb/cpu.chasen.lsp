(in-package :common-lisp-user)
;;;
;;; cpu definition for JACY as a slave cpu (parsing and generation)
;;;

;;
;; make sure we have sufficient space available
;;
#-:64bit
(system:resize-areas :old (* 256 1024 1024) :new (* 384 1024 1024))
#+:64bit
(system:resize-areas :old (* 640 1024 1024) :new (* 1024 1024 1024))
(setf (sys:gsgc-parameter :expansion-free-percent-new) 5)
(setf (sys:gsgc-parameter :free-percent-new) 2)
(setf (sys:gsgc-parameter :expansion-free-percent-old) 5)

(let* ((logon (system:getenv "LOGONROOT"))
       (lingo (namestring (parse-namestring (format nil "~a/lingo" logon)))))
  ;;
  ;; load MK defsystem() and LinGO load-up library first
  ;;
  (load (format nil "~a/lingo/lkb/src/general/loadup" logon))

  ;;
  ;; for parsing and generation, we need (close to) the full scoop
  ;;
  (unless (find-package :tsdb)
    (pushnew :lkb *features*)
    (pushnew :logon *features*)
    (pushnew :slave *features*)
    (excl:tenuring 
     (funcall (intern "COMPILE-SYSTEM" :make) "tsdb")))
  
  (funcall (symbol-function (find-symbol "INITIALIZE-TSDB" :tsdb))
           nil :rc (format nil "~a/dot.tsdbrc" logon))

  (excl:tenuring 
   (funcall 
    (intern "READ-SCRIPT-FILE-AUX" :lkb)
    (format nil "~a/dfki/jacy/lkb/script.chasen" logon))
   (funcall (intern "INDEX-FOR-GENERATOR" :lkb)))

  ;;
  ;; make sure we end up using the chasen(1) binary included in the LOGON tree
  ;;
  (set (intern "*CHASEN-DEBUG-P*" :lkb) nil)
  (set (intern "*CHASEN-APPLICATION*" :lkb) 
    (let* ((root (system:getenv "LOGONROOT"))
           (root (and root (namestring (parse-namestring root)))))
      (if root
        (format nil "~a/bin/chasen" root)
        "chasen")))
  
  ;;
  ;; allow the generator to relax post-generation MRS comparison, if need be
  ;;
  (set (intern "*BYPASS-EQUALITY-CHECK*" :lkb) :filter)
  
  ;;
  ;; i suspect none of these are needed, as effectively the values provided
  ;; from the calling process will override these.            (15-dec-05; oe)
  ;;
  (set (intern "*MAXIMUM-NUMBER-OF-EDGES*" :lkb) 5000)
  (set (intern "*UNPACK-EDGE-ALLOWANCE*" :lkb) 5000)
  (set (intern "*TSDB-MAXIMAL-NUMBER-OF-EDGES*" :tsdb) 5000)
  (set (intern "*TSDB-MAXIMAL-NUMBER-OF-RESULTS*" :tsdb) 100)
  (excl:gc :tenure) (excl:gc) (excl:gc t) (excl:gc)
  (setf (sys:gsgc-parameter :auto-step) nil)
  (set (intern "*TSDB-SEMANTIX-HOOK*" :tsdb) "mrs::get-mrs-string")
  (funcall (symbol-function (find-symbol "SLAVE" :tsdb))))

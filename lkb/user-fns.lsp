;;; Common user-fns file for all ESSLLI grammars

(in-package :lkb)

;;;
;;; determine surface order of constituents in rule: returns list of paths into
;;; feature structure of rule, i.e. (nil (args first) (args rest first)) for a
;;; binary rule, where the first list element is the path to the mother node of
;;; the rule.
;;;
(defun establish-linear-precedence (rule)
  (let ((daughters
         (loop
             for args = (existing-dag-at-end-of rule '(args))
             then (existing-dag-at-end-of args *list-tail*)
             for daughter = (when args 
                              (get-value-at-end-of args *list-head*))
             for path = (list 'args) then (append path *list-tail*)
             while (and daughter (not (eq daughter 'no-way-through)))
             collect (append path *list-head*))))
    (if (null daughters)
      (cerror "Ignore it" "Rule without daughters")
      (cons nil daughters))))

;;;
;;; detect rules that have orthographemic variation associated to them; those
;;; who do should only be applied within the morphology system; this version is
;;; a little complicated because we change from a full-form set-up to one with
;;; on-line morphology during the course.
;;;
(defun spelling-change-rule-p (rule)
  (rule-orthographemicp rule))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
  (declare (ignore rule))
  nil)
             

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. None of their descendents (if any) will be displayed either

(defun hide-in-type-hierarchy-p (type-name)
  (and (symbolp type-name)
       (search "GLBTYPE" (symbol-name type-name))))

;;;
;;; create feature structure representation of orthography value for insertion
;;; into the output structure of inflectional rules; somewhat more complicated
;;; than one might expect because of treatment for multi-word elements.
;;;
(defun make-orth-tdfs (orthography)
  (let* ((unifications
          (loop 
              for token in (split-into-words orthography)
              for path = *orth-path* then (append path *list-tail*)
              for opath = (create-path-from-feature-list 
                           (append path *list-head*))
              collect (make-unification :lhs opath                    
                                        :rhs (make-u-value :type token))))
         (indef (process-unifications unifications)))
    (when indef
      (make-tdfs :indef (create-wffs indef)))))


;;;
;;; characters dropped in preprocessing
;;; can be found out by typing in Lisp after #\!
;;;

;;; (ERB 2001-12-20) Colons are meaningful and we
;;; want to keep them!

(setf *punctuation-characters*
  '(#\space #\! #\" #\& #\' #\(
    #\) #\* #\+ #\, #\- #\. #\/ #\;
    #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^
    #\_ #\` #\{ #\| #\} #\~
    #\ideographic_full_stop #\fullwidth_question_mark 
    #\horizontal_ellipsis #\fullwidth_full_stop
    #\fullwidth_exclamation_mark 
    #\ideographic_space #\minus_sign
    #\white_circle #\black_circle
    #\white_star #\black_star
    #\white_diamond #\black_diamond))

;;;
;;; to generate `punctuation-characters' value for `japanese.set'
;;;
#+:null
(loop for c in *punctuation-characters* do (format t "~a" c))

;;;
;;; as of june 2008, the ChaSen wrapper code is now part of [incr tsdb()], so
;;; that it can be used in conjunction with PET even without JaCY itself.  the
;;; downside, however, is that ChaSen support is only available in conjunction
;;; with [incr tsdb()] then.                                   (17-jun-08; oe)
;;;
#+:(and :chasen :tsdb)
(defun preprocess-sentence-string (string)
  (tsdb::chasen-preprocess string))

;;;
;;; used in lexicon compilation for systems like PET and CHiC: when we compile
;;; out the morphology, there is no point in outputting uninflected entries for
;;; systems that have no on-line morphology.  also used in [incr tsdb()] for
;;; counting of `words'.
;;;
(defun dag-inflected-p (dag)           
  (let* ((key (existing-dag-at-end-of dag '(infl)))
         (type (and (dag-p key) (dag-type key))))
    (when type
      (or (eq type 'plus) (and (consp type) (eq (first type) 'plus))))))


;;
;; called in /lkb/src/io-paths/lexinput.lsp;lkb/src/io-paths/typeinput.lsp
;;
(defun set-temporary-lexicon-filenames nil
  (let* ((version (or (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)
                      (and (find-package :lkb)
                           (find-symbol "*GRAMMAR-VERSION*" :lkb))))
         (prefix
          (if (and version (boundp version))
            (remove-if-not #'alphanumericp (symbol-value version))
            "biglex")))
    (setf *psorts-temp-file* 
      (make-pathname :name (concatenate 'string prefix ".lex")
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *psorts-temp-index-file* 
      (make-pathname :name (concatenate 'string prefix ".idx") 
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *leaf-temp-file* 
      (make-pathname :name (concatenate 'string prefix ".lts")
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *predicates-temp-file* 
      (make-pathname :name (concatenate 'string prefix ".ric")
		     :host (pathname-host (lkb-tmp-dir))
		     :device (pathname-device (lkb-tmp-dir))
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *semantics-temp-file* 
      (make-pathname :name (concatenate 'string prefix ".stc")
		     :host (pathname-host (lkb-tmp-dir))
		     :device (pathname-device (lkb-tmp-dir))
                     :directory (pathname-directory (lkb-tmp-dir))))))


(defun gen-extract-surface (edge)
  (format nil "~{~a~^ ~}" (g-edge-leaves edge)))

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (setf *gen-extract-surface-hook* 'gen-extract-surface))


(defun bool-value-true (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type '+))))
  
(defun bool-value-false (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type '-))))


;;;
;;; Generate unknown words from the CARG
;;;
(defun instantiate-generic-lexical-entry (gle surface &optional (carg surface))
  (let ((tdfs (copy-tdfs-elements
               (lex-entry-full-fs (if (gle-p gle) (gle-le gle) gle)))))
    (loop
        with dag = (tdfs-indef tdfs)
        for path in '((STEM FIRST) (SYNSEM LKEYS KEYREL CARG))
        for foo = (existing-dag-at-end-of dag path)
        do (setf (dag-type foo) *string-type*))
    (let* ((surface(or
                    #+:logon
                    (case (gle-id gle)
                      (guess_n_gle 
                       (format nil "/~a/" surface))
                      (decade_gle
                       (format nil "~as" surface)))
                    surface))
           (unifications
            (list 
             (make-unification
              :lhs (create-path-from-feature-list
                    (append *orth-path* *list-head*))
              :rhs (make-u-value :type surface))
             (make-unification
              :lhs (create-path-from-feature-list
                    (append *orth-path* *list-tail*))
              :rhs (make-u-value :type *empty-list-type*))
             (make-unification
              :lhs (create-path-from-feature-list '(SYNSEM LKEYS KEYREL CARG))
              :rhs (make-u-value :type carg))))
           (indef (process-unifications unifications))
           (indef (and indef (create-wffs indef)))
           (overlay (and indef (make-tdfs :indef indef))))
      (values
       (when overlay
        (with-unification-context (ignore)
          (let ((foo (yadu tdfs overlay)))
            (when foo (copy-tdfs-elements foo)))))
       surface))))


;;;
;;; Idiom Implementation (CH 060804)
;;;
;;; try a new approach to post-parsing filtering of idioms, building on the new
;;; MRS transfer machinery.  essentially, the idiom phrases have been recast as
;;; MRS transfer rules (MTRs), each of them matching an idiom configuration 
;;; and replacing the idiomatic parts of the MRS with a synthesized relation 
;;; (or nothing, for the time being).  post-transfer, the filter can then just
;;; require that no idiomatic relation remain.  (20-feb-05; dan & oe phx - sfo)
;;;
(defun idiom-complete-p (tdfs)
  (let* ((mrs (and (tdfs-p tdfs)
                   (mrs::extract-mrs-from-fs (tdfs-indef tdfs))))
         (transfers (and (mrs::psoa-p mrs)
                         (mt:transfer-mrs mrs :task :idiom))))
    (loop
        for transfer in transfers
        for mrs = (mt::edge-mrs transfer)
        thereis (loop
                    for ep in (mrs:psoa-liszt mrs)
                    when (idiom-rel-p ep) return nil
                    finally (return t)))))

(eval-when #+:ansi-eval-when (:load-toplevel :execute)
	   #-:ansi-eval-when (load eval)
  (setf *additional-root-condition* #'idiom-complete-p))


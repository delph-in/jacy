;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; course grammar specific globals file
;;; parameters only - grammar specific functions 
;;; should go in user-fns.lsp

(setf excl:*locale* (excl:find-locale "japan.EUC"))

(defparameter *active-parsing-p* t)

;;; Types

(in-package :lkb)

(defparameter *toptype* '*top*)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;Chart reduction code from Oe, 2002/20/25

(defparameter *chart-dependencies*
    '((SYNSEM LOCAL KEYS --+COMPKEY) (SYNSEM LOCAL KEYS KEY)
      (SYNSEM LOCAL KEYS --+OCOMPKEY) (SYNSEM LOCAL KEYS KEY)))

;;; Lexical files


(defparameter *orth-path* '(orth list))

;(defparameter *orth-path* '(phon first stem))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(defparameter *empty-list-type* 'null)
;;(def-lkb-parameter *empty-list-type* 'null)

(defparameter *list-type* 'list)
;;(def-lkb-parameter *list-type* 'list)

(def-lkb-parameter *non-empty-list-type* 'ne-list)

(defparameter *diff-list-type* 'diff-list)
;;(def-lkb-parameter *diff-list-type* 'diff-list)

(defparameter *diff-list-list* 'list)
;;(def-lkb-parameter *diff-list-list* 'list)

(defparameter *diff-list-last* 'last)
;;(def-lkb-parameter *diff-list-last* 'last)

(defparameter *lex-rule-suffix* "-infl-rule"
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

(defparameter *irregular-forms-only-p* t)

;;;
;;; input characters to be ignored (i.e. suppressed) in tokenization
;;;
(defparameter *punctuation-characters*
  (append
   '(#\space #\! #\" #\& #\' #\(
     #\) #\* #\+ #\, #\- #\. #\/ #\;
     #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^
     #\_ #\` #\{ #\| #\} #\~)
   #+:ics
   '(#\ideographic_full_stop #\fullwidth_question_mark 
     #\horizontal_ellipsis #\fullwidth_full_stop
     #\fullwidth_exclamation_mark #\black_circle
     #\fullwidth_comma #\ideographic_space
     #\katakana_middle_dot #\white_circle)))

 ;;;

(defparameter *display-type-hierarchy-on-load* nil)

;;; Parsing

(defparameter *maximum-number-of-edges* 10000)

(defparameter *chart-limit* 100)

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

;for spoken and email language:

;(defparameter *start-symbol* '(utterance-root np-frag-root pp-frag-root)
;   "specifing valid parses")

;for written language better:

(defparameter *start-symbol* '(utterance-root)
   "specifing valid parses")

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

(defparameter *deleted-daughter-features* 
  '(ARGS HEAD-DTR NON-HEAD-DTR DTR)
  "features pointing to daughters deleted on building a constituent")

(defparameter *rule-keys*
  '((HEAD-ADJUNCT-RULE1 . 1)
    (COMPOUNDS-RULE . 1)
    (KARA-MADE-RULE . 2) 
    (HEAD_SUBJ_RULE . 2)
    (HEAD-SPECIFIER-RULE . 2)
    (HEAD-COMPLEMENT-RULE . 2) 
    (HEAD-COMPLEMENT2-RULE . 2)
    (HEAD-ADJUNCT-RULE2 . 2)))

;;; Parse tree node labels

;;; the path where the name string is stored
(defparameter *label-path* '(LABEL-NAME))

;;; the path for the meta prefix symbol
(defparameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(defparameter *suffix-path* '(META-SUFFIX))

;;; the path for the recursive category
(defparameter *recursive-path* '(SYNSEM NON-LOCAL SLASH LIST FIRST))

;;; the path inside the node to be unified with the recursive node
(defparameter *local-path* '(SYNSEM LOCAL))

;;; the path inside the node to be unified with the label node
(defparameter *label-fs-path* '())
;;(defparameter *label-fs-path* nil)

(defparameter *label-template-type* 'label)

;;;
;;; enable (mostly) YY-specific recording of surface position in MRS relations
;;;
;;;(defparameter *relation-label-path* '(SYNSEM LOCAL CONT KEY WLINK))
(defparameter *relation-label-path* nil)

;;;
;;; put CDB into multi-byte mode
;;;
(defparameter cdb::*cdb-ascii-p* nil)

;;; for the compare function 

(defparameter *discriminant-path* '(synsem local keys key pred))

(setf *semantics-index-path* '(synsem local cont hook index))

;;; connection parameters for lexical database, an association list with fields
;;; `:host', `:db', `:table', and `:user' (optional) 
;;; if unset we fall back to .tdl lexicon files

(defparameter *psql-lexicon-parameters* 
  #+:psql
  '((:db "jap") (:host "localhost") (:table "erg"))
  #-:psql
  nil)


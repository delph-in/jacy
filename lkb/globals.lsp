;;; Copyright (c) 1991--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `license' for conditions.

;;;
;;; Grammar specific globals file
;;; parameters only - grammar specific functions 
;;; should go in user-fns.lsp

(grammar-encoding 'utf-8)

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

;(defparameter *orth-path* '(orth list))
;(defparameter *orth-path* '(phon first stem))
(defparameter *orth-path* '(stem))

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

(defparameter *lex-rule-suffix* nil
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

(defparameter *irregular-forms-only-p* t)

(defparameter *display-type-hierarchy-on-load* nil)

;;; Parsing

(defparameter *maximum-number-of-edges* 10000)

(defparameter *chart-limit* 100)

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *active-parsing-p* t)

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

;;; for the compare function 
(defparameter *discriminant-path* '(SYNSEM LKEYS KEYREL PRED))

;;;
;;; put lexicon CDB into multi-byte mode
;;;
(defparameter cdb::*cdb-ascii-p* nil)

(setf *semantics-index-path* '(synsem local cont hook index))

;;;
;;; while debugging the generator, turn ambiguity packing off; equate QEQs to
;;; take advantage of handle constraints generator internally, this requires 
;;; that the qeq type in the grammar identifies the Skolem constants of the two
;;; variables, e.g.
;;;
;;;   qeq := avm &
;;;   [ HARG handle & [ INSTLOC #instloc ],
;;;     LARG handle & [ INSTLOC #instloc ] ].
;;;
;;; once generation develops a little further, turn packing back on and find a
;;; suitable restrictor (suppressing orthography and pieces of semantics that
;;; will never fail, e.g. RELS and maybe HCONS).               (25-dec-04; oe)
;;;
(setf *gen-packing-p* nil)
(setf *gen-equate-qeqs-p* t)

;;;
;;; make generation faster
;;;
(setf *gen-packing-p* t)
(setf *gen-filtering-p* t)
(setf *packing-restrictor*  '(RELS HCONS STEM RULE-NAME))

;;;
;;; list of things not to generate even though they maybe contentful
;;;

(setf *duplicate-lex-ids* 
  '(;; s-end1-decl-lex - emphatic sentence enders
    ga-sap keredomo-send kedomo-send ga-sap kedo-send shi-send 
    yo-2 yo-3 keredo-send exclamation-mark ze zo zo-2 
    ;; s-end1-decl-minusahon-lex - emphatic sentence enders
    i-emp
    ;;; question endings
    no-send kai-sap na-ne kai-chasen-sap nokai-sap
    ;;; variants of why
    naze-kanji-adv nande-adv nande-kanji-multi-adv nande-multi-adv doushite-adv
    ;; subjunctive ends (should do with mood)
    darou-v-cop-lex deshou-v-cop-lex
    ;; various copulars
    kanaa-cop-id-lex kai-cop-id-lex kashira-cop-id-lex degozaru-cop-id-multi
    nano-cop-id-multi nan-cop-id-multi naNdesu-cop-multi
    nandesu-cop-id-multi nanodesu-cop-id-multi nanodesu-cop-id-multi-2
    ;; variant forms of numbers (hankaku)
    zero_card_a one_card_a two_card_a three_card_a four_card_a 
    five_card_a six_card_a seven_card_a eight_card_a nine_card_a 
    ;; variant forms of numbers (zenkaku)
    zero_card one_card two_card three_card four_card  
    five_card six_card seven_card eight_card nine_card 
    ;;; indefinite pronouns FIXME - improve semantics
    donna douiu dono-det
    ;; variants of me
    atakushi atashi boku boku-kanji boku_3 onore_1_2 ore ore-firstsg_katakana 
    ore-kanji oresama-firstsg oresama-firstsg_chasen sei_10 shousei temae_2_1 
    uchi-kanji-pron uchi-pron ware_1_3 ware_6 washi-firstsg watakushi watashi yasei_1_3 
    ;; variants of you
    anata-kanji anta-pron kimi-pron kimi-pron-hiragana omae_1_hiragana omae_1_kanji 
    onmi_1 onoono_1_1 onore_2 otaku_3 socchi sochira_2 temee_1_2 ware_2_1 ware_5 
    ;; variants of youse
    anatatachi-b anatatachi-c kimitachi kimitachi-b kimitachi-c 
    ;; variants of wa: wa-narg
    tte-narg nanowa 
    colon-advp colon-advp-2 comma-advp
    ;; →
    arrow-postp
    ;;  第  FIXME - improve semantics
    dai-card2ord
    ;; who
    dochirasama-multi dare-hiragana donata_1
    ;; aru/iru
    aru-kanji-stem aru-kanji-stem-2 iru_be-2-stem
    ;; aru/iru aux
    iru-aux-kanji-stem aru-kanji-aux-stem
    ;; nai/nu
    nai-no-case-adj-kanji nu-end
    ;; da/dearu/desu: da-v-cop-id-stem 
    desu-v-cop-id-stem dearu-v-cop-id-stem 
    ;; koto
    koto-pred-kanji
    ;;; Lexical things here!
    kirei_1_2 kirei_1_3 ;  綺麗　奇麗　
    iya-katakana-gg-adj iya-kanji-gg-adj ;  嫌 いや
    dekiru_2				; dekiru
    furu_1-hiragana			; furu
    tabako_1_1 tabako_1_2		; tabako
    furui_2_2				; furui
    akiraka_hiragana			; akiraka
    mieru-hiragana-stem			; mieru
    shizuka_1-hiragana			; shizuka
    ;;; week days
    ka_dofw kayou			; kayou / kayoubi 
    ))

;;;
;;; with recent LKB versions (as of 23-jul-05), there is now better support for
;;; the (still primitive) `remote' generation mode: a `translation grid' can be
;;; configured from any number of LKB processes, each potentially prepared to
;;; act as a generator server.  the following, for example:
;;;
;;;  (setf *translate-grid* '(:ja . (:ja)))
;;;
;;; indicates that we can act as a generator server for japanese ourselves and
;;; will send of generation requests (from selection `Rephrase' on the parse
;;; summary view or `Generate' on the LOGON MRS browser) to a japanese server,
;;; i.e. ourselves.  likewise,
;;;
;;;   (setf *translate-grid* '(:ja . (:ja :en :no)))
;;;
;;; will send requests to three servers, which is something emily has long
;;; wanted (using an array of Matrix grammars and an interlingua semantics).
;;;
(setf *translate-grid* '(:ja . (:ja)))

(setf *gen-ignore-rules* '(head-complement2-rule))

;;; connection parameters for lexical database, an association list with fields
;;; `:host', `:db', `:table', and `:user' (optional) 
;;; was for me (MS):   '((:db "jacy") (:host "localhost") (:table "jacy") (:semi t) (:user "siegel"))
;;; if unset we fall back to .tdl lexicon files

;(defparameter *lexdb-params* 
;bmw!!!
					;  #+:psql
;  '((:db "jacy") (:host "localhost") (:table "jacy") (:semi t))
;  #-:psql
;  nil)


;;;
;;; Idiom Implementation (CH 060804)
;;;
(defparameter *non-idiom-root*
    'root_non_idiom )

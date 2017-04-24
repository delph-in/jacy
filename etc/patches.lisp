
;;; MWG 2017-04-22: copied from ${ERG}/etc/patches.lisp
;;;
;;; we must be careful not to rename any predicates that are used in grammar
;;; entities (lexical entries or rules), because these provide the link from
;;; the pieces of an input semantics to realization to the intial generator
;;; chart.
;;; 
;;; + alias predicate type
;;; + block predicate
;;; + link type type
;;; + parent predicate predicate
;;;
(
 ; (:block "be_v_prd-or-id")
 ; (:block "be_v_prd-or-nv")
 ; (:block "card")
 ; (:block "comp")
 ; (:block "dofm")
 ; (:block "every_q")
 ; (:block "interval_p_end")
 ; (:block "numbered_hour")
 ; (:block "person_name")
 ; (:block "plus")
 ; (:block "some_q")
 ; (:block "temp")
 ; (:block "time_n")
 ; (:block "times")
 ; (:block "v_event")
 ; (:alias "existential_q" def_udef_some_a_q_rel)
 ; (:link def_or_demon_q_rel def_udef_some_a_q_rel)
 ; (:link def_or_proper_q_rel def_udef_some_a_q_rel)
 ; (:link basic_free_relative_q_rel def_udef_some_a_q_rel)
 ; (:parent "_all_q" "universal_q")
 ; (:parent "_each_q" "universal_q")
 ; (:parent "every_q" "universal_q")
 ; (:parent "_both_q" "universal_q")
 ; (:parent "_either_q" "universal_q")
 ; (:parent "_across_p_temp" "_across_p")
 ; (:parent "_at_p_temp" "_at_p")
 ; (:parent "_by_p_temp" "_by_p")
 ; (:parent "_in_p_temp" "_in_p")
 ; (:parent "_on_p_temp" "_on_p")
)

#+:null
;; MWG 2017-04-22: changed absolute directories to relative, also
;;   matrix.smi corresponds to the ERG's erg.smi, so I load that
;;   for the initial construction instead of jacy.smi (because
;;   :includep nil prevents it from loading, e.g., core.smi, but
;;   also matrix.smi). Alternatively, we could make a separate
;;   jacy-core.smi that is included by jacy.smi and includes
;;   matrix.smi (and potentially defines JACY-specific stuff),
;;   then use jacy-core.smi for the initial construction, with
;;   :includep t.
(progn
  (setf semi
    (mt::construct-semi       
     :ids t :rules t :descendp t :embedp t
     :semi (mt::read-semi
            "etc/matrix.smi"
            :includep nil :finalizep nil :recordp nil)
     :patches "etc/patches.lisp"
     :finalizep t))
  (with-open-file
      (stream "etc/hierarchy.smi" :direction :output :if-exists :supersede)
    (mt::print-semi semi :stream stream :format :hierarchy))
  (with-open-file
      (stream "etc/abstract.smi" :direction :output :if-exists :supersede)
    (mt::print-semi semi :stream stream :format :compact :filter "^[^_]"))
  (with-open-file
      (stream "etc/surface.smi" :direction :output :if-exists :supersede)
    (mt::print-semi semi :stream stream :format :compact :filter "^_"))
)

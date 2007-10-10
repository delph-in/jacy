;;;
;;; local patches
;;;
;;;
(in-package :lkb)
(defmethod create-unnormalized-missing-lex-keys ((lex su-psql-lex-database))
  (loop
      for rec in
	(get-raw-records lex 
			 (format nil (create-unnormalized-missing-lex-keys3-FSQL lex)
				 (orth-field lex)))
      for orth-list = (string-2-str-list (second rec))
      if (= 1 (length orth-list))
      collect rec
      else
      append 
      (loop for word in orth-list
	  collect (list (first rec)  word))))

#+:tsdb
(in-package :tsdb)
;;  (lkb::lkb-load-lisp "/home/bond/delphin/grammars/japanese/lkb/" "patches.lsp")

;; (tsdb::export-htmls tsdb::*tsdb-data* :condition "i-id < 100" :path "/tmp")
;;
;; ToDo:
;;  get features working
;;  links to text (ala HoG)?
;;  links to derivation trees (ala Rebecca)
;;  set encoding dynamically (or even better, always convert to utf-8)
;;
;;  create an index.html
;;  copy lkb.css lkb.js to the directory (from delphin/lkb/src/www)
;;
#+:tsdb
(defun export-htmls (data &key (condition *statistics-select-condition*)
                               path prefix interrupt meter 
                               ;(compressor "gzip -c -9") (suffix "html.gz")
			       (compressor "cat") (suffix "html")
                               (stream *tsdb-io*))
  
  (loop
    ;#+(and :allegro-version>= (version>= 6 0))
    ;(setf (stream-external-format stream) (excl:find-external-format :utf-8))
      with offset = 0
      with target = (format 
                     nil 
                     "~a/~a"
                     (or path "/lingo/oe/tmp") (directory2file data))
      with lkb::*chart-packing-p* = nil
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with items = (analyze
                    data :thorough '(:derivation :mrs)
                    :condition condition :commentp t)
      with increment = (when (and meter items)
                         (/ (- (get-field :end meter) (get-field :start meter))
                            (length items) 1))
      with gc-strategy = (install-gc-strategy 
                          nil :tenure *tsdb-tenure-p* :burst t :verbose t)

      initially
        #+:allegro (ignore-errors (mkdir target))
        (when meter (meter :value (get-field :start meter)))
      for item in items
      for i-wf = (get-field :i-wf item)
      for input = (or (get-field :o-input item) (get-field :i-input item))
      for i-comment = (get-field :i-comment item)
      for parse-id = (get-field :parse-id item)
      for results = (let ((results (get-field :results item)))
                      (sort (copy-list results) #'< 
                            :key #'(lambda (foo) (get-field :result-id foo))))
      for trees = (select '("t-active" "t-version") '(:integer :integer) 
                          "tree" 
                          (format nil "parse-id == ~a" parse-id) 
                          data)
      for version = (when trees
                      (loop
                          for tree in trees
                          maximize (get-field :t-version tree)))
      for active = (when version
                     (let ((foo (select '("result-id") '(:integer) 
                                        "preference" 
                                        (format 
                                         nil 
                                         "parse-id == ~a && t-version == ~d" 
                                         parse-id version) 
                                        data)))
                       (loop 
                           for bar in foo 
                           collect (get-field :result-id bar))))
      for file = (format 
                  nil 
                  "~a/~@[~a.~]~d~@[.~a~]" 
                  target prefix (+ parse-id offset) suffix)
      when results do
        (format 
         stream 
         "[~a] export-htmls(): [~a] ~a active tree~:[~;s~] (of ~d) ~S.~%" 
         (current-time :long :short)
         (+ parse-id offset)
         (if version (length active) "all")
         (or (null version) (> (length active) 1))
         (length results) input)
        (clrhash *reconstruct-cache*)
        
        #+:allegro
	(multiple-value-bind (stream foo pid)
	    (run-process
	     compressor :wait nil :input :stream
	     :output file :if-output-exists :supersede
	     :error-output nil)
          (declare (ignore foo #-:allegro pid))
	  (format stream 
		  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%")
	  (format stream  "<html>~%<head>~%")
	  (format stream  "<meta>~%")
	  (format stream  "<meta http-equiv=\"Content-Type\" content=\"text/html ; charset=utf8\">~%")
	  (format stream  "<link TYPE=\"text/css\" REL=\"stylesheet\" HREF=\"lkb.css\">~%")
	  (format stream  "<script SRC=\"lkb.js\" LANGUAGE=\"javascript\" TYPE=\"text/javascript\"></script>~%")
	  (format stream  "</meta>~%")
	  (format stream  "<title>~a: ~a</title>~%</head>~%"
		  (+ parse-id offset) input)
	  (format stream  "<body>~%")
	  (format 
	   stream
	   "<h1>~d: ~a ~@[(~a)~]</h1>~%"
	   (+ parse-id offset)
	   input i-comment)
	  (format 
	   stream
	   "<h2>(~a of ~d) {~d}~%"
	   (if version (length active) "all") (length results) i-wf)
	  (export-html item active :offset offset :stream stream)
	  (unless *redwoods-thinning-export-p*
	    (export-html
             item active :complementp t :offset offset :stream stream))
	  (format stream  "<pre>Redwoods export of `~a' (~a@~a, ~a)</pre>~%"
	   data (current-user) (current-host) (current-time :long :pretty))
	  (format stream "</body>~%</html>~%")
          (force-output stream)
          (close stream)
          (sys:os-wait nil pid))
        (when increment (meter-advance increment))
      when (interrupt-p interrupt) do
        (format 
         stream
         "[~a] export-htmls(): external interrupt signal~%"
         (current-time :long :short))
        (force-output stream)
        (return)
      finally
        (when meter (meter :value (get-field :end meter)))
        (when gc-strategy (restore-gc-strategy gc-strategy))))





#+:tsdb
(defun export-html (item active 
                    &key complementp (offset 0) (stream *tsdb-io*))
  #+:debug
  (setf %item% item %active% active)
  (loop
      with *package* = (find-package :lkb)
      with lkb::*deleted-daughter-features* = 
        (if (or (eq *redwoods-export-values* :all)
                (smember :avm *redwoods-export-values*))
          nil
          lkb::*deleted-daughter-features*)
      with i-input = (get-field :i-input item)
      with i-id = (get-field :i-id item)
      for input = (or (get-field :o-input item) (get-field :i-input item))
      with i-comment = (get-field :i-comment item)
      with parse-id = (get-field :parse-id item)
      with results = (get-field :results item)
      for i from 1
      for result in results
      for result-id = (get-field :result-id result)
      for derivation = (when (if complementp
                               (not (member result-id active :test #'eql))
                               (member result-id active :test #'eql))
                         (get-field :derivation result))
      for edge = (and derivation (reconstruct derivation))
      for tree = (when (and edge
                            (or (eq *redwoods-export-values* :all)
                                (smember :tree *redwoods-export-values*)))
                   (let ((tree (ignore-errors
                                (lkb::parse-tree-structure edge))))
                     (unless tree
                       (format 
                        stream 
                        "[~a] export-htmls(): [~a] ~
                         error() labeling tree # ~a.~%" 
                        (current-time :long :short)
                        (+ parse-id offset)
                        result-id))
                     tree))
      for dag = (and edge (let ((tdfs (lkb::edge-dag edge)))
                            (and (lkb::tdfs-p tdfs)
                                 (lkb::tdfs-indef tdfs))))
      for mrs = (or (get-field :mrs result)
                    (and edge (mrs::extract-mrs edge)))
      for ident = (format nil "~a @ ~a~@[ @ ~a~]" i-id result-id i-comment)
      when (zerop (mod i 100)) do (clrhash *reconstruct-cache*)
      when (or dag mrs) do
        (format 
         stream 
         "<h3>[~d:~d] ~:[(active)~;(inactive)~]</h3>~%" 
         (+ parse-id offset) result-id complementp)
        (setf lkb::*cached-category-abbs* nil)
	;;
	;; HTML
	;;
	;; Caption
	(format stream "<table><caption>~a</caption>" 
		input)
	;; Tree
	;(format stream "<tr>~%")
	;(format stream "<td class=resultsTree >~%")
	;(lkb::html-tree edge :stream stream :indentation 4)
	;(format stream "~%")
	;(format stream "</td>~%")
	;; MRS
	(format stream "<td class=resultsMrs>~%")
	(mrs::output-mrs1 mrs 'mrs::html  stream)
	(format stream "~%")
	(format stream "</td>~%")
	(format stream "</tr></table>~%")
        ;; Derivation
        (when (or (eq *redwoods-export-values* :all)
                  (smember :derivation *redwoods-export-values*))
          (let ((*package* (find-package :tsdb)))
	    (format stream "<pre>~%")
            (format stream "~s~%~%~%" derivation)
	    (format stream "</pre>~%")))
	;; Dependency
        (when (or (eq *redwoods-export-values* :all)
                  (smember :dependencies *redwoods-export-values*))
          (ignore-errors (format stream "<pre>~%")
			 (mrs::ed-output-psoa mrs :stream stream)
			 (format stream "</pre>~%")))
	))
;;;
;;; make finding equivalences more efficient
;;; 
(defun semantic-equivalence (data &key condition (file "/tmp/equivalences"))
  (loop
      with stream = (open file :direction :output :if-exists :supersede)
      with lkb::*chart-packing-p* = nil
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with items = (analyze data :thorough '(:derivation) 
                            :condition condition :readerp nil)
      for item in items
      for i-id = (get-field :i-id item)
      for input = (or (get-field :o-input item) (get-field :i-input item))
      for results = (nreverse (copy-list (get-field :results item)))
      do
        (clrhash *reconstruct-cache*)
        (format t "~a: [~a] `~a'~%" i-id (length results) input)
        (format stream "~a: [~a] `~a'~%" i-id (length results) input)
        (loop
            with *package* = (find-package :lkb)
            for result in results
            for derivation = (get-field :derivation result)
            for edge = (when derivation (reconstruct derivation))
            for id = (when edge (lkb::edge-id edge))
            for mrs = (when edge (mrs::extract-mrs edge))
            do (nconc result (pairlis '(:id :mrs) (list id mrs))))
        (loop
            for result = (pop results)
            for id1 = (get-field :id result)
            ;;; unless id1 found
            for mrs1 = (get-field :mrs result)
            while result do
              (format stream "~a:"id1)
              (loop
                  for foo in results
                  for id2 = (get-field :id foo)
                  for mrs2 = (get-field :mrs foo)
		  ;do (format t "Compare ~a : ~a~%" id1 id2)
                  when (apply #'mrs::mrs-equalp mrs1 mrs2 '(t nil)) do
		    ;;; remove the identical result
	;	    (pprint results)
		    (delete foo results)
;		    (pprint results)
                    (format stream " ~a" id2)
;                    (format t "~a == ~a~%" id1 id2)
		    )
              (format stream "~%"))
        (format stream "~a~%" #\page)
      finally (close stream)))

;;;
;;; Type output
;;;
(in-package :lkb)

;;;
;;; Output information on lexical rules for lexical type DB.
;;;  (lkb::lrules-to-xml :file "/tmp/lrules.xml")

#+:tsdb
(defun lrules-to-xml  (&key (stream t) file)
  (let ((stream (if file
		    (open file
			  :direction :output :if-exists :supersede
			  :if-does-not-exist :create)
                  stream)))
    
    (loop
        for rule being each hash-value in *lexical-rules*
        for id = (rule-id rule)
        for tdfs = (rule-full-fs rule)
        for type = (and tdfs (indef-type-of-tdfs tdfs))
        when (and id type) do
          (format
           stream
           "<instance name=\"~(~a~)\" type=\"~(~a~)\" status=\"~a\" cat=\"~(~a~)\" val=\"~(~a~)\" cont=\"~(~a~)\"/>~%"
           id type (if (inflectional-rule-p id) "irule" "lrule")
	   (lkb::dag-type (mrs::path-value (tdfs-indef tdfs) 
					   '(SYNSEM LOCAL CAT HEAD)))
	   (lkb::dag-type (mrs::path-value (tdfs-indef tdfs) 
					   '(SYNSEM LOCAL CAT VAL)))
	   (lkb::dag-type (mrs::path-value (tdfs-indef tdfs) 
					   '(SYNSEM LOCAL CONT)))))
    (when file (close stream))))

;;;
;;; added xml as a type
;;; (lkb::output-types :xml "/tmp/jp-types.xml")
;;;
(defun output-types (syntax &optional file-name)
  (unless (member syntax '(:tdl :path :lilfes :xml))
    (error "Unsupported syntax specifier ~A" 
           syntax))
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (ecase syntax
	(:xml (format ostream "<types>~%")))
      (loop for type-name in *type-names*
;			      py-list
;                          ; remove unaccessed leaf types
;                          (remove-if-not 
;                           #'(lambda (x) (get-type-entry x))
;			   *types*)))
      ;;      (append *ordered-type-list*
      ;;	      *ordered-glbtype-list*))))
	  do
	    (let ((entry (get-type-entry type-name)))                  
             (ecase syntax
               (:tdl (output-type-as-tdl type-name entry
                                         ostream))
               (:path (output-type-as-paths type-name entry
                                         ostream))
               (:lilfes (output-type-as-lilfes type-name entry
                                               ostream))
	       (:xml  
		(output-type-as-xml type-name entry
				    ostream)))))
      (ecase syntax
	(:xml (format ostream "</types>~%"))))))
  

#+:tsdb
(defun output-type-as-xml (name type-struct stream)
 (let* ((status (assoc name *tdl-status-info*))
        (def (ltype-local-constraint type-struct))
        (parents (ltype-parents type-struct))
	(children (ltype-daughters type-struct))
	(tdfs (ltype-tdfs type-struct))
	(synsem (mrs::path-value 
	      (tdfs-indef tdfs) '(SYNSEM)))
	(cat (mrs::path-value 
	      (tdfs-indef tdfs) '(SYNSEM LOCAL CAT HEAD)))
	(kkey (mrs::path-value 
	      (tdfs-indef tdfs) '(SYNSEM LOCAL CAT HEAD KEYS KEY)))
	(val (mrs::path-value 
	      (tdfs-indef tdfs) '(SYNSEM LOCAL CAT VAL)))
	(cont (mrs::path-value 
	      (tdfs-indef tdfs) '(SYNSEM LOCAL CONT))))
   (format stream "~%<type name=\"~(~a~)\" "  name)
   (format stream "~%      parents=\"")
   (loop for parent in parents
	 for i from 1
	 when (> i 1)
	 do 
	 (format stream " "); separate with spaces
	 do
	 (format stream "~(~a~)" parent))
   (format stream "\"")
   (if children 
       (progn
	 (format stream "~%      children=\"")
	 (loop for child in children
	       for i from 1
	       when (> i 1)
	       do 
	       (format stream " "); separate with spaces
	       do
	       (format stream "~(~a~)" child))
	 (format stream "\"")))
   (if synsem 
       (format stream "~%      synsem=\"~(~a~)\"" (lkb::dag-type synsem)))
   (if kkey 
       (format stream "~%      keys.key=\"~(~a~)\"" (lkb::dag-type kkey)))
   (if cat 
       (format stream "~%      cat=\"~(~a~)\"" (lkb::dag-type cat)))
  (if val 
       (format stream "~%      val=\"~(~a~)\"" (lkb::dag-type val)))
  (if cont 
       (format stream "~%      cont=\"~(~a~)\"" (lkb::dag-type cont)))

					;   (format stream "~%          val=\"~(~a~)\"" 
;	   (lkb::dag-type (mrs::path-value (tdfs-indef tdfs)
;					   '(SYNSEM LOCAL CAT VAL))))
;   (format stream "~%          cont=\"~(~a~)\"" 
;	   (lkb::dag-type (mrs::path-value (tdfs-indef tdfs)
;					   '(SYNSEM LOCAL CONT))))
   (format stream ">")
   ;;; print escaped TDL
;;    (format stream (lkb::xml-escape 
;; 		   (with-output-to-string (plain-tdl)
;; 		     (output-type-as-tdl name type-struct plain-tdl))))
   ;;; print  TDL as CDATA
   (format stream "~%<![CDATA[")
   (output-type-as-tdl name type-struct stream)
   (format stream "~%]]>")
   ;; close XML
   (format stream "~%</type>~%")))
;;;
;;; parse selection with a model
;;;
(in-package :lkb)
#+:tsdb
(defun tsdb::parse-item (string
                  &key id exhaustive nanalyses trace
                       edges derivations semantix-hook trees-hook
                       filter burst (nresults 0))
 (declare (ignore id derivations filter))

 (let* ((*package* *lkb-package*)
        (*chasen-debug-p* nil)
        (*maximum-number-of-edges* (if (or (null edges) (zerop edges))
                                     *maximum-number-of-edges*
                                     edges))
        (*first-only-p* (if (or exhaustive *chart-packing-p*)
                          nil
                          (if (integerp nanalyses)
                            (or (zerop nanalyses) nanalyses)
                            (if (integerp *first-only-p*) *first-only-p* 1))))
        (*do-something-with-parse* nil))
   (declare (special *chasen-debug-p*))
   (multiple-value-bind (return condition)
     (#-:debug ignore-errors #+:debug progn
      (let* ((sent
              (split-into-words (preprocess-sentence-string string)))
             (str (make-string-output-stream)) ; capture any warning messages
             (*standard-output*
              (if trace
                (make-broadcast-stream *standard-output* str)
                str))
             tgc tcpu treal conses symbols others)
        ;;
        ;; this really ought to be done in the parser ...  (30-aug-99  -  oe)
        ;;
        (setf *sentence* string)
        (reset-statistics)
        (multiple-value-bind (e-tasks s-tasks c-tasks f-tasks m-tasks)
            (tsdb::time-a-funcall
             #'(lambda () (parse-tsdb-sentence sent trace))
             #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                (declare (ignore ignore))
                (setq tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                      conses (* scons 8) symbols (* ssym 24) others sother)))
         (declare (ignore m-tasks))
         (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                (packingp *chart-packing-p*)
                (output (get-output-stream-string str))
                (unifications (statistics-unifications *statistics*))
                (copies (statistics-copies *statistics*))
                utcpu
                utgc
                uspace
                (readings (if packingp
                            (tsdb::time-a-funcall
                             #'(lambda ()
                                 ;;
                                 ;; _fix_me_
                                 ;; this should really go into the parser, but
                                 ;; just now patch things up for francis, so
                                 ;; he can submit something to TMI.
                                 ;;                            (11-may-07; oe)
                                 (setf *parse-record*
                                   (selectively-unpack-edges
                                    *parse-record* nanalyses))
                                 (length *parse-record*))
                             #'(lambda (tgcu tgcs tu ts tr scons ssym sother
                                        &rest ignore)
                                 (declare (ignore tr ignore))
                                 (setf utcpu (- (+ tu ts) (+ tgcu tgcs)))
                                 (setf utgc (+ tgcu tgcs))
                                 (setf uspace
                                   (+ (* scons 8) (* ssym 24) sother))))
                            (length *parse-record*)))
                (readings (if (or (equal output "") (> readings 0))
                             readings
                            -1))
                (best-first-p (> (length *parse-times*) 2))
                (end (pop *parse-times*))
                (times (nreverse *parse-times*))
                (start (pop times))
                (total (round (* (- end start) 1000)
                              internal-time-units-per-second))
                (first (if best-first-p
                         (round (* (- (first times) start) 1000)
                                internal-time-units-per-second)
                         (if (> readings 0) total -1)))
                #+:pooling
                (pool (and (find-symbol "*DAG-POOL*")
                           (boundp (find-symbol "*DAG-POOL*"))
                           (symbol-value (find-symbol "*DAG-POOL*"))))
                #+:pooling
                (position (when pool
                           (funcall
                            (symbol-function (find-symbol "POOL-POSITION"))
                            pool)))
                #+:pooling
                (garbage (when pool
                           (funcall
                            (symbol-function (find-symbol "POOL-GARBAGE"))
                            pool)))
                (comment
                 #+:pooling
                 (format nil "(:pool . ~d) (:garbage . ~d)" position garbage)
                 #-:pooling
                 "")
                (comment
                 (if packingp
                   (format
                    nil
                    "(:utcpu . ~d) (:utgc . ~d) (:uspace . ~d)
                     (:subsumptions . ~d) (:equivalence . ~d) ~
                     (:proactive . ~d) (:retroactive . ~d)  ~
                     (:trees . ~d) (:frozen . ~d) (:failures . ~d) ~a"
                    utcpu utgc uspace
                    (statistics-subsumptions *statistics*)
                    (statistics-equivalent *statistics*)
                    (statistics-proactive *statistics*)
                    (statistics-retroactive *statistics*)
                    (length *parse-record*)
                    (statistics-frozen *statistics*)
                    (statistics-failures *statistics*)
                    comment)
                   comment))
                (summary (summarize-chart :derivationp (< nresults 0))))
           (multiple-value-bind (l-s-tasks redges words)
               (parse-tsdb-count-lrules-edges-morphs)
             (declare (ignore l-s-tasks words))
             `((:others . ,others) (:symbols . ,symbols)
               (:conses . ,conses)
               (:treal . ,treal) (:tcpu . ,tcpu)
               (:tgc . ,tgc)
               (:rpedges . ,redges)
               (:pedges . ,(rest (assoc :pedges summary)))
               (:aedges . ,(rest (assoc :aedges summary)))
               (:p-stasks . ,s-tasks) (:p-etasks . ,e-tasks)
               (:p-ftasks . ,f-tasks) (:p-ctasks . ,c-tasks)
               (:l-stasks . ,(rest (assoc :l-stasks summary)))
               (:words . ,(rest (assoc :words summary)))
               (:total . ,total) (:first . ,first)
               (:unifications . ,unifications) (:copies . ,copies)
               (:readings . ,readings)
               (:error . ,(pprint-error output))
               (:comment . ,comment)
               (:results .
                ,(append
                  (unless (and packingp nil)
                    (loop
                        with *package* = *lkb-package*
                        with nresults = (if (<= nresults 0)
                                          (length *parse-record*)
                                          nresults)
                        for i from 0
                        for parse in (reverse *parse-record*)
                        for time = (if (integerp (first times))
                                     (round (* (- (pop times) start) 1000)
                                            internal-time-units-per-second )
                                     total)
                        for derivation = (with-standard-io-syntax
                                           (let ((*package* *lkb-package*))
                                             (write-to-string
                                              (compute-derivation-tree parse)
                                              :case :downcase)))
                        for r-redges = (length
                                        (parse-tsdb-distinct-edges parse nil))
                        for size = (parse-tsdb-count-nodes parse)
                        for tree = (tsdb::call-hook trees-hook parse)
                        for mrs = (tsdb::call-hook semantix-hook parse)
                        while (>= (decf nresults) 0) collect
                          (pairlis '(:result-id :mrs :tree
                                     :derivation :r-redges :size
                                     :r-stasks :r-etasks
                                     :r-ftasks :r-ctasks
                                     :time)
                                   (list i mrs tree
                                         derivation r-redges size
                                         -1 -1
                                         -1 -1
                                         time))))
                  (when (< nresults 0)
                    (loop
                        for i from (- (length *parse-record*) 1)
                        for derivation in (rest (assoc :derivations summary))
                        for string = (with-standard-io-syntax
                                       (let ((*package* *lkb-package*))
                                         (write-to-string
                                          derivation :case :downcase)))
                        collect (pairlis '(:result-id :derivation)
                                         (list i string))))))))))))
   (unless trace (release-temporary-storage))
   (append
    (when condition
        (let* ((error (tsdb::normalize-string
                       (format nil "~a" condition)))
               (error (pprint-error error)))
          (pairlis '(:readings :condition :error)
                   (list -1 (unless burst condition) error))))
    return))))

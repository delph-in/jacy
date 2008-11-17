;;;
;;; local patches
;;;
;;;
(in-package :lkb)

#+:tsdb
(in-package :tsdb)
;;  (lkb::lkb-load-lisp "/home/bond/delphin/grammars/japanese/lkb/" "patches.lsp")

;; (tsdb::export-htmls tsdb::*tsdb-data* :condition "i-id < 100" :path "/tmp")
;;
;;
;; (setf  *redwoods-export-values* '(:TREE :MRS))
;; or (:DERIVATION :TREE :AVM :MRS :INDEXED :DEPENDENCIES :TRIPLES)
;;
;; ToDo:
;;  get features working
;;  links to text (ala HoG)?
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
	  (format stream  "<link TYPE=\"text/css\" REL=\"stylesheet\" HREF=\"../../logon.css\">~%")
	  (format stream  "<script SRC=\"../../logon.js\" LANGUAGE=\"javascript\" TYPE=\"text/javascript\"></script>~%")
	  (format stream  "<script SRC=\"../../custom.js\" LANGUAGE=\"javascript\" TYPE=\"text/javascript\"></script>~%")
	  (format stream  "<script SRC=\"../../prototype.js\" LANGUAGE=\"javascript\" TYPE=\"text/javascript\"></script>~%")
	  (format stream  "<script SRC=\"../../scriptaculous.js\" LANGUAGE=\"javascript\" TYPE=\"text/javascript\"></script>~%")
	  (format stream  "<script SRC=\"../../alttxt.js\" LANGUAGE=\"javascript\" TYPE=\"text/javascript\"></script>~%")
	  (format stream  "<script>window.name = 'default'</script>~%")
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
;;;       I always want to be thinned	  
;;;	  (unless *redwoods-thinning-export-p*
;;;	    (export-html
;;;             item active :complementp t :offset offset :stream stream))
	  (format stream  "<pre>Redwoods export of `~a' (~a@~a, ~a)</pre>~%"
	   data (current-user) (current-host) (current-time :long :iso))
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
;;;  for ident = (format nil "~a @ ~a~@[ @ ~a~]" i-id result-id i-comment)
      when (zerop (mod i 100)) do (clrhash *reconstruct-cache*)
      when (or dag mrs) do
;;;        (format 
;;;         stream 
;;;         "<h3>[~d:~d] ~:[(active)~;(inactive)~]</h3>~%" 
;;;         (+ parse-id offset) result-id complementp)
        (setf lkb::*cached-category-abbs* nil)
	;;
	;; HTML
	;;
	;; Caption
	(format stream "<table><caption>~a</caption>" 
		input)
	;; Tree
	(format stream "<tr>~%")
	(format stream "<td class=resultsTree >~%")
	(lkb::html-tree edge :stream stream :indentation 4)
	(format stream "~%")
	(format stream "</td>~%")
	;; MRS
	(format stream "<td class=resultsMrs>~%")
	(mrs::output-mrs1 mrs 'mrs::html  stream)
	(format stream "~%")
	(format stream "</td>~%")
	(format stream "</tr></table>~%")
;;;        ;; Derivation
;;;        (when (or (eq *redwoods-export-values* :all)
;;;                  (smember :derivation *redwoods-export-values*))
;;;          (let ((*package* (find-package :tsdb)))
;;;	    (format stream "<pre>~%")
;;;            (format stream "~s~%~%~%" derivation)
;;;	    (format stream "</pre>~%")))
	;; Dependency
        (when (or (eq *redwoods-export-values* :all)
                  (smember :dependencies *redwoods-export-values*))
          (ignore-errors (format stream "<h4>Dependencies</h4><pre>~%")
			 (mrs::ed-output-psoa mrs :stream stream)
			 (format stream "</pre>~%")))
	(when (or (eq *redwoods-export-values* :all)
                  (smember :triples *redwoods-export-values*))
          (ignore-errors (format stream "<h4>Triples</h4><pre>~%")
			 (mrs::ed-output-psoa mrs :format :triples :stream stream)
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



(defun decode-time (time &key long)
  (multiple-value-bind (second minute hour day month year foo bar baz)
      (decode-universal-time time)
    (declare (ignore foo bar baz))
    (let ((months '("jan" "feb" "mar" "apr" "may" "jun" 
                    "jul" "aug" "sep" "oct" "nov" "dec")))
      (cond
       ((null long)
        (format nil "~a-~a-~a" day month year))
       ;;; FCB
       ((member long '(:iso))
        (format nil "~a-~a-~a" year month day))
       ((member long '(:usa :us :reverse))
        (format nil "~2,'0d-~2,'0d-~2,'0d" (mod year 100) month day))
       ((member long '(:tsdb))
        (format
         nil "~a-~a-~a ~2,'0d:~2,'0d" 
         day (nth (- month 1) months) year hour minute))
       ((member long '(:pretty :readable))
        (format
         nil "~a-~a-~a (~2,'0d:~2,'0d h)" 
         day (nth (- month 1) months) year hour minute))
       ((eq long :short)
        (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second))
       (t
        (format
         nil "~a-~a-~a (~2,'0d:~2,'0d:~2,'0d)"
         day month year hour minute second))))))

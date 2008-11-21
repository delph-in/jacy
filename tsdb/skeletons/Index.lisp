;;;
;;; this file should be `Index.lisp' and reside in the directory containing the
;;; tsdb(1) test suite skeleton databases (typically a subdirectory `skeletons'
;;; in the tsdb(1) database root directory `*tsdb-home*').
;;;
;;; the file should contain a single un-quote()d Common-Lisp list enumerating
;;; the available skeletons, e.g.
;;;
;;;   (((:path . "english") (:content . "English TSNLP test suite"))
;;;    ((:path . "csli") (:content . "CSLI (ERGO) test suite"))
;;;    ((:path . "vm") (:content . "English VerbMobil data")))
;;;
;;; where the individual entries are assoc() lists with at least two elements:
;;;
;;;   - :path --- the (relative) directory name containing the skeleton;
;;;   - :content --- a descriptive comment.
;;;
;;; the order of entries is irrelevant as the `tsdb :skeletons' command sorts
;;; the list lexicographically before output.
;;;

(((:path . "tanaka") 
  (:content . "Japanese Tanaka Corpus"))
 ((:path . "vanilla") 
  (:content . "Basic Japanese Constructions"))
 ((:path . "haikingu") 
  (:content . "Hiking Corpus from Logon (tur/hike)"))
 ((:path . "mrs")
  (:content . "MRS test suite"))
 ((:path . "kinou1")
  (:content . "Ikehara Kinou Shikenbun Ch 01-05 (v2)"))
 ((:path . "kinou2")
  (:content . "Ikehara Kinou Shikenbun Ch 06-10 (v2)"))
 ((:path . "kinou3")
  (:content . "Ikehara Kinou Shikenbun Ch 11-14 (v2)"))
)







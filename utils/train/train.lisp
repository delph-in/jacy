(in-package :tsdb)

(tsdb :home 
      "/home/bond/hinoki/tanaka/070518")
(tsdb :skeletons 
      "/home/bond/hinoki/tanaka/070518/jacy/tsdb/tanaka_ja.skel")

(setf *feature-grandparenting* 3)

(setf *feature-use-preterminal-types-p* t)

(setf *feature-lexicalization-p* nil)

(setf *feature-constituent-weight* 0)

(setf *feature-active-edges-p* t)

(setf *feature-ngram-size* 0)

(setf *feature-ngram-tag* :type)

(setf *feature-ngram-back-off-p* t)
;;; increase this to make a smaller model
(setf *feature-frequency-threshold* (make-counts :relevant 1))

(setf *feature-random-sample-size* nil)

(setf *feature-item-enhancers* nil)

(setf *feature-lm-p* nil)

(setf *feature-preference-weightings* '((0 :binary)))

(setf %redwoods-items-increment% #-:64bit 100 #+:64bit 200)

(setf %redwoods-items-percentile% 20)

(setf *maxent-relative-tolerance* 1e-8)

(setf *maxent-variance* 1e-4)

(train "tanaka-train" "tanaka-006-015-new.mem" :fcp t)

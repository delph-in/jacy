#+:mrs
(load (merge-pathnames
       (make-pathname :directory 
                      (pathname-directory
                       (dir-append *grammar-directory* 
                                   '(:relative "lkb"))))
            (make-pathname :name "mrsglobals.lisp")
            (parent-directory)))

;;; (when (fboundp 'index-for-generator)
;;;   (index-for-generator))

#+mrs(read-mrs-rule-file-aux 
      (merge-pathnames
       (make-pathname :directory 
                      (pathname-directory
                       (dir-append *grammar-directory* 
                                   '(:relative "lkb"))))
      (make-pathname 
       :name "genrules.mrs"))
      t)




#+:mrs
(in-package "CL-USER")





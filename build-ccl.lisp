(let ((quicklisp-init (merge-pathnames
                       (make-pathname :directory '(:relative "quicklisp")
                                      :name "setup"
                                      :type "lisp")
                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :deadwood)

(let* ((dir (ccl:getenv "MAKEFILE_DIR"))
       (bindir (merge-pathnames
                (make-pathname :directory '(:relative "bin"))
                dir))
       (output-file (merge-pathnames
                     (make-pathname :name "dw")
                     bindir)))
  (ensure-directories-exist bindir)
  (save-application output-file
                    :toplevel-function #'dw:main
                    :prepend-kernel t))

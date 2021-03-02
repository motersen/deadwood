(uiop:define-package :deadwood/main
    (:nicknames :deadwood
                :dw)
  (:use :cl
        :uiop)
  (:import-from :unix-opts)
  (:use-reexport :deadwood/find
                 :deadwood/filesystem
                 :deadwood/utility)
  (:export :main))

(in-package :deadwood/main)

(opts:define-opts
  (:name :age
         :description "Maximum age in days"
         :short #\a
         :long "age"
         :arg-parser #'parse-integer)
  (:name :remove
         :description "Remove untouched files"
         :short #\r
         :long "remove"))

(defun unknown-option (condition)
  (format t "invalid option: ~s~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun parse-options (&rest arguments)
  (handler-case
      (handler-bind ((opts:unknown-option #'unknown-option))
        (apply #'opts:get-opts (and arguments
                                    (list arguments))))
    (opts:missing-arg (condition)
      (format t "option ~s needs an argument!~%"
              (opts:option condition))
      (opts:exit 1))
    (opts:arg-parser-failed (condition)
      (format t "cannot parse ~s as argument of ~s~%"
              (opts:raw-arg condition)
              (opts:option condition))
      (opts:exit 1))
    (opts:missing-required-option (con)
      (format t "fatal: ~a~%" con)
      (opts:exit 1))))

(defun main (&rest arguments)
  (handler-bind ((error (lambda (c)
                          (format *error-output* "Error: ~a~%" c)
                          (opts:exit 1))))
    (multiple-value-bind (options paths)
        (apply #'parse-options arguments)
      (let ((maximum-age (or (getf options :age)
                             30))
            (remove (getf options :remove))
            (paths (mapcar #'truename paths)))
        (multiple-value-bind (old-dirs old-files)
            (values-list (zip (mapcar (lambda (p)
                                        (find-old-files p :max-age maximum-age))
                                      paths)))
          (if remove
              (handler-bind ((file-error #'report-removal-errors))
                (format t "~a deleted the following files and directories:~%"
                        (pathname-name (pathname (first (opts:argv)))))
                (format t "~{~{~a~%~}~}"
                        (mapcar
                         (lambda (dirs files base-path)
                           (append
                            (delete-directories-if-subpath dirs base-path)
                            (delete-files-if-subpath files base-path)))
                         old-dirs
                         old-files
                         paths)))
              (mapc (lambda (dirs files)
                      (format t "~@{~{~a~%~}~}" dirs files))
                    old-dirs
                    old-files)))))))

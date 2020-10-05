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
        (apply #'opts:get-opts arguments))
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
            (values-list (zip (mapcar #'find-old-files paths)))
          (if remove
              (handler-bind ((file-error #'report-removal-errors))
                (mapc
                 (lambda (dirs files base-path)
                   (mapc (lambda (dir)
                           (delete-if-subpath dir base-path
                                              :file-or-dir :directory))
                         dirs)
                   (mapc (lambda (file)
                           (delete-if-subpath file base-path
                                              :file-or-dir :file))
                         files))
                 old-dirs
                 old-files
                 paths))
              (mapc (lambda (dirs files)
                      (format t "~{~a~%~}" dirs)
                      (format t "~{~a~%~}" files))
                    old-dirs
                    old-files)))))))

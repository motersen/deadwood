(uiop:define-package :deadwood/filesystem
    (:use :cl :uiop)
  (:export :delete-if-subpath
           :delete-file-if-subpath
           :delete-directory-if-subpath
           :report-removal-errors
           :does-not-exist-error
           :not-subpath-error))

(in-package :deadwood/filesystem)

(defun delete-if-subpath (path base-path &key file-or-dir)
  (restart-case
      (progn
        (when (not file-or-dir)
          (cond
            ((directory-exists-p path)
             (setq file-or-dir :directory))
            ((file-exists-p path)
             (setq file-or-dir :file))
            (t
             (error 'simple-file-error
                    :pathname path))))
        (ecase file-or-dir
          (:directory
           (delete-directory-if-subpath path base-path))
          (:file
           (delete-file-if-subpath path base-path))))
    (skip ())))

(defun delete-file-if-subpath (file-path base-path)
  (cond
    ((not (subpathp (pathname file-path)
                    (ensure-directory-pathname base-path)))
     (error 'not-subpath-error
            :pathname file-path
            :base-path base-path
            :file-or-dir :file))
    ((not (file-exists-p file-path))
     (error 'does-not-exist-error
            :pathname file-path
            :file-or-dir :file))
    (t
     (delete-file-if-exists file-path))))

(defun delete-directory-if-subpath (directory-path base-path)
  (let ((directory-path (ensure-directory-pathname directory-path))
        (base-path (ensure-directory-pathname base-path)))
    (handler-case
        (delete-directory-tree
         (ensure-directory-pathname directory-path)
         :validate (lambda (p) (subpathp p base-path))
         :if-does-not-exist :ignore)
      (uiop/utility:parameter-error (c)
        (if (not (subpathp directory-path base-path))
            (error 'not-subpath-error
                   :pathname directory-path
                   :base-path base-path
                   :file-or-dir :directory)
            (error c)))
      (simple-error (c)
        (if (not (directory-exists-p directory-path))
            (error 'does-not-exist-error
                   :pathname directory-path
                   :file-or-dir :directory)
            (error c))))))

(defun report-removal-errors (c)
  (format *error-output* "Error: Could not delete '~a': ~a~%"
          (file-error-pathname c)
          c)
  (let ((restart (find-restart 'skip)))
    (when restart
      (invoke-restart restart))))

(define-condition does-not-exist-error (file-error)
  ((file-or-dir :initarg :file-or-dir
                :reader file-or-dir
                :type (member :file
                              :directory)))
  (:report (lambda (c stream)
             (format stream "No such ~a~%"
                     (ecase (file-or-dir c)
                       (:file "file")
                       (:directory "directory"))))))

(define-condition not-subpath-error (file-error)
  ((base-path :initarg :base-path
              :reader base-path)
   (file-or-dir :initarg :file-or-dir
                :reader file-or-dir
                :type (member :file
                              :directory)))
  (:report (lambda (c stream)
             (format stream "Not a subpath of '~a'"
                     (base-path c)))))

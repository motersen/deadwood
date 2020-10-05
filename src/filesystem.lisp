(in-package :deadwood)

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
     (delete-file file-path)
     file-path)))

(defun delete-files-if-subpath (file-paths base-path)
  (if (null file-paths)
      nil
      (restart-case
          (cons
           (delete-file-if-subpath (car file-paths) base-path)
           (delete-files-if-subpath (cdr file-paths) base-path))
        (skip ()
          (delete-files-if-subpath (cdr file-paths) base-path)))))

(defun delete-directory-if-subpath (directory-path base-path)
  (setf directory-path (ensure-directory-pathname directory-path)
        base-path (ensure-directory-pathname base-path))
  (handler-case
      (progn
        (delete-directory-tree
         (ensure-directory-pathname directory-path)
         :validate (lambda (p) (subpathp p base-path))
         :if-does-not-exist :ignore)
        directory-path)
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
          (error c)))))

(defun delete-directories-if-subpath (directory-paths base-path)
  (if (null directory-paths)
      nil
      (restart-case
          (cons
           (delete-directory-if-subpath (car directory-paths) base-path)
           (delete-directories-if-subpath (cdr directory-paths) base-path))
        (skip ()
          (delete-directories-if-subpath (cdr directory-paths) base-path)))))

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

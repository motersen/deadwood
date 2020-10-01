(uiop:define-package :deadwood/find
  (:use :cl
        :uiop)
  (:import-from :osicat)
  (:export :find-old-files
           :is-file-recent
           :is-dir-recent
           :get-unix-time))

(in-package :deadwood/find)

(defun is-file-recent (path)
  (declare (special *age-limit*))
      ;; check not directory
  (let ((stats (nix:stat path)))
    (< *age-limit* (nix:stat-atime stats))))

(defun directories-recent-p (subdirs)
  (if (null subdirs)
      nil
      (if (member-if #'is-file-recent (directory-files (car subdirs)))
          t
          (directories-recent-p (append (cdr subdirs)
                                        (subdirectories (car subdirs)))))))
  
(defun is-dir-recent (path)
  (directories-recent-p (list path)))

(defun find-old-files (path &key (max-age 30))
  (let* ((path (truename path))
         (subdirs (subdirectories path))
         ;; e-d-p is necessary for ASDF < 3.3.4
         (files (directory-files (ensure-directory-pathname path)))
         (*age-limit* (- (get-universal-time)
                         (encode-universal-time 0 0 0 1 1 1970 0)
                         (* 60 60 24 max-age))))
    (declare (special *age-limit*))
    (list (remove-if #'is-dir-recent subdirs)
          (remove-if #'is-file-recent files))))

(in-package :deadwood)

(defun is-file-recent (path)
  (declare (special *age-limit*))
  ;; check not directory
  (handler-case
      (let ((stats (nix:stat path)))
        (< *age-limit* (nix:stat-atime stats)))
    (nix:enoent ()
      (format *error-output* "Error accessing '~a': File does not exist.~%"
              path)
      nil)))

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

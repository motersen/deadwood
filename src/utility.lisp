(in-package :deadwood)

(defun zip (lists)
  (if (or (null lists)
          (member-if #'null lists))
      nil
      (cons (mapcar #'first lists)
            (zip (mapcar #'rest lists)))))

#+ecl
(defun ecl-fix-utf8-pathname (path)
  (let* ((pathstring (coerce (namestring path) 'base-string))
         (stream (ext:make-sequence-input-stream pathstring
                                                 :external-format :utf-8)))
    (pathname (read-line stream))))

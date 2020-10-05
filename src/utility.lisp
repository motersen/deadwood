(in-package :deadwood)

(defun zip (lists)
  (if (or (null lists)
          (member-if #'null lists))
      nil
      (cons (mapcar #'first lists)
            (zip (mapcar #'rest lists)))))

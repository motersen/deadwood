(uiop:define-package :deadwood/utility
    (:use :cl)
  (:export :zip))

(in-package :deadwood/utility)

(defun zip (lists)
  (if (member-if #'null lists)
      nil
      (cons (mapcar #'first lists)
            (zip (mapcar #'rest lists)))))

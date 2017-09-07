;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-


;; TODO
;; (defun valid-p (l)
;;   (loop
;;      with all-same = t
;;      with first (first l)
;;      with common-value = first
;;      for x in l
;;      do (if (!= first x)
;;             (cond ((= (- first 1) x)
;;                    (progn
;;                      (if all-same))))
;;             )))

(defun main ()
  (let* ((s (read-line))
         (p-list '())
         (p-list-with-value (loop for c across s
                               do (setf (getf p-list c) (1+ (or (getf p-list c) 0)))))
         (num-list (loop for x on p-list-with-value by #'cddr collect (second x))))
    (valid-p num-list)))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defun split-string-to-integer-list (a-string)
  "将(空格分割的整数)字符串中的分割成整数list"
  (loop :for (integer position) := (multiple-value-list (parse-integer a-string :start (or position 0) :junk-allowed t)) ; 将 每次的position+1 可以从非纯数字的字符串中找到数字列表
     :while integer
     :collect integer))

(defun read-line-to-int-list ()
  "从标准输入读取一行，返回当中的整数list"
  (let* ((line (read-line))
         (int-list (split-string-to-integer-list line)))
    int-list))

(defun print-no ()
  (format t "NO"))

(defun print-yes ()
  (format t "YES"))

(defun main ()
  (let* ((int-list (read-line-to-int-list))
         (x1 (first int-list))
         (v1 (second int-list))
         (x2 (third int-list))
         (v2 (fourth int-list)))
    (break)
    (if (<= v1 v2)
        (print-no)
        (if (typep (/ (- x2 x1) (- v1 v2)) 'integer)
            (print-yes)
            (print-no)))))

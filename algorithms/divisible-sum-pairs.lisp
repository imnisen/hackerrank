;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defun split-string-to-integer-list (a-string)
  "将(空格分割的整数)字符串中的分割成整数list"
  (loop :for (integer position) := (multiple-value-list (parse-integer a-string :start (or position 0) :junk-allowed t)) ; 将 每次的position+1 可以从非纯数字的字符串中找到数字列表
     :while integer
     :collect integer))

(defun read-line-to-int-array ()
  "从标准输入读取一行，返回当中的整数array"
  (let ((line (read-line)))
    (list-to-1d-array (split-string-to-integer-list line))))

(defun list-to-1d-array (a-list)
  "列表转化成1维数组"
  (make-array (length a-list)
              :initial-contents a-list))

(defun main ()
  (let* ((n-k-array (read-line-to-int-array))
         (k (aref n-k-array 1))
         (integer-array (read-line-to-int-array))
         (l (length integer-array))
         (count 0))
    (loop for i from 0 below l do
         (loop for j from (+ 1 i) below l do
              (if (zerop (mod (+ (aref integer-array i) (aref integer-array j)) k))
                  (incf count)))
       finally (return count))))

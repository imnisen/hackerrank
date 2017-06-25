;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defun split-string-to-integer-list (a-string)
  "将(空格分割的整数)字符串中的分割成整数list"
  (loop :for (integer position) := (multiple-value-list (parse-integer a-string :start (or position 0) :junk-allowed t)) ; 将 每次的position+1 可以从非纯数字的字符串中找到数字列表
        :while integer
        :collect integer))

(defun list-to-1d-array (a-list)
  "列表转化成1维数组"
  (make-array (length a-list)
              :initial-contents a-list))

(defun read-line-to-int-array ()
  "从标准输入读取一行，返回当中的整数array"
  (let ((line (read-line)))
    (list-to-1d-array (split-string-to-integer-list line))))

(defun main ()
  (let* ((n (read-line-to-int-array))
         (a-array (read-line-to-int-array))
         (hash (make-hash-table))
         (id 0)
         (num 0))
    (declare (ignore n))
    (loop for i being the element of a-array do
          (if (nth-value 1 (gethash i hash))
              (incf (gethash i hash))
            (setf (gethash i hash) 1)))
    (loop for value being the hash-values of hash
          using (hash-key key) do
          (cond ((> value num) (setf num value) (setf id key))
                ((= value num) (if (< key id) (setf id key)))
                (t nil)))
    id))

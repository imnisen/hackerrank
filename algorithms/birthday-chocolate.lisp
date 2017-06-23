;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;; 处理输入输出函数
(defun split-string-to-integer-list (a-string)
  "将(空格分割的整数)字符串中的分割成整数list"
  (loop :for (integer position) := (multiple-value-list (parse-integer a-string :start (or position 0) :junk-allowed t)) ; 将 每次的position+1 可以从非纯数字的字符串中找到数字列表
     :while integer
     :collect integer))

(defun list-to-1d-array (a-list)
  "列表转化成1维数组"
  (make-array (length a-list)
              :initial-contents a-list))

(defun read-line-to-int-list ()
  "从标准输入读取一行，返回当中的整数list"
  (let ((line (read-line)))
    (split-string-to-integer-list line)))

(defun read-line-to-int-array ()
  "从标准输入读取一行，返回当中的整数array"
  (let ((line (read-line)))
    (list-to-1d-array (split-string-to-integer-list line))))

(defun print-list-one-line-with-space (a-list)
  "将一个list输出到1行， 中间空格分割"
  (format t "~{~a ~}" a-list))

(defun sum (a-list)
  "一个数字列表或者数组 求和"
  (reduce #'+ a-list))

(defun array-slice (a-array start end)
  "数组切片; 产生一个依赖原数组的displaced 数组;当切片start 或 end 大于数组长度，自动切短到数组长度；暂时不支持负值切片"
  (let* ((l (length a-array))
         (new-start (if (> start l) l start))
         (new-end (if (> end l) l end))
         (new-array-length (- new-end new-start)))
    (make-array new-array-length
                :displaced-to a-array
                :displaced-index-offset new-start)))

(defun main ()
  (let* ((n (read-line-to-int-list))
         (number-array (read-line-to-int-array))
         (l (length number-array))
         (d-m-list (read-line-to-int-list)))
    (declare (ignore n))
    (destructuring-bind (d m) d-m-list
      (if (>= l m)
          (loop for i upto (- l m)
             count (= d (sum (array-slice number-array i (+ i m)))))
          0))))

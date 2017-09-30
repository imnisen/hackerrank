;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; 处理hackerrank输入的工具
;;; 获取得用到这个将字符集设置成utf-8
;;; (setf sb-impl::*default-external-format* :UTF-8)
;;; sbcl -- eval '(setf sb-impl::*default-external-format* :UTF-8)'
;;; debug level : (declaim (optimize (debug 3)))

;; (in-package #:hackerrank.tools)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 字符串分割相关
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-string-to-integer-list (a-string)
  "将(空格分割的整数)字符串中的分割成整数list"
  (loop :for (integer position) := (multiple-value-list (parse-integer a-string :start (or position 0) :junk-allowed t)) ; 将 每次的position+1 可以从非纯数字的字符串中找到数字列表
     :while integer
     :collect integer))

(defun split-string-to-char-list (a-string)
  "将字符串分割成字符list"
  (loop :for x :across a-string :collect x))

(defun split-string-to-string-list (a-string)
  "将字符串转化成单独字符串list(空字符串也会返回)"
  (loop :for x :across a-string :collect (string x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 标准输入读取相关
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-line-to-int-list ()
  "从标准输入读取一行，返回当中的整数list"
  (let ((line (read-line)))
    (split-string-to-integer-list line)))

(defun read-line-to-int-array ()
  "从标准输入读取一行，返回当中的整数array"
  (let ((line (read-line)))
    (list-to-1d-array (split-string-to-integer-list line))))

(defun read-line-to-char-list ()
  "从标准输入读取一行，返回当中的字符(例如#\a)list"
  (let ((line (read-line)))
    (split-string-to-char-list line)))

(defun read-line-to-string-list ()
  "从标准输入读取一行，返回当中的字符串list"
  (let ((line (read-line)))
    (split-string-to-string-list line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 处理hackerrank输出的工具
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-list-line-by-line (a-list)
  "将一个list逐行输出"
  (loop for x in a-list
     do (format t "~a~%" x)))

(defun print-list-one-line-with-space (a-list)
  "将一个list输出到1行， 中间空格分割"
  (format t "~{~a ~}" a-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 类型转化工具
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-1d-array (a-list)
  "列表转化成1维数组"
  (make-array (length a-list)
              :initial-contents a-list))

(defun 1d-array-to-list (a-array)
  (loop for i below (array-dimension a-array 0)
     collect (aref a-array i)))

(defun string-to-integer (a-string)
  "将字符串转成整形数字"
  (nth-value 0 (parse-integer a-string :radix 10 :junk-allowed nil)))

(defun number-to-string (a-number)
  "将数字转化成字符串"
  (write-to-string a-number :base 10))

(defun string-to-number (a-string)
  "将字符串转成数字，包括(正负)整形浮点型. 这里没有对a-string进行类型检查，如果传一个不全是数字的字符串，会返回其它类型数据"
  (with-input-from-string (in a-string)
    (read in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 方便计算的函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum (a-list)
  "一个数字列表或者数组 求和"
  (reduce #'+ a-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 数组处理相关工具
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array-slice (a-array start end)
  "数组切片; 产生一个依赖原数组的displaced 数组;当切片start 或 end 大于数组长度，自动切短到数组长度；暂时不支持负值切片"
  (let* ((l (length a-array))
         (new-start (if (> start l) l start))
         (new-end (if (> end l) l end))
         (new-array-length (- new-end new-start)))
    (make-array new-array-length
                :displaced-to a-array
                :displaced-index-offset new-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash table 操作相关
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hash-value-to-list (a-hash)
  (loop for value being the hash-values in a-hash collect value))

(defun hash-key-to-list (a-hash)
  (loop for key being the hash-keys in a-hash collect key))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 查看值
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-hash (a-hash-table)
  (format t "LENGTH: ~A" (hash-table-count a-hash-table))
  (format t "~&-------------")
  (loop for value being the hash-values of a-hash-table
     using (hash-key key)
     do (format t "~&~A  ->  ~A" key value))
  (format t "~&-------------"))

(defun print-and-return (x)
  (format t "~&-------~&~a~&-------~&" x)
  x)

(defun print-cons (a-cons)
  (loop for x in a-cons
     do (format t "~&~a~&" x))
  a-cons)

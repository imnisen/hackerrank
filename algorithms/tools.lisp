;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; 处理hackerrank输入的工具
;;; 获取得用到这个将字符集设置成utf-8
;;; (setf sb-impl::*default-external-format* :UTF-8)
;;; sbcl -- eval '(setf sb-impl::*default-external-format* :UTF-8)'
;;; debug level : (declaim (optimize (debug 3)))

(in-package #:hackerrank.tools)

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
  (let* ((line (read-line))
         (int-list (split-string-to-integer-list line)))
    int-list))

(defun read-line-to-char-list ()
  "从标准输入读取一行，返回当中的字符(例如#\a)list"
  (let* ((line (read-line))
         (char-list (split-string-to-char-list line)))
    char-list))

(defun read-line-to-string-list ()
  "从标准输入读取一行，返回当中的字符串list"
  (let* ((line (read-line))
         (string-list (split-string-to-string-list line)))
    string-list))


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

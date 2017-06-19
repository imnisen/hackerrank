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

(defun main ()
  (let* ((n-m-list (read-line-to-int-list))
         (a-list (read-line-to-int-list))
         (b-list (read-line-to-int-list)))
    ))

(defun get-factors (x)
  (loop for i from 2 to x
     when (zerop (mod x i)) collect i))

(defun find-common-factor (v-list)
  (let* ((v1 (first v-list))
         (v-rest (rest v-list))
         (v1-factors (get-factors v1)))
    (loop for factor in v1-factors do
         (if (loop named inner for v in v-rest do
                  (if (not (zerop (mod v factor)))
                      (return-from inner nil))
                finally (return t))
             collect factor))))

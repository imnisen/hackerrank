;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;; 处理输入输出函数
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

(defun print-list-one-line-with-space (a-list)
  "将一个list输出到1行， 中间空格分割"
  (format t "~{~a ~}" a-list))

(defun main ()
  (let* ((number (read-line-to-int-list))
         (score-list (read-line-to-int-list))
         (max-score (first score-list))
         (min-score (first score-list))
         (break-max 0)
         (break-min 0))
    (declare (ignore number))
    (print-list-one-line-with-space
     (loop :for score :in score-list
        :do (cond ((> score max-score) (setq break-max (1+ break-max)) (setq max-score score))
                  ((< score min-score) (setq break-min (1+ break-min)) (setq min-score score)))
        :finally (return (list break-max break-min))))))

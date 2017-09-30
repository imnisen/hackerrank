;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defun split-string-to-integer-list (a-string)
  "将(空格分割的整数)字符串中的分割成整数list"
  (loop :for (integer position) := (multiple-value-list (parse-integer a-string :start (or position 0) :junk-allowed t)) ; 将 每次的position+1 可以从非纯数字的字符串中找到数字列表
     :while integer
     :collect integer))

(defun read-line-to-int-list ()
  "从标准输入读取一行，返回当中的整数list"
  (let ((line (read-line)))
    (split-string-to-integer-list line)))

(defun number-to-string (a-number)
  "将数字转化成字符串"
  (write-to-string a-number :base 10))

(defun string-to-number (a-string)
  "将字符串转成数字，包括(正负)整形浮点型. 这里没有对a-string进行类型检查，如果传一个不全是数字的字符串，会返回其它类型数据"
  (with-input-from-string (in a-string)
    (read in)))

(defun hash-value-to-list (a-hash)
  (loop for value being the hash-values in a-hash collect value))

(defun count-cons-different (l)
  (count-if-not (lambda (con)
                  (if (second con)
                      (= (first con) (second con))
                      t))
                l))

(defun find-biggest (data-structre k)
  )

(defun main ()
  (let* ((first-input-line (read-line-to-int-list))
         (n (first first-input-line))   ; n 是 numbers长度
         (k (second first-input-line))  ; k 是 能改变的数字的次数
         (number (first (read-line-to-int-list))) ; number 是要操作的数
         (n-string (number-to-string number))
         (n-string-reversed (reverse n-string))
         (data-structre '()))           ; 用来存储对number的分析数据
    (format t "~a ~a ~a" n-string n-string-reversed n)
    (loop
       with even? = (evenp n)
       with half-ceiling = (ceiling (/ n 2))
       for x across n-string
       for y across n-string-reversed
       for i below half-ceiling
       do (setf data-structre (append data-structre (list (list x (if (and (not even?) (= i (1- half-ceiling))) nil y))))))
    (format t "~&~a~&" data-structre)
    (if (> (count-cons-different data-structre)  k)
        -1
        (find-biggest data-structre k))
    ))


;; ;; a question
;; (defmacro m (a b c)
;;   (if c `(+ ,a ,b)
;;       `(- ,a ,b)))

;; (defun f (a b c)
;;   (m a b c))

;; (f 1 2 nil)

;; how does mac know what form to expand in compile time

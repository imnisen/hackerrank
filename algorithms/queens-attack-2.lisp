;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 帮助函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-string-to-integer-list (a-string)
  "将(空格分割的整数)字符串中的分割成整数list"
  (loop :for (integer position) := (multiple-value-list (parse-integer a-string :start (or position 0) :junk-allowed t)) ; 将 每次的position+1 可以从非纯数字的字符串中找到数字列表
     :while integer
     :collect integer))

(defun read-line-to-int-list ()
  "从标准输入读取一行，返回当中的整数list"
  (let ((line (read-line)))
    (split-string-to-integer-list line)))

(defun count-direction (rq-cq ri-ci-list direction)
  (cond ((= 1 direction) (count-direction-1 rq-cq ri-ci-list))
        ((= 2 direction) (count-direction-2 rq-cq ri-ci-list))
        ((= 3 direction) (count-direction-3 rq-cq ri-ci-list))
        ((= 4 direction) (count-direction-4 rq-cq ri-ci-list))
        ((= 5 direction) (count-direction-5 rq-cq ri-ci-list))
        ((= 6 direction) (count-direction-6 rq-cq ri-ci-list))
        ((= 7 direction) (count-direction-7 rq-cq ri-ci-list))
        ((= 8 direction) (count-direction-8 rq-cq ri-ci-list))
        (t nil)))

(defun count-direction-1 (rq-cq ri-ci-list)
  "假设ri-ci-list是分好组的"
  (- (first (first (sort ri-ci-list #'< :key #'second))) (first rq-cq) 1))

(defun list< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (first a) (first b)) (list< (rest a) (rest b)))
        (t (< (first a) (first b)))))
;; TODO
(defun count-direction-2 (rq-cq ri-ci-list)
  )

(defun my-sort (a-list)
  (sort (copy-seq a-list) #'list<))

(defun my-group (rq-cq a-list)
  (let ((rq (first rq-cq))
        (cq (second rq-cq)))
    (loop for (r c) in a-list
       if (and (= r rq) (> c cq)) collecting (list r c) into d1
       finally (return d1))))

(defun test-make-list (n)
  (loop for i from 1 to n do
       (loop for j from 1 to n
          collectint (list i j) into r)
     finally (return r)))


(defun main ()
  (let* ((n-k (read-line-to-int-list))
         (n (first n-k))
         (k (second n-k))
         (rq-cq (read-line-to-int-list))
         (a-list '())
         (ri-ci-list (dotimes (i k a-list)
                       (setf a-list (append a-list (list (read-line-to-int-list)))))))
    ))

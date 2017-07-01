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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 业务函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun count-direction (rq-cq ri-ci-list n)
  (let* ((group-list (my-group rq-cq ri-ci-list))
         (first-list (my-sort (first group-list)))
         (second-list (second group-list))
         (third-list (my-sort (third group-list)))
         (fourth-list (fourth group-list))
         (fifth-list (my-sort (fifth group-list)))
         (sixth-list (sixth group-list))
         (seventh-list (my-sort (seventh group-list)))
         (eighth-list (eighth group-list)))
    (+ (count-direction-1 rq-cq first-list n)
       (count-direction-2 rq-cq second-list n)
       (count-direction-3 rq-cq third-list n)
       (count-direction-4 rq-cq fourth-list n)
       (count-direction-5 rq-cq fifth-list n)
       (count-direction-6 rq-cq sixth-list n)
       (count-direction-7 rq-cq seventh-list n)
       (count-direction-8 rq-cq eighth-list n))))

(defun count-direction-1 (rq-cq a-list n)
  (if (null a-list)
      (- n (first rq-cq))
      (- (first (first a-list)) (first rq-cq) 1)))

(defun count-direction-2 (rq-cq a-list n)
  (let ((count 0)
        (rq (first rq-cq))
        (cq (second rq-cq)))
    (loop
       for r from (1+ rq) to n
       for c from (1+ cq) to n
       do (if (not (d2-in-p r c a-list))
              (incf count)
              (return)))
    count))

(defun count-direction-3 (rq-cq a-list n)
  (if (null a-list)
      (- n (second rq-cq))
      (- (second (first a-list)) (second rq-cq) 1)))

(defun count-direction-4 (rq-cq a-list n)
  (let ((count 0)
        (rq (first rq-cq))
        (cq (second rq-cq)))
    (loop
       for r from (1- rq) downto 1
       for c from (1+ cq) to n
       do (if (not (d4-in-p r c a-list))
              (incf count)
              (return)))
    count))

(defun count-direction-5 (rq-cq a-list n)
  (declare (ignore n))
  (if (null a-list)
      (- (first rq-cq) 1)
      (- (first rq-cq) (first (first (last a-list))) 1)))

(defun count-direction-6 (rq-cq a-list n)
  (declare (ignore n))
  (let ((count 0)
        (rq (first rq-cq))
        (cq (second rq-cq)))
    (loop
       for r from (1- rq) downto 1
       for c from (1- cq) downto 1
       do (if (not (d6-in-p r c a-list))
              (incf count)
              (return)))
    count))

(defun count-direction-7 (rq-cq a-list n)
  (declare (ignore n))
  (if (null a-list)
      (- (second rq-cq) 1)
      (- (second rq-cq) (second (first (last a-list))) 1)))

(defun count-direction-8 (rq-cq a-list n)
  (let ((count 0)
        (rq (first rq-cq))
        (cq (second rq-cq)))
    (loop
       for r from (1+ rq) to n
       for c from (1- cq) downto 1
       do (if (not (d8-in-p r c a-list))
              (incf count)
              (return)))
    count))

(defun d2-in-p (r c a-list)
  (loop named outer for a in a-list do
       (if (and (> (first a) r) (> (second a) c))
           (return-from outer nil)
           (if (and (= (first a) r) (= (second a) c))
               (return-from outer T)))
     finally (return-from outer nil)))

;; 算法需优化
(defun d4-in-p (r c a-list)
  (loop named outer for a in a-list do
       (if (and (= r (first a))
                (= c (second a)))
           (return-from outer T))
     finally (return-from outer nil)))

;; 算法需优化
(defun d6-in-p (r c a-list)
  (loop named outer for a in a-list do
       (if (and (= r (first a))
                (= c (second a)))
           (return-from outer T))
     finally (return-from outer nil)))

;; 算法需优化
(defun d8-in-p (r c a-list)
  (loop named outer for a in a-list do
       (if (and (= r (first a))
                (= c (second a)))
           (return-from outer T))
     finally (return-from outer nil)))



(defun list< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (first a) (first b)) (list< (rest a) (rest b)))
        (t (< (first a) (first b)))))

(defun my-sort (a-list)
  (sort (copy-seq a-list) #'list<))

(defun my-group (rq-cq a-list)
  (let ((rq (first rq-cq))
        (cq (second rq-cq)))
    (loop for (r c) in a-list
       if (and (= r rq) (> c cq)) collecting (list r c) into d3
       else if (and (= r rq) (< c cq)) collecting (list r c) into d7
       else if (and (> r rq) (= c cq)) collecting (list r c) into d1
       else if (and (< r rq) (= c cq)) collecting (list r c) into d5
       else if (and (< r rq) (< c cq)) collecting (list r c) into d6
       else if (and (< r rq) (> c cq)) collecting (list r c) into d4
       else if (and (> r rq) (> c cq)) collecting (list r c) into d2
       else if (and (> r rq) (< c cq)) collecting (list r c) into d8
       finally (return (list d1 d2 d3 d4 d5 d6 d7 d8)))))

;; (defun test-make-list (n)
;;   (loop for i from 1 to n
;;      append  (loop for j from 1 to n
;;                 collect (list i j))))

;; (defun p-print (a-list)
;;   (loop for x in a-list do
;;        (format t "~a~&" x)))


(defun main ()
  (let* ((n-k (read-line-to-int-list))
         (n (first n-k))
         (k (second n-k))
         (rq-cq (read-line-to-int-list))
         (a-list '())
         (ri-ci-list (dotimes (i k a-list)
                       (setf a-list (append a-list (list (read-line-to-int-list)))))))
    (count-direction rq-cq ri-ci-list n)))

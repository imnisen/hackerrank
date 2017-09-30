;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;; 待重构，因为我现在已经被这个问题搞得头晕，所以先放一放，解决其他问题去，留待以后将这个解决得更漂亮


(defun valid-p (l)
  "check if a listof char appearence times if valid"
  (if (apply #'= l)
      t
      (valid2-p l)))

(defun valid2-p (l)
  "this func is very ugly"
  (let (v-list)
    (loop for x in l
       do (setf (getf v-list x) (1+ (or (getf v-list x) 0))))
    (if (= 4 (length v-list))
        (if (or (= 1 (second v-list)) (= 1 (fourth v-list)))
            (if (< (second v-list) (fourth v-list))
                (if (or
                     (= 0 (- (first v-list) 1))
                     (= (third v-list) (- (first v-list) 1)))
                    t
                    nil)
                (if (or
                     (= 0 (- (third v-list) 1))
                     (= (first v-list) (- (third v-list) 1)))
                    t
                    nil))
            nil)
        nil)))

(defun main ()
  (let* ((s (read-line))
         (p-list '()))
    (loop for c across s
       do (setf (getf p-list c) (1+ (or (getf p-list c) 0))))
    (valid-p (loop for x on p-list by #'cddr collect (second x)))))


;; "aabbcc" => (2 2 2) => t
;; "aabbccc" => (2 2 3) => (2:2 3:1)
;; "aabbc" => (2 2 1) => (2:2 1:1)
;; "baacdd" => (2 1 1 2) => (1:2  2:2)
;; "abcccc" => (1 1 4) => (1:2  4:1)

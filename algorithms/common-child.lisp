;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-


;; (defun mklist (obj)
;;   (if (listp obj)
;;       obj
;;       (list obj)))

(defun all-child-strings (str)
  "返回一个字符串所有的子串"
  (labels ((map-join (str1 str2s)
             (loop for x in str2s collect
                  (concatenate 'string str1 x)))
           (join-together (str1s str2s)
             (concatenate 'list str1s str2s))
           (f (str)
             (if (= 0 (length str))
                 (list "")
                 (join-together
                  (map-join (subseq str 0 1)
                            (f (subseq str 1)))
                  (f (subseq str 1))))))
    (f str)))

(defun list-to-hash (lst)
  (let ((h (make-hash-table :test 'equal)))
    (loop for x in lst do (setf
                           (gethash (length x) h)
                           (append (gethash (length x) h) (list x))))
    h))

(defun have-common (lst1 lst2)
  (loop named out for x in lst1
     do (loop for y in lst2
           do (when (string-equal x y) (return-from out t)))
     finally (return-from out nil))
  )

(defun find-longest (str1s str2s)
  "返回最长的子串的长度"
  (let ((n (length str1s))
        (h1 (list-to-hash str1s))
        (h2 (list-to-hash str2s)))
    (loop for i from n downto 1
       when (have-common (gethash i h1)
                         (gethash i h2))
       do (return i))))

(defun main ()
  (let ((str1 (read-line))
        (str2 (read-line)))
    (find-longest (all-child-strings str1)
                  (all-child-strings str2))))

;; 优化方向
;; 1. 不要等到返回所有的子串再选择，应对从长度开始递减，找一个子串比较一下
;; 2. all-child-strings 返回的结构用数组
;;

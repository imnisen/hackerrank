;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-


;; New solution, 为了减少重复计算，从小往大算，算到最大的也就是需要的答案
;; refer to https://www.geeksforgeeks.org/longest-common-subsequence/
(defun lcs (x y)
  (let* ((m (length x))
         (n (length y))
         (table (make-array (list (1+ m) (1+ n)))))
    (loop for i from 0 to m
       do (loop for j from 0 to n
             do (cond
                  ((or (= i 0) (= j 0)) (setf (aref table i j) 0))
                  ((char-equal (aref x (1- i)) (aref y (1- j)))
                   (setf (aref table i j)
                         (1+ (aref table (1- i) (1- j)))))
                  (t (setf (aref table i j)
                           (max (aref table (1- i) j)
                                (aref table i (1- j))))))))
    (aref table m n)))

(defun main ()
  (let ((str1 (read-line))
        (str2 (read-line)))
    (lcs str1 str2)))



;; ;; New solution 但是仍然有很多重复计算
;; (defun lcs (x y m n)
;;   (cond ((or (= m 0) (= n 0)) 0)
;;         ((char-equal
;;           (aref x (1- m))
;;           (aref y (1- n))) (1+ (lcs x y (1- m) (1- n))))
;;         (t (max (lcs x y (1- m) n) (lcs x y m (1- n))))))

;; (defun main ()
;;   (let* ((str1 (read-line))
;;          (str2 (read-line))
;;          (n (length str1)))
;;     (lcs str1 str2 (length str1) (length str2))
;;     ))




;; Old solution
;; ;;; Not finished yet. Sucks at mem usage, efficiency.
;; ;;; sbcl hold and not excute


;; (defun have-common (lst1 lst2)
;;   (loop for x across lst1
;;      do (when (position x lst2 :test 'equal) (return t))
;;      finally (return nil)))


;; (defun map-join (str1 str2s)
;;   (if str2s
;;       (let ((acc (make-array (length str2s)))
;;             (len (array-dimension str2s 0)))
;;         (loop for i from 0 below len do
;;              (setf (aref acc i)
;;                    (concatenate 'string str1 (aref str2s i))))
;;         acc)
;;       (make-array 1 :initial-contents (list str1))))

;; (defun join-together (str1s str2s)
;;   (if str2s
;;       (concatenate 'vector str1s str2s)
;;       str1s))

;; (defun n-length-child-strings (st n cache)
;;   (let ((key (format nil "~a->~a" st n)))
;;     (if (nth-value 1 (gethash key cache))
;;         (progn
;;           ;; (format t "Cache Hit! key: ~a ~%" key)
;;           (gethash key cache))
;;         (cond ((= 0 n) nil)
;;               ((< (length st) n) nil)
;;               ((= (length st) n) (make-array 1 :initial-contents (list st)))
;;               (t (setf (gethash key cache)
;;                        (join-together
;;                         (map-join (subseq st 0 1)
;;                                   (n-length-child-strings
;;                                    (subseq st 1)
;;                                    (- n 1)
;;                                    cache))
;;                         (n-length-child-strings
;;                          (subseq st 1)
;;                          n
;;                          cache))))))))



;; (defun main ()
;;   (let* ((str1 (read-line))
;;          (str2 (read-line))
;;          (n (length str1))
;;          (cache (make-hash-table :test 'equal)))

;;     (loop for i from n downto 1
;;        do (progn
;;             (format t "---~a ~%" i)
;;             (when (have-common (n-length-child-strings str1 i cache)
;;                                (n-length-child-strings str2 i cache))
;;               (return i)))
;;        finally (return 0))))

;; ;; 现在的问题是，对于50长度的字符串， 使用n-length-child-strings求45长度的子串，这一计算过程占用太多内存，没法正常进行，而不用说长度更小的了

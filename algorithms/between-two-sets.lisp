;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;; 处理出入函数
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

;; 逻辑相关代码
(defun get-factors (x)
  "获取x的约数"
  (loop for i from 1 to x
     when (zerop (mod x i)) collect i))

(defun filter-factors (a-list v)
  "筛选出a-list中是v的约数列表，比如 a-list -> (3 4 5 6 7), v -> 24, 删选的结果是 (3 4 6)"
  (remove-if-not #'(lambda (x)
                     (if (zerop (mod v x))
                         x
                         nil))
                 a-list))

(defun filter-common-factors (a-list v-list)
  "筛选出a-list中，v-list中所有元素共同的约数的列表"
  (do* ((rest-v v-list (cdr rest-v))
        (v (first rest-v) (first rest-v))
        (a-list (filter-factors a-list v) (filter-factors a-list v)))
       ((= 1 (length rest-v)) a-list)
    (if (null a-list)
        (return nil))))

(defun filter-multiples (a-list v)
  "筛选出a-list中是v的倍数列表，比如 a-list -> (12 15 18), v -> 2, 删选的结果是 (12 18)"
  (remove-if-not #'(lambda (x)
                     (if (zerop (mod x v))
                         x
                         nil))
                 a-list))

(defun filter-common-multiples (value-list a-list)
  "筛选出a-list中，v-list中所有元素共同的倍数的列表"
  (do* ((rest-v value-list (cdr rest-v))
        (v (first rest-v) (first rest-v))
        (a-list (filter-multiples a-list v) (filter-multiples a-list v)))
       ((= 1 (length rest-v)) a-list)
    (if (null a-list)
        (return nil))))


(defun find-common-factors (v-list)
  "找出v-list中的公约数"
  (let* ((v1 (first v-list))
         (v-rest (rest v-list))
         (v1-factors (get-factors v1)))
    (if (null v-rest)                   ;如果v-list只有一个元素，那么公约数为该元素所有的约数
        v1-factors
        (filter-common-factors v1-factors v-rest))))

;; 找出公倍数的用不到，所以没有写出来

(defun main ()
  (let* ((n-m-list (read-line-to-int-list))
         (a-list (read-line-to-int-list))
         (b-list (read-line-to-int-list)))
    (declare (ignore n-m-list))
    (print-length (filter-common-multiples a-list (find-common-factors b-list)))))

;; 输出函数
(defun print-length (a-list)
  (format t "~a" (length a-list)))

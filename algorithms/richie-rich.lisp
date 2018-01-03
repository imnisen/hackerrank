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

(defun count-cons-different (l)
  (count-if-not (lambda (con)
                  (if (second con)
                      (= (first con) (second con))
                      t))
                l))

(defun find-biggest (data-structure k)
  (loop for i below k do
       (loop named inner for each in data-structure do
            (when (not (= (first each) (second each)))
              (if (> (first each) (second each))
                  (setf each (list (first each) (first each) t))
                  (setf each (list (second each) (second each) t)))
              (return-from inner nil)))
       ))

(defun two-equalp (cons)
  (if (second cons)
      (= (first cons) (second cons))
      t))

;; (defmacro check-and-set ())

(defun list-number (l)
  (nth-value 0 (parse-integer
                (concatenate 'string (loop for x in l when x collect (digit-char x))))))

(defun struct-number (data-structure)
  (loop for each across data-structure
     collecting (first each) into a
     collecting (second each) into b
     finally (return (list-number (append a (reverse b)))))
  )

(defun is-palindromes (str)
  (loop with str1 = (reverse str)
     for i below (ceiling (/ (length str) 2))
     always (char= (aref str i) (aref str1 i))))

(defun duplicate-n-times-x (x number)
  (loop with acc = nil
     for i below number do
       (setf acc (concatenate 'string acc x))
     finally (return acc)))

(defun main ()
  (let* ((first-input-line (read-line-to-int-list))
         (n (first first-input-line))   ; n 是 numbers长度
         (k (second first-input-line))  ; k 是 能改变的数字的次数
         (n-string (read-line))
         (n-string-reversed (reverse n-string))
         (data-structure (make-array 10 :fill-pointer 0 :adjustable t))  ; 用来存储对number的分析数据
         )
    ;; (format t "number ~a~%" n-string)
    ;; (format t "~a ~a ~a" n-string n-string-reversed n)

    ;; 特殊情况
    (when (= 0 k)
      (if (is-palindromes n-string)
          (return-from main n-string)
          (return-from main -1)))

    ;; 特殊情况
    (when (>= k (length n-string))
      (return-from main (duplicate-n-times-x "9" (length n-string))))

    (loop
       with even? = (evenp n)
       with half-ceiling = (ceiling (/ n 2))
       for x across n-string
       for y across n-string-reversed
       for i below half-ceiling
       do (progn (setf x (digit-char-p x)
                       y (digit-char-p y))
                 (vector-push-extend (list x
                                           (if (and (not even?)
                                                    (= i (1- half-ceiling)))
                                               nil
                                               y))
                                     data-structure) ))
    ;; (format t "~&data_structure: ~a~&" data-structure)
    (let ((len (length data-structure)))
      (labels ((fn (k1 i1 n)
                 (progn
                   ;; (format t "~%k1: ~a, i1: ~a, n: ~a , data-structure:~a~%" k1 i1 n data-structure)
                   (cond
                     ((> n 2) (struct-number data-structure))

                     ((and (> k1 0) (< i1 len)) ;;正常消耗 todo
                      (if (= n 1)
                          ;; 第1次扫描
                          (if (two-equalp (aref data-structure i1))
                              ;; data-structure 里两个元素相等
                              (progn
                                (setf (aref data-structure i1) (list (first (aref data-structure i1)) (second (aref data-structure i1)) nil))
                                (fn k1 (1+ i1) 1))

                              ;; data-structure 里两个元素不相等
                              (progn
                                (if (> (first (aref data-structure i1)) (second (aref data-structure i1)))
                                    (setf (aref data-structure i1) (list (first (aref data-structure i1)) (first (aref data-structure i1)) t))
                                    (setf (aref data-structure i1) (list (second (aref data-structure i1)) (second (aref data-structure i1)) t)))
                                (fn (1- k1) (1+ i1) 1)))

                          ;;第2次扫描
                          (if (third (aref data-structure i1))
                              ;; 第 i1个是修改过的
                              (if (= 9 (first (aref data-structure i1)))
                                  ;; 两个元素都是9
                                  (fn k1 (1+ i1) 2)
                                  ;; 两个元素修改过一样 不是9, 就都改成9, 消耗k 1的值
                                  (progn
                                    (setf (aref data-structure i1) (list 9 9 t))
                                    (fn (1- k1) (1+ i1) 2)))

                              ;; 第i1个是没有修改过的
                              (if (= 9 (first (aref data-structure i1)))
                                  ;; 两个都是9
                                  (fn k1 (1+ i1) 2)

                                  (if (second (aref data-structure i1))
                                      (if (>= k1 2)
                                          (progn
                                            (setf (aref data-structure i1) (list 9 9 t))
                                            (fn (- k1 2) (1+ i1) 2))
                                          (fn k1 (1+ i1) 2))
                                      (progn
                                        (setf (aref data-structure i1) (list 9 nil t))
                                        (fn (1- k1) (1+ i1) 2)))
                                  ))))
                     ((and (<= k1 0) (< i1 len)) ;;k不够返回-1
                      (if (>= n 2)
                          (struct-number data-structure)
                          -1))
                     ((and (> k1 0) (>= i1 len)) ;;k 还有，i没了，执行第2次扫描
                      (fn k1 0 (1+ n)))
                     ((and (<= k1 0) (>= i1 len)) ;; k和i同时消耗完
                      (struct-number data-structure))
                     ))))
        (fn k 0 1)))
    ))

(format t "~a" (main))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; 解决思路
;;; 假设输入为:
;;; 5  1
;;; 39143
;;; 根据 092282(以及反转的282290) 构造数据结构 #((3 3) (9 4) (1 nil) ,  nil代表此处位置是中间重复的值
;;; 然后从向量第一个 item (3 3) 开始'扫描', 依据k值, 如果发现item 两者相等, 那么将其结构改成 (3 3 nil), nil 代表这个item没有消耗k的次数修改过;
;;; 如果发现item两者不等,比如(3 8)，那么将其结构修改成(8 8 t), t 代表 这里修改过
;;; 如果在扫描第一次时， k用完了, 数据结构 还没扫描到结尾, 那么要查看接下的数据结构符不符合回文结构(不符合返回-1,符合的话返回修改后的数据结构)
;;; 如果第一次扫描完后, k还没用完，那么进行第2次扫描;
;;; 第2次扫描的时候，会先检查每个item, 比如(3 3 nil),看第3个元素是否为t
;;; 如果为t(也就是之前改动过), 先检查前面两个元素是否为9, 不是9的话，那么消耗1个k,将其改成(9 9 t)； 是9的话， 不消耗k,扫描下一个元素
;;; 如果为nil(也就是之前没改动过), 先检查前面两个元素是否为9, 如果不为9, 则检查k是否比2大(比2大的话才能满足修改两次的需求), 比2大的话改成(9 9 t), 小于2的话，k不变，继续扫描下一个元素; 注意2个元素是nil的case
;;; 如果有第3次扫描，那么直接返回已经成型的数据结构


;;; 特殊case k=0; k>字符串总长度

(defun split-string-to-integer-list (a-string)
  "将(空格分割的整数)字符串中的分割成整数list"
  (loop :for (integer position) := (multiple-value-list (parse-integer a-string :start (or position 0) :junk-allowed t)) ; 将 每次的position+1 可以从非纯数字的字符串中找到数字列表
     :while integer
     :collect integer))

(defun read-line-to-int-list ()
  "从标准输入读取一行，返回当中的整数list"
  (let ((line (read-line)))
    (split-string-to-integer-list line)))

(defun two-equalp (cons)
  "判断cons两个元素是否'相等', 如果一个为nil, 也任务相等"
  (if (second cons)
      (= (first cons) (second cons))
      t))

(defun list-number (l)
  "将(1 2 3)=>123"
  (nth-value 0 (parse-integer
                (concatenate 'string (loop for x in l when x collect (digit-char x))))))

(defun struct-number (data-structure)
  "将struct翻译成代表的数字"
  (loop for each across data-structure
     collecting (first each) into a
     collecting (second each) into b
     finally (return (list-number (append a (reverse b)))))
  )

(defun is-palindromes (str)
  "判断字符串是否是回文"
  (loop with str1 = (reverse str)
     for i below (ceiling (/ (length str) 2))
     always (char= (aref str i) (aref str1 i))))

(defun rest-is-palindromes (data-structure i)
  "判断一个数据结构从index=i开始算接下来是不是满足回文结构"
  (loop for index from i to (- (length data-structure) 1)
     always (two-equalp (aref data-structure i))))

(defun duplicate-n-times-x (x number)
  "将字符串x复制number份连接起来   '2' 5 =>  '22222' "
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
         (data-structure (make-array 10 :fill-pointer 0 :adjustable t)))          ;; 用来存储对number的分析数据
    ;; 特殊情况
    (when (= 0 k)
      (if (is-palindromes n-string)
          (return-from main n-string)
          (return-from main -1)))

    ;; 特殊情况
    (when (>= k (length n-string))
      (return-from main (duplicate-n-times-x "9" (length n-string))))

    ;; 预扫描生成数据结构
    (loop
       with even? = (evenp n)
       with half-ceiling = (ceiling (/ n 2))
       for x across n-string
       for y across n-string-reversed
       for i below half-ceiling
       do (progn (setf x (digit-char-p x)
                       y (digit-char-p y))
                 (vector-push-extend (list x  (if (and (not even?) (= i (1- half-ceiling))) nil y))
                                     data-structure)))

    ;; 主要逻辑
    (let ((len (length data-structure)))
      (labels ((fn (k1 j times)
                 (cond
                   ;; Case 1
                   ((> times 2) (struct-number data-structure)) ;; 第3次扫描的话直接返回

                   ;; Case 2
                   ((and (> k1 0) (< j len))                ;;正常消耗
                    (if (= times 1)
                        ;; 第1次扫描
                        (if (two-equalp (aref data-structure j))
                            ;; data-structure 里两个元素相等
                            (progn
                              (setf (aref data-structure j) (list (first (aref data-structure j)) (second (aref data-structure j)) nil))
                              (fn k1 (1+ j) 1))

                            ;; data-structure 里两个元素不相等
                            (progn
                              (if (> (first (aref data-structure j)) (second (aref data-structure j)))
                                  (setf (aref data-structure j) (list (first (aref data-structure j)) (first (aref data-structure j)) t))
                                  (setf (aref data-structure j) (list (second (aref data-structure j)) (second (aref data-structure j)) t)))
                              (fn (1- k1) (1+ j) 1)))

                        ;;第2次扫描
                        (if (third (aref data-structure j))
                            ;; 第 j个是修改过的
                            (if (= 9 (first (aref data-structure j)))
                                ;; 两个元素都是9
                                (fn k1 (1+ j) 2)
                                ;; 两个元素修改过一样 不是9, 就都改成9, 消耗k 1的值
                                (progn
                                  (setf (aref data-structure j) (list 9 9 t))
                                  (fn (1- k1) (1+ j) 2)))

                            ;; 第j个是没有修改过的
                            (if (= 9 (first (aref data-structure j)))
                                ;; 两个都是9
                                (fn k1 (1+ j) 2)

                                (if (second (aref data-structure j))
                                    (if (>= k1 2)
                                        (progn
                                          (setf (aref data-structure j) (list 9 9 t))
                                          (fn (- k1 2) (1+ j) 2))
                                        (fn k1 (1+ j) 2))
                                    (progn
                                      (setf (aref data-structure j) (list 9 nil t))
                                      (fn (1- k1) (1+ j) 2)))
                                ))))
                   ;; Case 3
                   ((and (<= k1 0) (< j len)) ;;k不够返回-1
                    (if (>= times 2)
                        ;; 第2次扫描的时候发生这种情况直接返回数据结构代表的值
                        (struct-number data-structure)
                        ;; 第一次扫描的时候，k消耗完了后，检查之后的是不是回文结构
                        (if (rest-is-palindromes data-structure j)
                            (struct-number data-structure)
                            -1)))

                   ;; Case 4
                   ((and (> k1 0) (>= j len)) ;;k 还有，数据结构已经扫描到末尾，执行第2次扫描
                    (fn k1 0 (1+ times)))

                   ;; Case 5
                   ((and (<= k1 0) (>= j len)) ;; k和数据结构同时消耗完
                    (struct-number data-structure)))))
        (fn k 0 1)))))

(format t "~a" (main))

;; 其他人的答案，待研究
;; (defun max-char (c1 c2) (if (char> c1 c2) c1 c2))
;; (define-modify-macro maxf (&rest args) max-char)

;; (defun solve (n k x)
;;   (let ((n/2 (floor n 2))
;;         (flipped (make-array (ceiling n 2) :initial-element nil))
;;         mark)
;;     (when (oddp n) (setf (aref flipped n/2) t))
;;     (loop for i below n/2
;;        when (char/= (aref x i) (aref x (- n i 1))) do
;;          (maxf (aref x i) (aref x (- n i 1)))
;;          (setf (aref x (- n i 1)) (aref x i)
;;                (aref flipped i) t)
;;          (decf k))
;;     (when (minusp k) (return-from solve "-1"))
;;     (loop while (> k 1)
;;        for i below n/2
;;        when (char/= #\9 (aref x i)) do
;;          (setf (aref x i) #\9 (aref x (- n i 1)) #\9)
;;          (decf k (if (aref flipped i) 1 2))
;;        finally (setf mark i))
;;     (loop while (plusp k) for i from mark below (ceiling n 2)
;;        when (and (aref flipped i) (char/= #\9 (aref x i))) do
;;          (setf (aref x i) #\9 (aref x (- n i 1)) #\9)
;;          (decf k))
;;     x))

;; (let* ((n (read)) (k (read)) (x (read-line))) (write-line (solve n k x)))

;; https://www.hackerrank.com/challenges/mini-max-sum

;; Enter your code here. Read input from STDIN. Print output to STDOUT
;; (let ((x nil))
;;   (setq x (read-line))
;;   (write-line x))

;; (defun r-o ()
;;   (let ((x nil))
;;     (setq x (read-line))
;;     ()
;;     (write-line x)
;;     nil))

;; (defun check-type-of-read-line ()
;;   (let ((x nil))
;;     (setq x (read-line))
;;     (type-of x)))

;; (defun check-type-of-read ()
;;   (let ((x nil))
;;     (setq x (read))
;;     (type-of x)))

;; (defun read-array (stream)
;;   (let* ((line (read-line stream))
;;          (items (split-sequence #\Space line))
;;          (numbers (map 'vector #'parse-integer items)))
;;     numbers))






;; ;;; Solutions

;; (defun min-max-sum ()
;;   (let* ((array (read-array-stdin2))
;;          (sum-of-all-numbers (reduce #'+ array))
;;          (min-number (reduce #'min array))
;;          (max-number (reduce #'max array)))
;;     (list (- sum-of-all-numbers max-number) (- sum-of-all-numbers min-number))))

;; (defun my-split-of-array (a-array)
;;   (loop for a across a-array
;;      when (not (equal a #\Space))
;;      collect a))

;; (defun my-split-of-array2n (a-array)
;;   (loop for a across a-array
;;      when (not (equal a #\Space))
;;      collect (char-int a)))

;; (defun list-to-array (a-list)
;;   (make-array (list (length a-list))
;;               :initial-contents a-list))

;; (defun read-array-stdin ()
;;   (let* ((line (read-line))
;;          (items (split-sequence:split-sequence #\Space line))
;;          (numbers (map 'vector #'parse-integer items)))
;;     numbers))

;; (defun read-array-stdin2 ()
;;   (let* ((line (read-line))
;;          (items (list-to-array (split-of-string line))))
;;     items))

;; (defun read-array-stdin3 ()
;;   (let* ((line (read-line)))
;;     (print line)))


;; Final Solution
;; (defun split-string-to-integer-list (a-string)
;;   (loop for c across a-string
;;      if (not (equal c #\Space))
;;      collect (parse-integer (string c))))

(defun split-string-to-integer-list (a-string)
  (loop :for
     (integer position) :=  (multiple-value-list
                             (parse-integer a-string :start (or position 0) :junk-allowed t ))
     :while integer
     :collect integer))

(defun list-to-array (a-list)
  (make-array (list (length a-list))
              :initial-contents a-list))

(defun read-stdin-to-array ()
  (let* ((line (read-line))
         (items (list-to-array (split-string-to-integer-list line))))
    items))

(defun min-max-sum ()
  (let* ((array (read-stdin-to-array))
         (sum-of-all-numbers (reduce #'+ array))
         (min-number (reduce #'min array))
         (max-number (reduce #'max array)))
    (list (- sum-of-all-numbers max-number) (- sum-of-all-numbers min-number))))

(defun print-list (a-list)
  (format t "~{~a ~}" a-list))

(print-list (min-max-sum)) ; result to output

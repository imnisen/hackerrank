;; (defun fib (n)
;;   (if (<= 0 n 1)
;;       n
;;       (+ (fib (- n 1))
;;          (fib (- n 2)))))


(defclass point ()
  ((x :accessor point-x :initarg :x :initform 0)
   (y :accessor point-y :initarg :y :initform 0)))


(defun sum (xs &optional (acc 0))
  (break)
  (if (null xs)
      acc
      (sum (cdr xs) (+ (car xs) acc))))

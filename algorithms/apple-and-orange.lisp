;; https://www.hackerrank.com/challenges/apple-and-orange?h_r=next-challenge&h_v=zen

(defun split-string-to-integer-list (a-string)
  (loop :for (integer position) := (multiple-value-list (parse-integer a-string :start (or position 0) :junk-allowed t ))
     :while integer
     :collect integer))

(defun read-line-to-int-list ()
  (let* ((line (read-line))
         (int-list (split-string-to-integer-list line)))
    int-list))

(defun hit-house-p (s t1 tree-point distance)
  (<= s (+ tree-point distance) t1))

(defun true-p (x)
  x)

(defun count-hits (s t1 tree-point value-list)
  (count-if #'true-p
            (loop for x in value-list when (hit-house-p s t1 tree-point x) collect x)))

(defun main ()
  (let* ((s-t-list (read-line-to-int-list))
         (s (first s-t-list))
         (t1 (second s-t-list))
         (a-b-list (read-line-to-int-list))
         (a (first a-b-list))
         (b (second a-b-list))
         (m-n-list (read-line-to-int-list))
         ;; (m (first s-t-list))
         ;; (n (second s-t-list))
         (m-value-list (read-line-to-int-list))
         (n-value-list (read-line-to-int-list)))
    (list
     (count-hits s t1 a m-value-list) (count-hits s t1 b n-value-list))))

(defun print-list-line-by-line (a-list)
  (loop for x in a-list
     do (format t "~a~%" x)))

(print-list-line-by-line (main)) ; result to output

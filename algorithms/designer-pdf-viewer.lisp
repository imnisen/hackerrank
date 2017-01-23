;; https://www.hackerrank.com/challenges/designer-pdf-viewer?h_r=next-challenge&h_v=zen

(defun split-string-to-integer-list (a-string)
  (loop :for
     (integer position) :=  (multiple-value-list
                             (parse-integer a-string :start (or position 0) :junk-allowed t ))
     :while integer
     :collect integer))

(defun split-string-to-char-list (a-string)
  (loop :for x :across a-string :collect x))

(defun main ()
  (let* ((line (read-line))
         (height-list (split-string-to-integer-list line))
         (line2 (read-line))
         (word-list (split-string-to-char-list line2))
         (my-hash (make-hash-table)))
    (loop for x in height-list
       and char-code from (char-code #\a) to (char-code #\z)
       do (setf (gethash (code-char char-code) my-hash) x))
    (* (* (length word-list) 1)
       (reduce #'max (loop for y in word-list
                        collect (gethash y my-hash))))))

(write (main))

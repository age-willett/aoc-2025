#!/usr/bin/sbcl --script
(require :uiop)

(defun delete-low-joltage (cell)
  ;; Walks the list looking for a high order value that is lower than the next value down
  ;; Deletes the high order value, effectively moving up the order of the rest of the list
  (cond ((> (cadr cell) (car cell))
         (rplaca cell (cadr cell))
         (rplacd cell (cddr cell))) ;; Replace this cell with the next cell, replace the link to the next cell with its next cell
        ((not (cddr cell)) (rplacd cell nil)) ;; Covers edge case where every value was greater than the next
        (t (delete-low-joltage (cdr cell))))) ;; Walk to the next cell

;; Probably should split find-joltage into its init and recursive varities

(defun find-joltage (s &optional joltage-list)
  ;; Marches through string s looking for the highest pair of ordered digits. Returns the joltage value as an int. Has self-initialization behavior
  (let ((test-value (if (> (length s) 0) (parse-integer (subseq s (- (length s) 1))))))
    (cond ((not joltage-list) (find-joltage (subseq s 0 (- (length s) 12))
                                            (map 'list #'digit-char-p (subseq s (- (length s) 12)))))
          ((not test-value) (reduce (lambda (a x) (+ (* 10 a) x)) joltage-list))
          ((>= test-value (car joltage-list)) ;; Had to progn with current design
           (push test-value joltage-list)
           (delete-low-joltage joltage-list)
           (find-joltage (subseq s 0 (1- (length s))) joltage-list))
          (t (find-joltage (subseq s 0 (1- (length s))) joltage-list)))))

(let ((file-contents (uiop:read-file-lines (first (uiop:command-line-arguments))))
      (joltage-sum 0))
  (dolist (row file-contents
               (print (concatenate 'string "The sum of each row's joltage is: " (write-to-string joltage-sum))))
    (incf joltage-sum (find-joltage row))))

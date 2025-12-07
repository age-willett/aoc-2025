#!/usr/bin/sbcl --script
(require :uiop)

(defun find-joltage (s &optional tens ones)
  ;; Marches through string s looking for the highest pair of ordered digits. Has self-initialization behavior
  (let ((test-value (if (> (length s) 0) (parse-integer (subseq s (- (length s) 1))))))
    (cond ((not tens) (find-joltage (subseq s 0 (- (length s) 2))
                                    (parse-integer (subseq s (- (length s) 2) (- (length s) 1)))
                                    (parse-integer (subseq s (- (length s) 1))))) ;; Initialize search
          ((not test-value) (+ (* 10 tens) ones))
          ((>= test-value tens) (find-joltage (subseq s 0 (1- (length s))) test-value (max tens ones)))
          (t (find-joltage (subseq s 0 (1- (length s))) tens ones)))))

(let ((file-contents (uiop:read-file-lines (first (uiop:command-line-arguments))))
      (joltage-sum 0))
  (dolist (row file-contents
               (print (concatenate 'string "The sum of each row's joltage is: " (write-to-string joltage-sum))))
    (incf joltage-sum (find-joltage row))))

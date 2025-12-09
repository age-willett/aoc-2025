#!/usr/bin/sbcl --script
(require :uiop)

;; Taken from Day 2 solutions
(defun split-line-by-character (sep line &optional collector)
  (let ((pos (position sep line :from-end t)))
    (cond (pos (split-line-by-character sep (subseq line 0 pos) (cons (subseq line (1+ pos)) collector)))
          (t (cons line collector)))))

;; Need to sharpen up my naming conventions
(defun insert-into-range-list (range-list range-pair)
  ;;; Insert a range pair into a list attempting to keep order based on the range minimum
  (let ((compare range-list))
    (loop
      (cond ((not compare) (return (cons range-pair nil))) ; Init empty list
            ((<= (car range-pair) (caar compare)) ; Behind the highest low number
             (rplacd compare (cons (car compare) (cdr compare)))
             (rplaca compare range-pair)
             (return range-list))
            ((not (cdr compare)) (rplacd compare (cons range-pair nil)) (return range-list)) ; Made it to list end
            (t (setq compare (cdr compare))))))) ; Advance compare after exhausting all cases

(defun build-fresh-ranges (lines &optional fresh-range)
  (cond ((string= (car lines) "") fresh-range)
        (t (let* ((range-ends (split-line-by-character #\- (car lines)))
                  (range-pair `(,(parse-integer (car range-ends)) . ,(parse-integer (cadr range-ends)))))
             (build-fresh-ranges (cdr lines) (insert-into-range-list fresh-range range-pair))))))

(let* ((file-contents (uiop:read-file-lines (car (uiop:command-line-arguments))))
       (fresh-ranges (build-fresh-ranges file-contents))
       (compare fresh-ranges))
  ;; Compress overlapping ranges
  (loop
    (unless (cdr compare) (return))
    (if (< (cdar compare) (caadr compare)) (setq compare (cdr compare))
        (progn
          (rplaca compare (cons (caar compare) (max (cdar compare) (cdadr compare))))
          (rplacd compare (cddr compare)))))
  (format t "There are ~a different fresh ingredient IDs." (reduce (lambda (a x) (+ 1 a (- (cdr x) (car x))))
                                                                   fresh-ranges
                                                                   :initial-value 0)))

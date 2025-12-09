#!/usr/bin/sbcl --script
(require :uiop)

;; Taken from Day 2 solutions
(defun split-line-by-character (sep line &optional collector)
  (let ((pos (position sep line :from-end t)))
    (cond (pos (split-line-by-character sep (subseq line 0 pos) (cons (subseq line (1+ pos)) collector)))
          (t (cons line collector)))))

(defun build-fresh-ranges (lines &optional fresh-range)
  (cond ((string= (car lines) "") fresh-range)
        (t (let ((range-ends (split-line-by-character #\- (car lines))))
             (build-fresh-ranges (cdr lines)
                                 (cons `(,(parse-integer (car range-ends)) .
                                         ,(parse-integer (cadr range-ends)))
                                       fresh-range))))))

(defun init-range-iter (start end)
  (let ((i (1- start)))
    (lambda () (if (< i end) (incf i)))))

(let* ((file-contents (uiop:read-file-lines (car (uiop:command-line-arguments))))
       (fresh-ranges (build-fresh-ranges file-contents))
       (fresh-collection (make-hash-table)))
  ;; dolist smashed the heap, trying a mutable loop variant.
  (loop
    (when (not fresh-ranges) (return))
    (let* ((range (pop fresh-ranges))
           (range-iter (init-range-iter (car range) (cdr range))))
      (loop
        (let ((i (funcall range-iter)))
          (if (not i) (return) (setf (gethash i fresh-collection) t))))))
  ;; I should eventually bother to learn format.
  (format t "There are ~a different fresh ingredient IDs." (hash-table-count fresh-collection)))

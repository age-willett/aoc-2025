#!/usr/bin/sbcl --script
(require :uiop)

(defun freshp (id fresh-range)
  ;;; Compare an id to the ends of the next item of fresh-range to determine whether id is fresh
  (cond ((not fresh-range) nil) ; Not fresh
        ((and (>= id (caar fresh-range)) (<= id (cdar fresh-range))) t) ; Is fresh
        (t (freshp id (cdr fresh-range))))) ; Go to the next range

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

(let* ((file-contents (uiop:read-file-lines (car (uiop:command-line-arguments))))
       (fresh-ranges (build-fresh-ranges file-contents))
       (test-ids (map 'list
                      #'parse-integer
                      (subseq file-contents (1+ (position-if (lambda (n) (string= "" n)) file-contents)))))
       (fresh-count 0))
  (dolist (id test-ids)
    (if (freshp id fresh-ranges) (incf fresh-count)))
  (print (concatenate 'string "There are " (write-to-string fresh-count) " fresh ingredients in the kitchen.")))

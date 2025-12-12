#!/usr/bin/sbcl --script
(require :uiop)

(defun string-to-char-list (s)
  (map 'list #'identity s))

(defun range (start &optional stop (step 1))
  (cond ((not stop) (loop for i from 0 to (1- start) collect i))
        (t (loop for i from start by step to (1- stop) collect i)))) 

(defun split-beam (s)
  (substitute #\| #\. s))

(defun propagate-lines (next comparator)
  ;;; Compare the NEXT set of lines with COMPARATOR, continuing beams and splitting when ^ is hit.
  (map-into next (lambda (n c) (if (not (eq n #\^)) c n)) next comparator)
  (let* ((len (length next))
         (collisions (mapcan (lambda (n c v) (if (and (eq n #\^) (eq c #\|)) `(,v)))
                             (string-to-char-list next)
                             (string-to-char-list comparator)
                             (range len)))) ; Pull down beams
    (dolist (c collisions (length collisions))
      (let ((left-bound (max 0 (1- c)))
            (right-bound (min len (+ 2 c))))
        (replace next (split-beam (subseq next left-bound right-bound)) :start1 left-bound :end1 right-bound)))))


(let* ((file-contents (uiop:read-file-lines (car (uiop:command-line-arguments))))
       (filtered-lines (mapcan (lambda (x) (if (not (string= "" (string-trim '(#\.) x))) (list x))) file-contents))
       (compare-line (substitute #\| #\S (pop filtered-lines)))
       (split-total 0))
  (loop while (car filtered-lines)
        do (progn (incf split-total (propagate-lines (car filtered-lines) compare-line))
                  (setq compare-line (substitute #\. #\^ (pop filtered-lines)))))
  (format t "The beams split ~a times.~%" split-total))

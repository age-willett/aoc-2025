#!/usr/bin/sbcl --script
(require :uiop)

;; Taken from Day 2 solutions, getting mileage, minor modification to chomp repeated separators.
(defun split-line-by-character (sep line &optional collector)
  (let* ((clean-line (string-trim `(,sep) line))
        (pos (position sep clean-line :from-end t)))
    (cond (pos (split-line-by-character sep
                                        (subseq clean-line 0 pos)
                                        (cons (subseq clean-line (1+ pos)) collector)))
          (t (cons clean-line collector)))))

(defun build-operator-lines (lines &optional op-list)
  (cond ((not lines) op-list)
        (t (build-operator-lines (cdr lines) (cons (split-line-by-character #\Space (car lines)) op-list)))))

(let* ((file-contents (uiop:read-file-lines (car (uiop:command-line-arguments))))
       (operator-operands (build-operator-lines file-contents))
       (equations (multiple-value-call #'mapcar #'list (values-list operator-operands)))
       (results))
  (dolist (equation equations (format t "The grand total of all answers is ~a." (apply #'+ results)))
    (push (apply (find-symbol (car equation)) (mapcar #'parse-integer (cdr equation))) results)))

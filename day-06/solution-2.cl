#!/usr/bin/sbcl --script
(require :uiop)

(defun split-equations (lines &optional op-sets)
  (let ((blank-line (member-if (lambda (x) (string= "" (string-trim '(#\Space) x))) lines)))
    (cond (blank-line (let ((pos (position (car blank-line) lines)))
                        (split-equations (subseq lines (1+ pos)) (cons (subseq lines 0 pos) op-sets))))
          (t (cons lines op-sets)))))

(defun build-operator-lines (lines)
  (let* ((op-sets (split-equations lines)))
    (dolist (equation op-sets op-sets)
      (rplacd equation (cons (subseq (car equation) 1) (cdr equation)))
      (rplaca equation (subseq (car equation) 0 1)))))

(let* ((file-contents (uiop:read-file-lines (car (uiop:command-line-arguments))))
       (rotated-list (map 'list (lambda (x) (coerce x 'string)) (multiple-value-call #'mapcar 'list (values-list (map 'list (lambda (x) (coerce x 'list)) (reverse file-contents))))))
       (equations (build-operator-lines rotated-list))
       (results))
  (dolist (equation equations (format t "The grand total of all answers is ~a." (apply #'+ results)))
    (push (apply (find-symbol (car equation)) (mapcar (lambda (x) (parse-integer (reverse x))) (cdr equation))) results))) ; HACK TODO hella screwed up on character order

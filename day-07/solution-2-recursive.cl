#!/usr/bin/sbcl --script
(require :uiop)

(defun trace-path (grid row &optional (depth 0) (acc 0))
  (let ((next-splitter (position #\^ (nth row grid) :start (1+ depth))))
    (cond ((not next-splitter) (+ acc 1))
          ((if (and (> row 0) (< row (1- (length grid))))
               (trace-path grid (1- row) next-splitter (trace-path grid (1+ row) next-splitter acc)))))))

(let* ((file-contents (uiop:read-file-lines (car (uiop:command-line-arguments))))
       (rotated-grid (apply #'mapcar #'list (mapcar (lambda (x) (map 'list #'identity x)) file-contents)))
       (start-row (position #\S rotated-grid :key #'car)))
  (format t "There are ~a paths the tachyon particle can take." (trace-path rotated-grid start-row)))

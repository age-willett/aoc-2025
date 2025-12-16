#!/usr/bin/sbcl --script
(require :uiop)

(defun split-line-by-character (sep line &optional collector)
  (let ((pos (position sep line :from-end t)))
    (cond (pos (split-line-by-character sep (subseq line 0 pos) (cons (subseq line (1+ pos)) collector)))
          (t (cons line collector)))))

(defun rect-area (p1 p2)
  (let ((p1x (car p1)) (p1y (cadr p1))
        (p2x (car p2)) (p2y (cadr p2)))
    (* (1+ (abs (- p2x p1x))) (1+ (abs (- p2y p1y))))))

(let* ((file-contents (uiop:read-file-lines (first (uiop:command-line-arguments))))
       (point-list (mapcar (lambda (x) (mapcar #'parse-integer (split-line-by-character #\, x))) file-contents))
       (anchor-point)
       (max-area 0))
  (loop while (setf anchor-point (pop point-list))
        do (loop for corner in point-list
                 do (setf max-area (max max-area (rect-area anchor-point corner)))))
  (print max-area))

;; 

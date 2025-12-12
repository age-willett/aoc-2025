#!/usr/bin/sbcl --script
(require :uiop)

(defun walk-paths (grid)
  (let ((paths-row (pop grid)))
    (if (not (car grid)) (apply #'+ paths-row)
        (progn (loop for i below (length paths-row)
                     do (if (numberp (nth i (car grid))) (incf (nth i (car grid)) (nth i paths-row))
                            (progn
                              (incf (nth (1- i) (car grid)) (nth i paths-row))
                              (incf (nth (1+ i) (car grid)) (nth i paths-row)))))
               (nsubstitute 0 #\^ (car grid))
               (walk-paths grid)))))

(let* ((file-contents (uiop:read-file-lines (car (uiop:command-line-arguments))))
       (cleaned-grid (mapcan (lambda (x) (if (or (position #\S x) (position #\^ x)) (list x))) 
                             (mapcar (lambda (x) (map 'list #'identity x)) file-contents))))
  (rplaca cleaned-grid (substitute 1 #\S (car cleaned-grid)))
  (dolist (line cleaned-grid) (nsubstitute 0 #\. line))
  (format t "There are ~a paths the tachyon particle can take.~%" (walk-paths cleaned-grid)))

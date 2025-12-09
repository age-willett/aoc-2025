#!/usr/bin/sbcl --script
(require :uiop)

(defun build-targets (grid loc)
  ;;; Build a list of valid surrounding targets given a grid and anchor point
  (let ((k (car loc))
        (l (cdr loc))
        (k-bound (array-dimension grid 0))
        (l-bound (array-dimension grid 1)))
    ;; Trying out loop here
    (loop for i in (map 'list (lambda (n) (+ k n)) '(-1 0 1))
          if (and (>= i 0) (> k-bound i))
            nconc (loop for j in (map 'list (lambda (n) (+ l n)) '(-1 0 1))
                        if (and (not (and (eq i k) (eq j l)))
                                (and (>= j 0) (> l-bound j)))
                          collect (cons i j)))))

(defun increase-heat-map (grid targets)
  ;;; Increase the heat map according to a target list
  (dolist (target targets)
    (let* ((i (car target))
           (j (cdr target))
           (v (aref grid i j)))
      (setf (aref grid i j) (1+ v)))))

(let* ((file-contents (uiop:read-file-lines (car (uiop:command-line-arguments))))
       (floor-grid (make-array `(,(length file-contents) ,(length (car file-contents)))
                               :initial-contents file-contents))
       (total-accessible 0)
       (k-bound (array-dimension floor-grid 0))
       (l-bound (array-dimension floor-grid 1)))
  (loop
    (let ((heat-map (make-array `(,(length file-contents) ,(length (car file-contents)))
                                :initial-element 0))
          (removed-rolls nil))
      ;; Build up the heat map by walking over the floor.
      (loop for i from 0 to (1- k-bound)
            do (loop for j from 0 to (1- l-bound)
                     if (eq (aref floor-grid i j) #\@)
                       do (increase-heat-map heat-map (build-targets heat-map `(,i . ,j)))))
      ;; Increase the count of accessible rolls by referencing the heat map and floor.
      (loop for i from 0 to (1- k-bound)
            do (loop for j from 0 to (1- l-bound)
                     if (and (eq (aref floor-grid i j) #\@)
                             (< (aref heat-map i j) 4))
                       do (progn (incf total-accessible)
                                 (setq removed-rolls t)
                                 (setf (aref floor-grid i j) #\.))))
      (unless removed-rolls (return))))
  
  (print (concatenate 'string "There are " (write-to-string total-accessible) " total rolls accessible.")))

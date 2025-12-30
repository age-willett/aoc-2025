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

(defun fill-poly-line (grid row &optional (pos 0))
  ;; Fill the inside of two lines by finding their 'edges'
  ;; Return the right bound if a line was filled or nil if not
  (let* ((left-bound (search "#." (aref grid row) :start2 pos))
         (right-bound (if left-bound (position #\# (aref grid row) :start (1+ left-bound)))))
    (if (and left-bound right-bound)
        (draw-horizontal-line grid row left-bound right-bound))
    right-bound))

(defun draw-vertical-line (grid col row-start row-end)
  (loop for row from row-start to row-end
        do (setf (elt (aref grid row) col) #\#)))

(defun draw-horizontal-line (grid row col-start col-end)
  (nsubstitute #\# #\. (aref grid row) :start col-start :end col-end))

(defun draw-line (grid coord1 coord2)
  (let ((x1 (car coord1))
        (x2 (car coord2))
        (y1 (cadr coord1))
        (y2 (cadr coord2)))
    (cond ((eq x1 x2) (draw-vertical-line grid x1 (min y1 y2) (max y1 y2))) ; X is eq, vertical line
          ((eq y1 y2) (draw-horizontal-line grid y1 (min x1 x2) (max x1 x2))) ; Y is eq, horizontal line
          (t (error "Diagonal coordinates are not implemented: ~a ~a" coord1 coord2)))))  ; Diagonal, unimplemented

(let* ((file-contents (uiop:read-file-lines (first (uiop:command-line-arguments))))
       (point-list (mapcar (lambda (x) (mapcar #'parse-integer (split-line-by-character #\, x))) file-contents))
       (grid-width (reduce #'max point-list :key #'car))
       (grid-height (reduce #'max point-list :key #'cadr))
       (tile-grid (make-array (1+ grid-height) :initial-element (make-string 1))))
  ;; Initialize tile-grid with unique strings
  (loop for row below (length tile-grid)
        do (setf (aref tile-grid row) (make-string (1+ grid-width) :initial-element #\.)))
  ;; Draw lines into tile grid
  (mapl (lambda (x) (draw-line tile-grid (car x) (or (cadr x) (car point-list)))) point-list)
  ;; 
  (loop for row below (length tile-grid)
        do (format t "~a~%" (aref tile-grid row))))

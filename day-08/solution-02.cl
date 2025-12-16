#!/usr/bin/sbcl --script
(require :uiop)

;; It's back again
(defun split-line-by-character (sep line &optional collector)
  (let ((pos (position sep line :from-end t)))
    (cond (pos (split-line-by-character sep (subseq line 0 pos) (cons (subseq line (1+ pos)) collector)))
          (t (cons line collector)))))

(defun calc-distance (coord1 coord2)
  (let ((x1 (first coord1)) (y1 (second coord1)) (z1 (third coord1))
        (x2 (first coord2)) (y2 (second coord2)) (z2 (third coord2)))
    (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2) (expt (- z1 z2) 2)))))

(defun merge-circuits (circuit-map node-a node-b)
  ;; Find the two circuits on the circuit map that contain node a and node b
  ;; then combine them together if they are not the same circuit
  (let (circuit-a circuit-b)
    ;; I had problems saving the circuits directly and modifying them
    ;; Had to use nth to properly reference what I wanted to modify
    (loop for i below (length circuit-map)
          do (if (find node-a (nth i circuit-map) :test #'equal) (setf circuit-a i))
             (if (find node-b (nth i circuit-map) :test #'equal) (setf circuit-b i)))
    (if (not (= circuit-a circuit-b))
        (loop while (nth circuit-b circuit-map)
              do (push (pop (nth circuit-b circuit-map)) (nth circuit-a circuit-map))))))

(let* ((file-contents (uiop:read-file-lines (first (uiop:command-line-arguments))))
       (junction-list (mapcar (lambda (x) (mapcar #'parse-integer (split-line-by-character #\, x))) file-contents))
       (circuit-map (mapcar #'list (copy-list junction-list)))
       (distance-pairs)
       (next-junction)
       (node-a)
       (node-b))
  ;; Build up the distances then cull down to parameter count
  (loop while junction-list
        do (setf next-junction (pop junction-list))
           (dolist (compare-junction junction-list)
             (push (list (calc-distance next-junction compare-junction) next-junction compare-junction)
                   distance-pairs)))


  (sort distance-pairs #'> :key #'car)
  (setf distance-pairs (reverse distance-pairs)) ;; nreverse had the same issue as sort <
  ;; Walk the distance list and join junctions together
  (dolist (pair distance-pairs)
    (setf node-a (cadr pair) node-b (caddr pair))
    (merge-circuits circuit-map node-a node-b)
    (setf circuit-map (remove nil circuit-map))
    (if (= (length circuit-map) 1) (return)))
  (format t "Multiplying the x coordinates of the two nodes forming a complete network equals ~a."
          (* (car node-a) (car node-b))))

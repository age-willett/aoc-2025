#!/usr/bin/sbcl --script
(require :uiop)

;; Can be replaced by uiop:read-file-lines in the future?
(defun read-file-by-lines (filename)
  (let ((file-contents (list)))
    (with-open-file (s filename)
      (do ((l (read-line s) (read-line s nil)))
          ((eq l nil) (reverse file-contents))
        (push l file-contents)))))

(defun split-line-by-character (sep line &optional collector)
  (let ((pos (position sep line :from-end t)))
    (cond (pos (split-line-by-character sep (subseq line 0 pos) (cons (subseq line (1+ pos)) collector)))
          (t (cons line collector)))))

(defun create-list-by-range (start end &optional collector)
  ;;; Creates a range from beginning to end, inclusive
  (cond ((> end start) (create-list-by-range start (1- end) (cons end collector)))
        (t (cons end collector))))

(defun validate-id (id)
  ;;; Tests an id string to see if it is valid (isn't the same number twice)
  (cond ((oddp (length id)) t) ; Odd length numbers aren't ever invalid
        (t (let ((midpoint (/ (length id) 2)))
             (if (string= (subseq id 0 midpoint) (subseq id midpoint)) nil t)))))

(let* ((filename (first (uiop:command-line-arguments)))
       (file-contents (read-file-by-lines filename))
       (id-ranges (split-line-by-character #\, (first file-contents)))
       (invalid-ids (list)))
  (dolist (range id-ranges)
    (let* ((pos (position #\- range))
           (range-start (parse-integer (subseq range 0 pos)))
           (range-end (parse-integer (subseq range (1+ pos))))
           (range-list (create-list-by-range range-start range-end)))
      (dolist (tenative-id range-list)
        (if (not (validate-id (write-to-string tenative-id))) (push tenative-id invalid-ids)))))
  (print (reduce #'+ invalid-ids)))

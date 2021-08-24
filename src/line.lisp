;; Lines are extendable vectors of characters which represent a line of text.
;; They do not include the newline character ending the line nor any other
;; string terminator.

(in-package #:typewriter)

(defparameter *initial-line-size* 80
  "Initial size of lines created by the make-line function.")

(defun make-line (&optional arg)
  "Make an empty line.

The single optional parameter is ignored, this allows it to be used with the map
function to map a sequence of anything to empty lines."
  (declare (ignore arg))
  (make-array *initial-line-size* :adjustable t
                                  :element-type 'character
                                  :fill-pointer 0))

(defun string-to-line (string)
  "Convert a string to a line."
  (let ((line (make-line)))
    (loop for char across string
          do (vector-push-extend char line))
    line))

(defun line-to-string (line)
  "Convert a line to a string."
  (coerce line 'string))

(defun line-blank-p (line)
  "Determine if a line is blank, that is, if it contains only spaces."
  (let ((is-blank t))
    (loop for char across line
          while is-blank
          do (unless (eql char #\space)
               (setq is-blank nil)))
    is-blank))

(defun trim-trailing-whitespace (line)
  "Trim all whitespace from the end of a line."
  (let ((last-char -1)
        (walk 0))
    (loop for char across line
          do (unless (eql char #\space)
               (setq last-char walk))
             (incf walk))
    (subseq line 0 (1+ last-char))))

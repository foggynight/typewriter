;; There are two categories of cursor in this program: text and screen cursors.
;;
;; Text cursors are used to identify the point in a page that will be affected
;; by user input. In this program the page moves rather than the cursor, so text
;; cursors do not move across the screen like traditional cursors.
;;
;; There is also a single screen cursor, which is handled by ncurses and moves
;; across the screen during draw operations, but is effectively always located
;; at the center of the screen from the user's perspective.

(in-package :typewriter)

(defclass cursor ()
  ((y
    :accessor y
    :initarg :y
    :documentation
    "Index of the line currently containing the cursor.")
   (x
    :accessor x
    :initarg :x
    :documentation
    "Index of the character within its containing line on which the cursor is
located."))
  (:documentation
   "Cursor class used to identify the location on a page that will be affected
by user input."))

(defmethod move ((object cursor) direction &optional (n 1))
  "Move a cursor in the direction specified by the direction keyword.

Optionally, an argument may be passed for n, which represents the number of
spaces to move the cursor in the given direction; the default is one."
  (flet ((multiply (x) (* n x)))
    (let* ((dir (crt:get-direction direction))
           (offset (if (> n 1)
                       (mapcar #'multiply dir)
                       dir)))
      (setf (y object) (+ (y object) (car offset)))
      (when (< (y object) 0) (setf (y object) 0))
      (setf (x object) (+ (x object) (cadr offset)))
      (when (< (x object) 0) (setf (x object) 0)))))

(defmethod newline ((object cursor))
  "Move a cursor to the beginning of the next line."
  (setf (y object) (1+ (y object)))
  (setf (x object) 0))

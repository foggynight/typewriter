;;;
;; --- typewriter.lisp ---
;;
;; Typewriter inspired text editor.
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the GPLv2 license
;;;

(require :croatoan)
(require :uiop)

;;; CONFIG SECTION -------------------------------------------------------------

(defparameter *initial-line-size* 81
  "Initial size of lines created by the make-line function.")

(defparameter *slide-width* 4
  "Number of characters to slide the text cursor on slide commands.")

;;; LINE SECTION ---------------------------------------------------------------
;;
;; Lines are extendable vectors of characters which represent a line of text.
;; They do not include the newline character ending the line nor any other
;; string terminator.

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

;;; CURSOR SECTION -------------------------------------------------------------
;;
;; There are two categories of cursor in this program: text and screen cursors.
;;
;; Text cursors are used to identify the point in a page that will be affected
;; by user input. In this program the page moves rather than the cursor, so text
;; cursors do not move across the screen like traditional cursors.
;;
;; There is also a single screen cursor, which is handled by ncurses and moves
;; across the screen during draw operations, but is effectively always located
;; at the center of the screen from the user's perspective.

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
located.")
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

;;; PAGE SECTION ---------------------------------------------------------------
;;
;; Pages are a representation of files, they contain a text buffer which is a
;; list of lines. All pages are associated with a file where their text content
;; is read from/written to.

(defclass page ()
  ((text-buffer
    :accessor text-buffer
    :documentation
    "Text contained within the page represented as a list of lines."))
  (:documentation
   "Page class representing a file."))

(defmethod line-count ((object page))
  "Get the number of lines contained within the text buffer of a page."
  (length (text-buffer object)))

(defmethod add-lines ((object page) line-list)
  "Add a list of lines to the end of the text buffer of a page."
  (setf (text-buffer object) (append (text-buffer object)
                                     line-list)))

(defmethod add-char ((object page) char y x)
  "Add a character at the given y-x position of a page.

Any character located at the y-x position before calling add-char is
overwritten."
  (let ((page-line-count (line-count object)))
    (unless (> page-line-count y)
      (let ((new-line-list (make-list (1+ (- y page-line-count)))))
        (setq new-line-list (map 'list #'make-line new-line-list))
        (add-lines object new-line-list))))
  (let ((line (car (nthcdr y (text-buffer object)))))
    (if (> (length line) x)
        (setf (aref line x) char)
        (progn (loop while (< (length line) x)
                     do (vector-push-extend #\space line))
               (vector-push-extend char line)))))

;;; FILE SECTION ---------------------------------------------------------------

(defun read-page-from-file (filename)
  "Create a new page with its text buffer filled with the contents of the file
named filename."
  (let ((page (make-instance 'page)))
    (with-open-file (stream filename :if-does-not-exist :create)
      (setf (text-buffer page)
            (loop for line = (read-line stream nil)
                  while line
                  collect (string-to-line line))))
    page))

(defun write-page-to-file (filename page)
  "Write the text content of a page to a file named filename."
  (with-open-file (stream filename :direction :output
                                   :if-exists :supersede)
    (dolist (line (text-buffer page))
      (format stream "~A~%" line))))

;;; SCREEN SECTION -------------------------------------------------------------

(defun screen-center-cursor (scr)
  "Move the screen cursor to the center of the screen."
  (apply #'crt:move (cons scr (crt:center-position scr))))

(defun screen-newline (scr start-x)
  "Move the screen cursor to the start-x'th column of the next row."
  (let* ((pos (crt:cursor-position scr))
         (y (car pos)))
    (crt:move scr (1+ y) start-x)))

(defun screen-draw-page (scr cursor page)
  "Draw a page such that the screen is centered on the text cursor."
  (let* ((center (crt:center-position scr))
         (center-y (car center))
         (center-x (cadr center))
         (start-x (- center-x (x cursor))))
    (crt:save-excursion scr
      (crt:clear scr)
      (crt:move scr
                (- center-y (y cursor))
                start-x)
      (dolist (line (text-buffer page))
        (crt:add scr line)
        (screen-newline scr start-x)))))

;;; MAIN SECTION ---------------------------------------------------------------

(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (filename (car args))
         (cursor (make-instance 'cursor :y 0 :x 0))
         (page nil))
    (when (or (< (length args) 1)
              (> (length args) 1))
      (format t "typewriter: Wrong number of arguments~%Usage: typewriter FILENAME~%")
      (exit))
    (setq page (read-page-from-file filename))
    (crt:with-screen (scr :input-echoing nil
                          :process-control-chars nil)
      (screen-center-cursor scr)

      ;;; -- <CTRL> Command Events --
      (crt:bind scr #\ 'crt:exit-event-loop)
      (crt:bind scr #\
                (lambda (w e)
                  (declare (ignore w e))
                  (write-page-to-file filename page)))

      ;;; -- Movement Events --
      (macrolet ((bind (key direction &optional (n 1))
                   `(crt:bind scr ,key
                              (lambda (w e)
                                (declare (ignore w e))
                                (if (> ,n 1)
                                    (move cursor ,direction ,n)
                                    (move cursor ,direction))
                                (screen-draw-page scr cursor page)))))
        (bind :backspace :left)
        (bind #\tab :right *slide-width*)
        (bind :btab :left *slide-width*)
        (bind :up :up)
        (bind :down :down)
        (bind :left :left)
        (bind :right :right))
      (crt:bind scr #\newline
                (lambda (w e)
                  (declare (ignore w e))
                  (newline cursor)
                  (screen-draw-page scr cursor page)))

      ;;; -- Window Events --
      (crt:bind scr :resize
                (lambda (w e)
                  (declare (ignore w e))
                  (screen-center-cursor scr)
                  (screen-draw-page scr cursor page)))

      ;;; -- Print Events --
      ;; For any unbound character: if the character is a standard character,
      ;; add it to the page and move the cursor, otherwise do nothing.
      (crt:bind scr t
                (lambda (w e)
                  (declare (ignore w))
                  (when (standard-char-p e)
                    (add-char page e (y cursor) (x cursor))
                    (move cursor :right)
                    (screen-draw-page scr cursor page))))

      (screen-draw-page scr cursor page)
      (crt:run-event-loop scr))))

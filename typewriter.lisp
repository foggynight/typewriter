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

(defparameter *initial-line-size* 81)

(defparameter *slide-width* 4)

;;; LINE SECTION ---------------------------------------------------------------

(defun make-line (&optional arg)
  (declare (ignore arg))
  (make-array *initial-line-size* :adjustable t
                                  :element-type 'character
                                  :fill-pointer 0))

(defun string-to-line (string)
  (let ((line (make-line)))
    (loop for char across string
          do (vector-push-extend char line))
    line))

;;; CURSOR SECTION -------------------------------------------------------------

(defclass cursor ()
  ((y :accessor y
      :initarg :y)
   (x :accessor x
      :initarg :x)))

(defmethod move ((object cursor) direction &optional (n 1))
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
  (setf (y object) (1+ (y object)))
  (setf (x object) 0))

;;; PAGE SECTION ---------------------------------------------------------------

(defclass page ()
  ((text-buffer :accessor text-buffer)))

(defmethod line-count ((object page))
  (length (text-buffer object)))

(defmethod add-lines ((object page) line-list)
  (setf (text-buffer object) (append (text-buffer object)
                                     line-list)))

(defmethod add-char ((object page) char y x)
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
  (let ((page (make-instance 'page)))
    (with-open-file (stream filename :if-does-not-exist :create)
      (setf (text-buffer page)
            (loop for line = (read-line stream nil)
                  while line
                  collect (string-to-line line))))
    page))

(defun write-page-to-file (filename page)
  (with-open-file (stream filename :direction :output
                                   :if-exists :supersede)
    (dolist (line (text-buffer page))
      (format stream "~A~%" line))))

;;; SCREEN SECTION -------------------------------------------------------------

(defun screen-center-cursor (scr)
  (apply #'crt:move (cons scr (crt:center-position scr))))

(defun screen-newline (scr start-x)
  (let* ((pos (crt:cursor-position scr))
         (y (car pos)))
    (crt:move scr (1+ y) start-x)))

(defun screen-draw-page (scr cursor page)
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

      ;;; -- Control Command Events --
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

      ;;; -- Print Events --
      ;; For any unbound character: add that character to the page at the cursor
      ;; position, and move the cursor position forward.
      (crt:bind scr t
                (lambda (w e)
                  (declare (ignore w))
                  (add-char page e (y cursor) (x cursor))
                  (move cursor :right)
                  (screen-draw-page scr cursor page)))

      (screen-draw-page scr cursor page)
      (crt:run-event-loop scr))))

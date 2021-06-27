;;;
;; --- typewriter.lisp ---
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the GPLv2 license
;;;

(require :croatoan)

;;; CONFIG SECTION -------------------------------------------------------------

(defparameter *initial-line-size* 2)

(defparameter *slide-size* 4)

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

;;: PAGE SECTION ---------------------------------------------------------------

(defun add-char (text-buf char y x)
  (let ((text-buf-len (length text-buf)))
    (unless (> text-buf-len y)
      (let ((new-lines (make-list (1+ (- y text-buf-len)))))
        (setq new-lines (map 'list #'make-line new-lines))
        (setq text-buf (append text-buf new-lines)))))
  (let ((line (car (nthcdr y text-buf))))
    (if (> (length line) x)
        (setf (aref line x) char)
        (progn (loop while (< (length line) x)
                     do (vector-push-extend #\space line))
               (vector-push-extend char line))))
  text-buf)

;;: FILE SECTION ---------------------------------------------------------------

(defun read-page-from-file (filename)
  (with-open-file (stream filename :if-does-not-exist :create)
    (loop for line = (read-line stream nil)
          while line
          collect (string-to-line line))))

(defun write-page-to-file (filename text-buf)
  (let ((out-file (open filename :direction :output
                                 :if-exists :supersede)))
    (dolist (line text-buf)
      (format out-file "~A~%" line))
    (close out-file)))

;;; SCREEN SECTION -------------------------------------------------------------

(defun cursor-newline (scr)
  (let* ((pos (crt:cursor-position scr))
         (y (car pos)))
    (crt:move scr (1+ y) 0)))

(defun draw-page (scr text-buf)
  (crt:save-excursion scr
    (crt:move scr 0 0)
    (dolist (line text-buf)
      (when (> (length line) 0)
        (crt:add scr line)
        (cursor-newline scr)))
    (crt:refresh scr)))

;;; MAIN SECTION ---------------------------------------------------------------

(defun main ()
  (let ((text-buf (read-page-from-file "test.txt")))
    (crt:with-screen (scr :input-echoing nil)
      (crt:bind scr #\esc 'exit-event-loop)
      (crt:bind scr :backspace
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :left)
                  (draw-page scr text-buf)))
      (crt:bind scr #\tab
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :right *slide-size*)
                  (draw-page scr text-buf)))
      (crt:bind scr :btab
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :left *slide-size*)
                  (draw-page scr text-buf)))
      (crt:bind scr :up
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :up)
                  (draw-page scr text-buf)))
      (crt:bind scr :down
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :down)
                  (draw-page scr text-buf)))
      (crt:bind scr :left
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :left)
                  (draw-page scr text-buf)))
      (crt:bind scr :right
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :right)
                  (draw-page scr text-buf)))
      (crt:bind scr #\newline
                (lambda (w e)
                  (declare (ignore w e))
                  (cursor-newline scr)
                  (draw-page scr text-buf)))
      (crt:bind scr t
                (lambda (w e)
                  (declare (ignore w))
                  (let* ((pos (crt:cursor-position scr))
                         (y (car pos))
                         (x (cadr pos)))
                    (setq text-buf (add-char text-buf e y x)))
                  (crt:move-direction scr :right)
                  (draw-page scr text-buf)))
      (draw-page scr text-buf)
      (crt:run-event-loop scr))))

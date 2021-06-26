;;;
;; --- typewriter.lisp ---
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the GPLv2 license
;;;

(require :croatoan)

(defparameter *initial-line-size* 2)

(defparameter *slide-size* 4)

(defun init-line (&optional arg)
  (declare (ignore arg))
  (make-array *initial-line-size* :adjustable t
                                  :element-type 'character
                                  :fill-pointer 0))

(defun add-char (text-buf char y x)
  (let ((text-buf-len (length text-buf)))
    (unless (> text-buf-len y)
      (let ((new-lines (make-list (1+ (- y text-buf-len)))))
        (setq new-lines (map 'list #'init-line new-lines))
        (setq text-buf (append text-buf new-lines)))))
  (let ((line (car (nthcdr y text-buf))))
    (if (> (length line) x)
        (setf (aref line x) char)
        (progn (loop while (< (length line) x)
                     do (vector-push-extend #\space line))
               (vector-push-extend char line))))
  text-buf)

(defun cursor-newline (scr)
  (let* ((pos (crt:cursor-position scr))
         (y (car pos)))
    (crt:move scr (1+ y) 0)))

(defun draw-page (text-buf scr)
  (crt:save-excursion scr
    (crt:move scr 0 0)
    (dolist (line text-buf)
      (when (> (length line) 0)
        (crt:add scr line)
        (cursor-newline scr)))
    (crt:refresh scr)))

(defun write-page-to-file (text-buf name)
  )

(defun main ()
  (let ((text-buf '()))
    (crt:with-screen (scr :input-echoing nil)
      (crt:bind scr #\esc 'exit-event-loop)
      (crt:bind scr :backspace
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :left)
                  (draw-page text-buf scr)))
      (crt:bind scr #\tab
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :right *slide-size*)
                  (draw-page text-buf scr)))
      (crt:bind scr :btab
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :left *slide-size*)
                  (draw-page text-buf scr)))
      (crt:bind scr :up
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :up)
                  (draw-page text-buf scr)))
      (crt:bind scr :down
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :down)
                  (draw-page text-buf scr)))
      (crt:bind scr :left
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :left)
                  (draw-page text-buf scr)))
      (crt:bind scr :right
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :right)
                  (draw-page text-buf scr)))
      (crt:bind scr #\newline
                (lambda (w e)
                  (declare (ignore w e))
                  (cursor-newline scr)
                  (draw-page text-buf scr)))
      (crt:bind scr t
                (lambda (w e)
                  (declare (ignore w))
                  (let* ((pos (crt:cursor-position scr))
                         (y (car pos))
                         (x (cadr pos)))
                    (setq text-buf (add-char text-buf e y x)))
                  (crt:move-direction scr :right)
                  (draw-page text-buf scr)))
      (draw-page text-buf scr)
      (crt:run-event-loop scr))))

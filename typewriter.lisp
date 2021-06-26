;;;
;; --- typewriter.lisp ---
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the GPLv2 license
;;;

(require :croatoan)

(defparameter *initial-line-size* 2)

(defparameter *slide-size* 4)

(defun init-line ()
  (make-array *initial-line-size* :adjustable t
                                  :element-type 'character
                                  :fill-pointer 0))

(defun add-char (text-buf char y x)
  (let ((line (car (nthcdr y text-buf))))
    (if (> (length line) x)
        (setf (aref line x) char)
        (vector-push-extend char line)))
  text-buf)

(defun draw-page (text-buf scr)
  (crt:save-excursion scr
                      (crt:move scr 0 0)
                      (dolist (line text-buf)
                        (when (> (length line) 0)
                          (crt:add scr line)
                          (crt:move-direction scr :down)))
                      (crt:refresh scr)))

(defun write-page-to-file (text-buf name)
  )

(defun main ()
  (let ((text-buf (list (init-line))))
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
                  (crt:move-direction scr :down)
                  (draw-page text-buf scr)))

      (crt:bind scr t
                (lambda (w e)
                  (declare (ignore w))
                  (add-char text-buf e)
                  (draw-page text-buf scr)))

      (draw-page text-buf scr)
      (crt:run-event-loop scr))))

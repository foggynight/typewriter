;;;
;; --- typewriter.lisp ---
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the GPLv2 license
;;;

(require :croatoan)

(defparameter *initial-line-size* 81)

(defparameter *slide-size* 4)

(defun add-char (text-buf char)
  )

(defun del-char (text-buf)
  )

(defun draw-page (text-buf scr)
  (crt:save-excursion scr
    (crt:move scr 0 0)
    (dolist (line text-buf)
      (crt:add scr line))
    (crt:refresh scr)))

(defun write-page-to-file (text-buf name)
  )

(defun main ()
  (let ((text-buf '("This is a test.")))
    (crt:with-screen (scr :input-echoing nil)

      (crt:bind scr #\esc 'exit-event-loop)

      (crt:bind scr #\space
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :right)
                  (draw-page text-buf scr)))

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

      (crt:bind scr t
                (lambda (w e)
                  (declare (ignore w))
                  (add-char text-buf e)
                  (draw-page text-buf scr)))

      (draw-page text-buf scr)
      (crt:run-event-loop scr))))

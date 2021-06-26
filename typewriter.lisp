;;;
;; --- typewriter.lisp ---
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the GPLv2 license
;;;

(require :croatoan)

(defparameter *initial-line-size* 81)

(defun add-char (text-buf char)
  )

(defun del-char (text-buf)
  )

(defun draw-page (text-buf scr)
  (crt:move scr 0 0)
  (dolist (line text-buf)
    (crt:add scr line))
  (crt:refresh scr))

(defun write-page-to-file (text-buf name)
  )

(defun main ()
  (let ((text-buf `(,(format nil "This is a test.~%")
                    ,(format nil "This is also a test.~%"))))
    (crt:with-screen (scr :input-echoing nil)

      (crt:bind scr #\esc 'exit-event-loop)

      (crt:bind scr '(#\space :backspace #\tab :btab)
                (lambda (w e)
                  (declare (ignore w e))
                  (draw-page text-buf scr)))

      (crt:bind scr t
                (lambda (w e)
                  (declare (ignore w))
                  (add-char text-buf e)
                  (draw-page text-buf scr)))

      (crt:run-event-loop scr))))

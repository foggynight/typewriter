(in-package :typewriter)

(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (filename (car args))
         (cursor (make-instance 'cursor :y 0 :x 0))
         (page nil))
    (when (or (< (length args) 1)
              (> (length args) 1))
      (format t "typewriter: Wrong number of arguments~%~
                 Usage: typewriter FILENAME~%")
      (sb-ext:exit))
    (setq page (read-page-from-file filename))
    (crt:with-screen (scr :input-echoing nil
                          :process-control-chars nil)

      ;;; <CTRL> Command Events
      (crt:bind scr #\ 'crt:exit-event-loop)
      (crt:bind scr #\
                (lambda (w e)
                  (declare (ignore w e))
                  (write-page-to-file filename page)))

      ;;; Movement Events
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

      ;;; Window Events
      (crt:bind scr :resize
                (lambda (w e)
                  (declare (ignore w e))
                  (screen-center-cursor scr)
                  (screen-draw-page scr cursor page)))

      ;;; Print Events
      ;; For any unbound character: if the character is a standard character,
      ;; add it to the page and move the cursor, otherwise do nothing.
      (crt:bind scr t
                (lambda (w e)
                  (declare (ignore w))
                  (when (standard-char-p e)
                    (add-char page e (y cursor) (x cursor))
                    (move cursor :right)
                    (screen-draw-page scr cursor page))))

      (screen-center-cursor scr)
      (screen-draw-page scr cursor page)
      (crt:run-event-loop scr))))

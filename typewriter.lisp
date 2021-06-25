;;;
;; --- typewriter.lisp ---
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the GPLv2 license
;;;

(require :croatoan)

(defun main ()
  (crt:with-screen (scr :input-echoing nil)
    (ncurses:addstr "Hello, world!")
    (crt:bind scr #\ 'exit-event-loop)
    (crt:run-event-loop scr)))

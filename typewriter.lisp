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

(defun add-char (page char y x)
  (let ((page-len (length page)))
    (unless (> page-len y)
      (let ((new-lines (make-list (1+ (- y page-len)))))
        (setq new-lines (map 'list #'make-line new-lines))
        (setq page (append page new-lines)))))
  (let ((line (car (nthcdr y page))))
    (if (> (length line) x)
        (setf (aref line x) char)
        (progn (loop while (< (length line) x)
                     do (vector-push-extend #\space line))
               (vector-push-extend char line))))
  page)

;;: FILE SECTION ---------------------------------------------------------------

(defun read-page-from-file (filename)
  (with-open-file (stream filename :if-does-not-exist :create)
    (loop for line = (read-line stream nil)
          while line
          collect (string-to-line line))))

(defun write-page-to-file (filename page)
  (with-open-file (stream filename :direction :output
                                   :if-exists :supersede)
    (dolist (line page)
      (format stream "~A~%" line))))

;;; SCREEN SECTION -------------------------------------------------------------

(defun cursor-newline (scr)
(defun screen-center-cursor (scr)
  (apply #'crt:move (cons scr (crt:center-position scr))))
  (let* ((pos (crt:cursor-position scr))
         (y (car pos)))
    (crt:move scr (1+ y) 0)))

(defun draw-page (scr page)
  (crt:save-excursion scr
    (crt:move scr 0 0)
    (dolist (line page)
      (crt:add scr line)
      (cursor-newline scr))
    (crt:refresh scr)))

;; TODO Add move-command function (or macro?)

;;; MAIN SECTION ---------------------------------------------------------------

(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (filename (car args))
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
      ;; Quit the program
      (crt:bind scr #\ 'crt:exit-event-loop)
      ;; Save page to file
      (crt:bind scr #\
                (lambda (w e)
                  (declare (ignore w e))
                  (write-page-to-file filename page)))

      ;;; -- Movement Events --
      (crt:bind scr :backspace
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :left)
                  (draw-page scr page)))
      (crt:bind scr #\tab
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :right *slide-size*)
                  (draw-page scr page)))
      (crt:bind scr :btab
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :left *slide-size*)
                  (draw-page scr page)))
      (crt:bind scr :up
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :up)
                  (draw-page scr page)))
      (crt:bind scr :down
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :down)
                  (draw-page scr page)))
      (crt:bind scr :left
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :left)
                  (draw-page scr page)))
      (crt:bind scr :right
                (lambda (w e)
                  (declare (ignore w e))
                  (crt:move-direction scr :right)
                  (draw-page scr page)))
      (crt:bind scr #\newline
                (lambda (w e)
                  (declare (ignore w e))
                  (cursor-newline scr)
                  (draw-page scr page)))

      ;;; -- Print Events --
      ;; For any unbound character: add that character to the page at the cursor
      ;; position, and move the cursor position forward.
      (crt:bind scr t
                (lambda (w e)
                  (declare (ignore w))
                  (let* ((pos (crt:cursor-position scr))
                         (y (car pos))
                         (x (cadr pos)))
                    (setq page (add-char page e y x)))
                  (crt:move-direction scr :right)
                  (draw-page scr page)))

      (draw-page scr page)
      (crt:run-event-loop scr))))

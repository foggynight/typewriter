;; Pages are a representation of files, they contain a text buffer which is a
;; list of lines. All pages are associated with a file where their text content
;; is read from/written to.

(in-package #:typewriter)

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
  (setf (text-buffer object) (append (text-buffer object) line-list)))

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

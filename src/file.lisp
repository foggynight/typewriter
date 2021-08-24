(in-package #:typewriter)

(defun read-page-from-file (filename)
  "Create a new page with its text buffer filled with the contents of the file
named filename."
  (let ((page (make-instance 'page)))
    (with-open-file (stream filename :if-does-not-exist nil)
      (if stream
          (setf (text-buffer page)
                (loop for line = (read-line stream nil)
                      while line
                      collect (string-to-line line)))
          (setf (text-buffer page) '())))
    page))

(defun write-page-to-file (filename page)
  "Write the text content of a page to a file named filename.

Before writing, text content is stripped of trailing empty lines and the
remaining lines are stripped of trailing whitespace."
  (let ((out-lines (text-buffer page)))
    ;; Remove trailing empty lines
    (let ((last-line -1)
          (walk 0))
      (dolist (line out-lines)
        (unless (line-blank-p line)
          (setq last-line walk))
        (incf walk))
      (setq out-lines (subseq out-lines 0 (1+ last-line))))
    ;; Remove trailing whitespace
    (dotimes (i (length out-lines))
      (setf (nth i out-lines) (trim-trailing-whitespace (nth i out-lines))))
    (with-open-file (stream filename :direction :output
                                     :if-exists :supersede)
      (dolist (line out-lines)
        (format stream "~A~%" line)))))

(in-package :typewriter)

(defun screen-center-cursor (scr)
  "Move the screen cursor to the center of the screen."
  (apply #'crt:move (cons scr (crt:center-position scr))))

(defun screen-newline (scr start-x)
  "Move the screen cursor to the start-x'th column of the next row."
  (let* ((pos (crt:cursor-position scr))
         (y (car pos)))
    (crt:move scr (1+ y) start-x)))

(defun screen-draw-line (scr cursor line)
  "Draw a line at the current position of the screen cursor, leaving the screen
cursor unmoved."
  (when (> (length line) 0)
    (let* ((width (crt:width scr))
           (win-center (crt:center-position scr))
           (center-x (cadr win-center))
           (first-char (- (x cursor) center-x))
           (char-count (if (< first-char 0)
                           (+ width first-char)
                           width)))
      ;; The screen cursor must be returned to its previous position after
      ;; drawing a line, otherwise lines that run to the end of the page will
      ;; cause the cursor to end up on the next line, causing the following line
      ;; to be drawn in the wrong position.
      (crt:save-excursion scr
        (when (< first-char 0)
          (crt:move-direction scr :right (- first-char))
          (setq first-char 0))
        (when (< first-char (length line))
          (let ((line-to-draw (subseq line first-char)))
            (when (< char-count (length line-to-draw))
              (setq line-to-draw (subseq line-to-draw 0 char-count)))
            (crt:add scr (line-to-string line-to-draw))))))))

(defun screen-draw-page (scr cursor page)
  "Draw a page with its text content positioned such that the screen is centered
on the text cursor."
  (let* ((height (crt:height scr))
         (win-center (crt:center-position scr))
         (center-y (car win-center))
         (first-line (- (y cursor) center-y))
         (line-count (if (< first-line 0)
                         (+ height first-line)
                         height)))
    (crt:save-excursion scr
      (crt:clear scr)
      (loop while (< first-line 0)
            do (screen-newline scr 0)
               (incf first-line))
      (when (< first-line (line-count page))
        (let ((lines-to-draw (if (> first-line 0)
                                 (subseq (text-buffer page) first-line)
                                 (text-buffer page))))
          (when (< line-count (length lines-to-draw))
            (setq lines-to-draw (subseq lines-to-draw 0 line-count)))
          (dolist (line lines-to-draw)
            (screen-draw-line scr cursor line)
            (screen-newline scr 0)))))))

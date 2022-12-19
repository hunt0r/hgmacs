;; General purpose functions
(defun hgmacs-split-window-three-horizontal()
  (interactive)
  (split-window-right)
  (split-window-right)
  (balance-windows))

;; Create custom functions used for auto-creating new buffers
(defun hgmacs-first-unused-index (base-string)
  "Return the first unused index (for a unique buffer name) counting from 0.

For example, return 0 if no buffer starts with base-string0.
 return 1 if base-string0* buffer exists, but not base-string1*.
 return 2 if base-string0* and base-string1* buffers exist, but not base-string2*.
 return 3 if 0, 1, 2 but not 3... (you get the idea)"
  (let ((i 0))
    (while (hgmacs-some-buffer-starts-with-this-p (concat base-string (number-to-string i)))
      (setq i (1+ i)))
    i))

(defun hgmacs-some-buffer-starts-with-this-p (string)
  "Return t if a buffer name starts with STRING. (Return nil otherwise.)"
  (let ((rtnval nil)
        (bufferlist (buffer-list)))
    (while bufferlist
      (if (string-match string (buffer-name (car bufferlist)))
          (progn
            (setq rtnval t)
            (setq bufferlist ())
            )
        (setq bufferlist (cdr bufferlist))
        )
      )
    rtnval))

(defun new-scratch-buffer ()
  "Launch a new scratch buffer and automatically name it"
  (interactive)
  (let ((prefix "*scratch"))
    (switch-to-buffer (concat prefix (number-to-string (hgmacs-first-unused-index prefix)) "*"))))

(defun hgmacs-open-scratch-file ()
  "Visit org-mode scratch file"
  (interactive)
  (find-file org-default-notes-file))


(defun hgmacs-get-line-stops ()
  "Return stop spots of the current line

Some 'stop spots':
1) the actual line beginning
2) the start of any non-whitespace (could be a comment delimiter)
3) the start of the meaningful text (e.g. comment body)"
  (save-excursion
    (let ((stops ())
          (bol (progn (move-beginning-of-line nil) (point)))
          (crux-start (progn (crux-move-to-mode-line-start) (point)))
          (beginning-of-comment (progn (looking-at (or comment-start-skip "")) (goto-char (match-end 0)))))
      (push bol stops)
      (cl-pushnew crux-start stops)
      (cl-pushnew beginning-of-comment stops)
      (reverse stops))))

(defun hgmacs-move-back-to-prev-stop ()
  "Move to the nearest stop before current point."
  (let ((stops (hgmacs-get-line-stops))
        (start-point (point))
        (final-point 0))
    (while (and (< final-point start-point) stops)
      (setq final-point (pop stops))
      (if (< final-point start-point) (goto-char final-point)))))

(defun hgmacs-move-to-final-stop ()
  "Move to the furthest-in-buffer stop."
  (goto-char (apply 'max (hgmacs-get-line-stops))))


(defun hgmacs-move-beginning-of-line ()
  "Move point to line start, which can mean various things.

This will toggle between these with repeated presses.
It is an extension of crux-move-beginning-of-line that does what I want for comments."
  (interactive)
  (let ((orig-point (point)))
    (hgmacs-move-back-to-prev-stop)
    (when (= orig-point (point)) (hgmacs-move-to-final-stop))))

(defun toggle-subword-normal-superword-mode ()
  "Toggle (subword -> superword -> normal -> subword -> ...) modes"
  (interactive)
  (cond (subword-mode (superword-mode +1))
        (superword-mode (superword-mode -1))
        (t (subword-mode +1))))

(defun degrees (angle)
  "Convert ANGLE to degrees (from radians)"
  (* (/ angle float-pi) 180.0))

(defun radians (angle)
  "Convert ANGLE to radians (from degrees)"
  (* (/ angle 180.0) float-pi))

(defun percent-error-small-angle (angle-deg)
  "Return small-angle error of ANGLE-DEG from sin and cos approx as percentage"
  (let* ((ang (radians angle-deg))
         (sin-ang (sin ang))
         (cos-ang (cos ang)))
    `(,(* 100 (abs (/ (- sin-ang ang) sin-ang))) ,(* 100 (abs (/ (- cos-ang 1) cos-ang))))))

;; (percent-error-small-angle 10) ; (0.5095 1.543)

(defun interp (xx yy x)
  "Return linear interpolation of X between two points with x-coord XX and y-coord YY."
  (let* ((xx0 (car xx))
         (xx1 (cadr xx))
         (yy0 (car yy))
         (yy1 (cadr yy))
         (dx-norm (/ (- x xx0) (- xx1 xx0)))
         (dy (- yy1 yy0)))
    (+ yy0 (* dx-norm dy))))

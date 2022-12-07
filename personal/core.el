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

(defun toggle-subword-normal-superword-mode ()
  "Toggle (subword -> superword -> normal -> subword -> ...) modes"
  (interactive)
  (cond (subword-mode (superword-mode +1))
        (superword-mode (superword-mode -1))
        (t (subword-mode +1))))

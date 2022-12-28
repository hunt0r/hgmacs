;; Set command as meta and option as super
(prelude-swap-meta-and-super)

;; Remove GUI scrollbar
(scroll-bar-mode -1)

;; Adapt some Prelude settings to my liking
(setq hgmacs-column-size 120)
(volatile-highlights-mode -1)
(ad-deactivate 'exchange-point-and-mark)
(setq whitespace-line-column hgmacs-column-size)
(when (eq system-type 'darwin) (setq ns-function-modifier 'none))
(setq scroll-conservatively 0)
(setq scroll-preserve-screen-position nil)
(setq scroll-error-top-bottom t)
(setq next-screen-context-lines 1)
(defalias 'sp-strict 'smartparens-strict-mode)
(setq sp-escape-quotes-after-insert nil)
;; Setting this to t is too aggressive... is there a lighter version?
;; I don't like that if I kill the remainder of a line, it wraps the next line up to previous
(setq sp-hybrid-kill-excessive-whitespace nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(define-key prelude-mode-map (kbd "C-a") 'hgmacs-move-beginning-of-line)
;; Because I like end-of-line cleanup of whitespace, but don't want that when I just switch away,
;; disable super-save-mode (which saves on switch-away)
(super-save-mode -1)
;; Much elisp code has tabs, this doesn't bother me
(defun remove-tabs-from-whitespace-active-style ()
  (make-local-variable 'whitespace-active-style)
  (setq whitespace-active-style (delq 'tabs whitespace-active-style)))
(add-hook 'emacs-lisp-mode-hook 'remove-tabs-from-whitespace-active-style)
(setq company-minimum-prefix-length 3)

;; Auth saved my ssh password in raw text form, I didn't like that.
;; Would be glad to find some more secure option...
(setq auth-source-save-behavior nil)

(defun set-hgmacs-fill-column-size ()
  "Note that fill-column is special in that it makes itself local on set."
  (setq fill-column hgmacs-column-size))
(add-hook 'c++-mode-hook 'set-hgmacs-fill-column-size)
(add-hook 'python-mode-hook 'set-hgmacs-fill-column-size)

(setq delete-by-moving-to-trash t)
(setq dired-listing-switches "-alh")
(setq list-directory-verbose-switches "-lh")
(setq sentence-end-double-space nil)
(setq shift-select-mode nil)

;; Don't want line numbers (user can get them from mode line, see also screenshare-mode)
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode -1)
  (global-nlinum-mode -1))

(defun prelude-update ()
  "Disable this."
  (interactive)
  (message "This command has been disabled. To update prelude, use github and update HGMacs fork."))

;; Unbind rare commands on prime keys
(global-unset-key (kbd "C-\\")) ;; used to change input method (multi-lingual)
(global-unset-key (kbd "C-z"))  ;; suspend-frame
(global-unset-key (kbd "M-m"))  ;; C-a toggles btwn line-start / text-start
(global-unset-key (kbd "M-\\")) ;; cycle-spacing twice does this
;; I prefer term-like C-h, and use C-S-h to start help
(global-set-key (kbd "C-S-h") 'help-command)
(global-set-key (kbd "C-h") 'backward-kill-word)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Create screenshare-mode
(prelude-require-package 'smooth-scroll)
(require 'smooth-scroll)
(setq smooth-scroll/hscroll-step-size 1)
(setq smooth-scroll/vscroll-step-size 1)
(easy-mmode-define-minor-mode screenshare-local-mode
  "Toggle (local) screenshare minor mode on or off. It is useful when sharing my emacs screen."
  :lighter " ScrnShr"
  (if screenshare-local-mode (hl-line-mode +1) (hl-line-mode -1))
  (if screenshare-local-mode (smooth-scroll-mode +1) (smooth-scroll-mode -1)))

(define-globalized-minor-mode screenshare-mode
  screenshare-local-mode
  ;; I think I should be able to exclude minibuffer mode using :predicate, but I don't know how.
  ;; The following below, or something like it, *should* work...
  ;; :predicate (not minibuffer-mode)
  (lambda () (unless (eq major-mode 'minibuffer-mode) (screenshare-local-mode t))))

(global-set-key (kbd "M-m") 'toggle-subword-normal-superword-mode)
(add-hook 'c-mode-hook 'subword-mode)
(add-hook 'c++-mode-hook 'subword-mode)
(add-hook 'python-mode-hook 'subword-mode)

(add-to-list 'auto-mode-alist '("\\.inl" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . text-mode))
;; Open ASL files in python-mode for syntax highlighting
(add-to-list 'auto-mode-alist '("\\.asl" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD" . python-mode))
(add-to-list 'auto-mode-alist '("\\.sadl" . python-mode))

;; Enable clipboard
(setq x-select-enable-clipboard t)

;; Use C-<tab> / C-S-<tab> for quick switching between windows, C-x o for accurate switch
(global-set-key [remap other-window] nil)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") (lambda () (interactive nil) (other-window -1)))
(setq aw-dispatch-always t)
;; Make this also work in magit
(defun hgmacs-fix-C-tab-in-magit ()
  (when (lookup-key magit-mode-map (kbd "C-<tab>"))
    (let ((binding-to-stomp (lookup-key magit-mode-map (kbd "C-M-<tab>")))
          (binding-to-transfer (lookup-key magit-mode-map (kbd "C-<tab>"))))
      (unless (or (not binding-to-stomp)
                  (equal binding-to-stomp binding-to-transfer))
        (message "Magit mode: losing C-M-<tab>'s binding to %s" binding-to-stomp))
      (define-key magit-mode-map (kbd "<C-M-tab>") binding-to-transfer)
      (define-key magit-mode-map (kbd "<C-tab>") nil))))
(add-hook 'magit-mode-hook 'hgmacs-fix-C-tab-in-magit)


(prelude-require-package 'with-editor)
(require 'with-editor)

(setq org-default-notes-file "~/scratch.org")

(global-set-key (kbd "C-x 4 3") 'hgmacs-split-window-three-horizontal)

;; Org mode keybindings (from Org mode 1.3)
(global-set-key "\C-cc" 'org-capture)

(prelude-require-package 'vterm)
(require 'vterm)
(setq vterm-buffer-name "*term*")
;; crux seems to assume one terminal, stomping on vterm's multi-terminal possibility.
;; I could potentially contribute an upgrade?
(setq crux-shell-func 'vterm)
(setq crux-term-func 'vterm)
(setq crux-term-buffer-name vterm-buffer-name)
(setq vterm-max-scrollback (floor 1e5))
(setq vterm-min-window-width hgmacs-column-size)
(defun unbind-c-a-in-vterm ()
  "Fix C-a behavior in vterm, Adapted from Prelude's suggestion in troubleshooting.md"
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-a") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))

(defun vterm-kill ()
  "Kill text in regular Emacs way and also C-k to vterm."
  ;; The read-only vterm buffer does not respond well to the kill command, so this copies instead.
  (interactive)
  (save-excursion
    (push-mark)
    (move-end-of-line ())
    (copy-region-as-kill (mark) (point)))
  (vterm-send (kbd "C-k")))

(defun hgmacs-fix-vterm-bindings ()
  "Set vterm key bindings as desired."
  (local-unset-key (kbd "C-l")) ; Since C-l does not preserve scrollback, disable it
  (local-unset-key (kbd "C-S-h")) ; No need to send this to term, it's my help key
  ;; Need this to kill the nano opened by vterm when using git
  (local-set-key (kbd "C-c C-x RET") (lambda () (vterm-send (kbd "C-x"))))
  (local-set-key (kbd "C-k") 'vterm-kill)
  (unbind-c-a-in-vterm))
(add-hook 'vterm-mode-hook 'hgmacs-fix-vterm-bindings)

(global-set-key (kbd "C-c C-s") 'hgmacs-open-scratch-file)

(setq bibtex-autokey-year-length 4)
(setq bibtex-autokey-year-title-separator "")
(setq bibtex-entry-format '(opts-or-alts
                            required-fields
                            numerical-fields
                            whitespace
                            realign
                            last-comma
                            delimiters
                            unify-case
                            braces
                            strings
                            sort-fields))
(setq bibtex-maintain-sorted-entries 'crossref)

;; Overwrite text-scale-adjust commands with global versions
(prelude-require-package 'default-text-scale)
(default-text-scale-mode +1)
(define-key default-text-scale-mode-map (kbd "C-M-=") nil)
(define-key default-text-scale-mode-map (kbd "C-M--") nil)
(define-key default-text-scale-mode-map (kbd "C-M-0") nil)
;; I don't know why these were in here, removing them just in case
(define-key default-text-scale-mode-map (kbd "ESC") nil)
(setq default-text-scale-mode-map (delete '(27) default-text-scale-mode-map))
;; Add the commands that I want
(define-key default-text-scale-mode-map (kbd "C-x C-+") 'default-text-scale-increase)
(define-key default-text-scale-mode-map (kbd "C-x C-=") 'default-text-scale-increase)
(define-key default-text-scale-mode-map (kbd "C-x C--") 'default-text-scale-decrease)
(define-key default-text-scale-mode-map (kbd "C-x C-_") 'default-text-scale-decrease)
(define-key default-text-scale-mode-map (kbd "C-x C-0") 'default-text-scale-reset)
;; TODO get these to work with repeated press mode
;; (Does default-text-scale already have this? If not, I can adapt from text-scale-adjust OR hydra.)

(add-to-list 'avy-keys ?\; t)
(setq avy-keys (delq ?h avy-keys))
(setq avy-keys (delq ?g avy-keys))

;;; Want C-s and C-r to work like isearch, except using ivy always
(prelude-require-package 'swiper)
(require 'swiper)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
;; swiper ideas:
;; Repeated C-s should never match backward. (Sometimes starts from last search end, not point.)
;; Same for repeated C-r.
(global-set-key (kbd "M-s s") 'isearch-forward) ; (Often overshadowed by smartparens)
(global-set-key (kbd "M-s r") 'isearch-backward) ; (Often overshadowed by smartparens)

;; TODO bug in highlighting newlines:
;; when I add several newlines to bottom of file, they are (correctly) highlighted
;; but when I add content to the final one, the others are no longer 'trailing'.
;; But their highlighting does not disappear.

;; TODO check out Brendan Miller's https://github.com/catphive/emacs

;; Idea: Get C-tab (and C-S-tab) briefly (0.1 sec) highlight the current line?
;; Idea: Some sort of mechanism for reminding me about new emacs/prelude features

;; Configure dbt mode and sql-indent?
;; (straight-use-package '(dbt-mode
;;                         :type git
;;                         :host github
;;                         :repo "CyberShadow/dbt-mode"
;;                         ;; Customize `sql-product' to set the flavor of the SQL syntax.
;;                         :custom (sql-product 'ansi)))
;; (add-to-list 'auto-mode-alist '("\\.sql\\'" . dbt-mode))

;; (straight-use-package 'sql-indent)
;; (require 'sql-indent)

;; ;; For more info, go to file sql-indent.org
;; ;; To build these rules, use M-x sqlind-show-syntax-of-line
;; (defvar hgm-sql-indent-offset-alists
;;   `((select-clause 0) ;; Left-align SELECT
;;     (insert-clause 0) ;; I don't use this. Should left-align INSERT
;;     (delete-clause 0) ;; I don't use this. Should left-align DELETE
;;     (update-clause 0) ;; I don't use this. Should left-align UPDATE
;;     (with-clause-cte 0) ;; Don't indent CTE names wrt WITH
;;     (comment-continuation 0)
;;     ,@sqlind-default-indentation-offsets-alist))

;; (defun hmacs-sqlind-changes ()
;;   "Set sqlind as desired."
;;   (setq sqlind-basic-offset 4)
;;   (setq sqlind-indentation-offsets-alist
;;         hgm-sql-indent-offset-alists))
;; (add-hook 'sqlind-minor-mode-hook 'xyz)

;; Useful code snippets in determining what I like above.
;; I have not yet figured out how to make "temporary" changes to the settings
;; (setq hgm-sql-indent-offset-alists sqlind-indentation-offsets-alist)
;; (setq hgm-sql-indent-offset-alists
;;       `((select-clause 0) ;; Left-align SELECT
;;         (insert-clause 0) ;; I don't use this. Should left-align INSERT
;;         (delete-clause 0) ;; I don't use this. Should left-align DELETE
;;         (update-clause 0) ;; I don't use this. Should left-align UPDATE
;;         ,@hgm-sql-indent-offset-alists))
;; (setq hgm-sql-indent-offset-alists
;;       `((comment-continuation 0)
;;         ,@hgm-sql-indent-offset-alists))
;; (setq hgm-sql-indent-offset-alists (delete '(with-clause 0) hgm-sql-indent-offset-alists))

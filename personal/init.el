;; Set command as meta and option as super
(prelude-swap-meta-and-super)

;; Remove GUI scrollbar
(scroll-bar-mode -1)

;; Adapt some Prelude settings to my liking
(setq hgmacs-column-size 120)
;; (volatile-highlights-mode -1) ;; TODO should I leave this disabled?
(global-hl-line-mode -1) ; Messes with face coloring on the current line, not useful enough
(projectile-mode -1) ;; I'm not ready for this yet... but I'd like to learn it.
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
;; Much elisp code has tabs, this doesn't bother me, so don't highlight them
(defun remove-tabs-from-whitespace-active-style ()
  "Used to prevent tab-highlighting in some modes (e.g. elisp)"
  (make-local-variable 'whitespace-active-style)
  (setq whitespace-active-style (delq 'tabs whitespace-active-style)))
(add-hook 'emacs-lisp-mode-hook 'remove-tabs-from-whitespace-active-style)
(setq company-minimum-prefix-length 3)
(key-chord-unset-global "xx")
(delete "Press <xx> quickly to execute extended command." key-chord-tips)

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

;; Don't want line numbers by default
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
(setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\; ?g ?h))
(custom-set-faces '(aw-leading-char-face ((default (:height 3.0 :inherit ace-jump-face-foreground))
                                          (((class color)) (:foreground "white" :background "red"))
                                          (((background dark)) (:foreground "gray100"))
                                          (((background light)) (:foreground "gray0"))
                                          (t (:foreground "gray100" :underline nil)))
                                         nil
                                         "Undo zenburn customization"))
;; Idea: Modify ace-window s.t. it can display multiple chars (as the comment says)
;; Also consider mod to ace-window that if point is in same pos as letter, do something smart like hide point
(global-set-key (kbd "C-x o") 'ace-window) ; Remember C-u = swap, C-u C-u = delete
(global-set-key (kbd "C-x 4 0") (lambda () (interactive) (ace-window 16))) ; (may remove in favor of C-u C-u C-x o)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-x 4 C-x") 'aw-show-dispatch-help)
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
(defun vterm-local (&optional arg)
  "Ensure vterm launches in local home dir"
  (interactive "P")
  (let ((default-directory "~/"))
    (vterm arg)))
(setq vterm-buffer-name "*term*")
;; crux seems to assume one terminal, stomping on vterm's multi-terminal possibility.
;; I could potentially contribute an upgrade?
(setq crux-shell-func 'vterm-local)
(setq crux-term-func 'vterm-local)
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

;; Setup preferred zooming (text-scale) options:
;;   Default zoom is be frame-wide
;;   Some option for single-window instead
;;   Key combos are uncommon (So don't conflict with neg-arg)
;;   Use a hydra to get repeat-key UX
(prelude-require-packages '(hydra default-text-scale))
(default-text-scale-mode +1)
(defhydra hydra-zoom () "Repeat +/-/0 to zoom in/out/reset"
  ("C-+" default-text-scale-increase)
  ("C--" default-text-scale-decrease)
  ("C-0" default-text-scale-reset)
  ("C-=" default-text-scale-increase)
  ("C-_" default-text-scale-decrease))
(define-key default-text-scale-mode-map (kbd "C-x C-+") 'hydra-zoom/body)
(define-key default-text-scale-mode-map (kbd "C-x C--") 'hydra-zoom/body)
(define-key default-text-scale-mode-map (kbd "C-x C-0") 'hydra-zoom/body)
(define-key default-text-scale-mode-map (kbd "C-x C-=") 'hydra-zoom/body)
(define-key default-text-scale-mode-map (kbd "C-x C-_") 'hydra-zoom/body)
;; Because I want C-- as neg-arg, disable both single-window default commands
(global-set-key (kbd "C--") 'negative-argument)
(global-unset-key (kbd "C-+"))
;; TODO single-window zooming

(add-to-list 'avy-keys ?\; t)
(setq avy-keys (delq ?h avy-keys))
(setq avy-keys (delq ?g avy-keys))
;; Idea: does avy have functionality to accept multiple chars, then jump after a small pause?
;; This is like somewhat-fuzzy.
;; Example: say I want to jump to "class". I could jump with "c" and get a bunch. But "cla" should be a rarer match.

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

(set-face-background 'mode-line "gray8")

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(prelude-require-packages '(lsp-mode lsp-ui google-c-style latex-math-preview math-preview ein))

(setq-default ein:output-area-inlined-images t) ;; if you want plots printed inline with notebook
;;(setq ein:output-area-inlined-images nil) ;; if you want them exported to app (configured in mailcap-user-mime-data)
;;(setq math-preview--debug-json t) ;; to debug math-preview output

(defun hgmacs-ein-bindings ()
  "Modify bindings as I like in ein notebooks"
  (define-key ein:notebook-mode-map (kbd "C-c C-x C-e") 'ein:worksheet-execute-all-cells)
  (define-key ein:notebook-mode-map (kbd "C-c C-x C-a") 'ein:worksheet-execute-all-cells-above)
  (define-key ein:notebook-mode-map (kbd "C-c C-x C-b") 'ein:worksheet-execute-all-cells-below))

(add-hook 'ein:notebook-mode-hook 'hgmacs-ein-bindings)
;; TODO figure out how to default to this in the prompt
(setq hgmacs-ein-default-link "http://explorer:8888")

(add-to-list 'grep-files-aliases'("cpp" . "*.cc *.hh *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++ *.inl"))
(add-to-list 'grep-files-aliases'("bzl" . "BUILD *.bzl"))
(add-to-list 'grep-files-aliases'("py" . "*.py *.ipynb"))
;; Wonder if I will ever want combinations of these... cpp+bzl, py+bzl, etc.

;; When running lgrep or rgrep, I don't want ivy to directory-match at 2nd stage (for pattern).
;; This is a simple disabler of that.
(defun dont-ivy-on-grep-read-files (grf-original &rest args)
  "Don't let grep-read-files use ivy-completing-read"
  (let ((completing-read-function
         (if (equal completing-read-function #'ivy-completing-read) ivy--old-crf completing-read-function)))
    (apply grf-original args)))
(advice-add 'grep-read-files :around #'dont-ivy-on-grep-read-files)
;; Idea: Could I have ivy list out the available aliases from grep-file-aliases?
;; Did some digging: grep calls completing-read with #'read-file-name-internal as COLLECTION.
;; So I would need to either modify grep (which I don't love), or hijack read-file-name-internal and make it DWIW.
;; I don't love mucking about in something so deep (only want it to use grep-files-aliases in this one case) so
;; I am procrastinating as I think about a better solution.
;; My BATNA is good: Just don't have it say anything

;; I'm a little frustrated with ivy. Things I miss from helm:
;; C-f to drop into non-helm mode
;; Matching "always" dwim. (attempting to find-file and tab-complete /ssh:explorer sometimes does not match, usually matches many)
;; The single-tab (complete) and double-tab (complete-and-go) doesn't work well with tramp lag.

;; Recommended by https://github.com/bbatsov/projectile/issues/1232
(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))

;; Idea: Get C-tab (and C-S-tab) briefly (0.1 sec) highlight the current line?
;; Idea: Some sort of mechanism for reminding me about new emacs/prelude features
;; Idea: Work on tab completion inside compile for bazel build commands

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

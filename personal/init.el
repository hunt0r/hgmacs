;; Set command as meta and option as super
(when (eq system-type 'darwin)
  (prelude-swap-meta-and-super))

;; Remove GUI scrollbar
(scroll-bar-mode -1)

;; Adapt some Prelude settings to my liking
(setq hgmacs-column-size 120)
;; (volatile-highlights-mode -1) ;; TODO should I leave this disabled?
(global-hl-line-mode -1) ; Messes with face coloring on the current line, not useful enough
(projectile-mode +1) ; (Was already on, I think... as I learn this, can drop it)
;; Consider this to speed up projectile-remote usage:
;; (defadvice projectile-on (around exlude-tramp activate)
;;   should disable projectile when visiting a remote file
;;   (unless  (--any? (and it (file-remote-p it))
;;                    (list
;;                     (buffer-file-name)
;;                     list-buffers-directory
;;                     default-directory
;;                     dired-directory))
;;     ad-do-it))
;; Since super does not (yet) transmit over my X, I want to duplicate all those bindings as "alt"
;; So I copy-pasted from prelude setup. If I keep this, may find a more elegant way.
(when prelude-super-keybindings
  (define-key prelude-mode-map (kbd "A-p") 'projectile-command-map))
(when prelude-super-keybindings
  ;; crux
  (define-key prelude-mode-map (kbd "A-r") 'crux-recentf-find-file)
  (define-key prelude-mode-map (kbd "A-j") 'crux-top-join-line)
  (define-key prelude-mode-map (kbd "A-k") 'crux-kill-whole-line)
  (define-key prelude-mode-map (kbd "A-o") 'crux-smart-open-line-above)
  ;; magit
  (define-key prelude-mode-map (kbd "A-m m") 'magit-status)
  (define-key prelude-mode-map (kbd "A-m j") 'magit-dispatch)
  (define-key prelude-mode-map (kbd "A-m k") 'magit-file-dispatch)
  (define-key prelude-mode-map (kbd "A-m l") 'magit-log-buffer-file)
  (define-key prelude-mode-map (kbd "A-m b") 'magit-blame)
  ;; misc
  (define-key prelude-mode-map (kbd "A-/") 'hippie-expand))

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
(setq org-special-ctrl-a/e t)
;; Because I like end-of-line cleanup of whitespace, but don't want that when I just switch away,
;; disable super-save-mode (which saves on switch-away)
(super-save-mode -1)
;; Much elisp code has tabs, this doesn't bother me, so don't highlight them
(defun hgmacs-remove-tabs-from-whitespace-active-style ()
  "Used to prevent tab-highlighting in some modes (e.g. elisp)"
  (make-local-variable 'whitespace-active-style)
  (delete 'tabs whitespace-active-style))
(add-hook 'emacs-lisp-mode-hook 'hgmacs-remove-tabs-from-whitespace-active-style)

(defun hgmacs-disable-whitespace-in-buffer ()
  "Turn off whitespace highlighting in some effective way.

   I don't understand what all this does or why, sometime perhaps refine it."
  ;; Not sure if this is necessary, but do it anyway
  (set (make-local-variable 'prelude-whitespace) nil)
  (set (make-local-variable 'whitespace-style) nil)
  ;; Also, using this command makes this buffer-local automatically
  (whitespace-mode -1))
(defun disable-whitespace-in-scratch-buffer ()
  "Disable whitespace in scratch buffer."
  (when (equal (buffer-name) "*scratch*")
    (hgmacs-disable-whitespace-in-buffer)))
;; This hook runs too late. What earlier thing is causing the fail?
(add-hook 'lisp-interaction-mode-hook 'disable-whitespace-in-scratch-buffer)



(defun hgmacs-fix-whitespace-highlight-issue-in-describe-variable ()
  "When I describe-variable, the value is sometimes whitespace highlighted in a bad way."
  (defun disable-prelude-whitespace-in-temp-buffer ()
    "Disable whitespace in a buffer named ' *temp*'.

   This buffer seems to be a temporary component while buiding the *Help* of describe-variable.
   (And so it has the local variable value of prelude-whitespace which is relevant.)"
    (when (equal (buffer-name) " *temp*")
      (setq-local prelude-whitespace nil)))
  (setq hook-depth-to-run-this-before-prelude-prog-mode -1)
  (add-hook 'prog-mode-hook 'disable-prelude-whitespace-in-temp-buffer hook-depth-to-run-this-before-prelude-prog-mode))

(hgmacs-fix-whitespace-highlight-issue-in-describe-variable)

(setq company-minimum-prefix-length 3)
(key-chord-unset-global "xx")
(delete "Press <xx> quickly to execute extended command." key-chord-tips)
(if global-auto-revert-mode (setq magit-auto-revert-mode nil))

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

(defun hgmacs-require-package (package)
  "Weird that prelude-require-package does not actually require. So this does."
  (prelude-require-package package)
  (require package))

(defun hgmacs-require-packages (packages)
  "Loop over packages."
  (mapc #'hgmacs-require-package packages))

;; Create screenshare-mode
(hgmacs-require-package 'smooth-scroll)
(setq smooth-scroll/hscroll-step-size 1)
(setq smooth-scroll/vscroll-step-size 1)
(easy-mmode-define-minor-mode screenshare-local-mode
                              "Toggle (local) screenshare minor mode on or off. It is useful when sharing my emacs screen."
                              :lighter " ScrnShr"
                              (if screenshare-local-mode (hl-line-mode +1) (hl-line-mode -1))
                              (if screenshare-local-mode (nlinum-mode +1) (nlinum-mode -1))
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
(add-to-list 'auto-mode-alist '("\\.inc" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . text-mode))
;; Open ASL files in python-mode for syntax highlighting
(add-to-list 'auto-mode-alist '("\\.asl" . python-mode))
(add-to-list 'auto-mode-alist '("\\.sadl" . python-mode))

;; Enable clipboard
(setq x-select-enable-clipboard t)

;; I use both a quick-switch (maybe forward and backward) and an accurate-switch
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
;; Idea: Rely on avy for onscreen jumping, but make "back to where I was in last window" something repeatable.
;;     Each window might have top line (or last line) as the ace-window char for this window
(setq prelude-flyspell nil) ; This uses some keys I want to try for window-switching
(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "C-:") (lambda () (interactive nil) (other-window -1)))
(global-set-key (kbd "C-x 4 C-x") 'aw-show-dispatch-help)
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
;; (add-hook 'magit-mode-hook 'hgmacs-fix-C-tab-in-magit)


(hgmacs-require-package 'with-editor)

(setq org-default-notes-file "~/scratch.org")

(global-set-key (kbd "C-x 4 2") 'hgmacs-split-window-two-horizontal)
(global-set-key (kbd "C-x 4 3") 'hgmacs-split-window-three-horizontal)

;; Org mode keybindings (from Org mode 1.3)
(global-set-key "\C-cc" 'org-capture)

(hgmacs-require-package 'vterm)
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
;;   Use prefix arg for single-window instead
;;   Use a hydra to get repeat-key UX
;;   Use uncommon key combos (so don't conflict with neg-arg)
(hgmacs-require-packages '(hydra default-text-scale))
(default-text-scale-mode +1)
(defhydra hydra-zoom () "Repeat C-(+/-/0) to zoom in/out/reset"
  ("C-+" default-text-scale-increase)
  ("C--" default-text-scale-decrease)
  ("C-0" default-text-scale-reset)
  ("C-=" default-text-scale-increase)
  ("C-_" default-text-scale-decrease))
(defun text-scale-reset ()
  "Reset text scale (zooming)"
  (interactive)
  (text-scale-set 0))
;; Idea: somehow make these two noarg DRYer
;; Idea for improvement: use interactive P to distinguish arg type
(defun text-scale-increase-noarg (arg)
  "Prevent ARG from getting to text-scale-increase"
  (interactive "p")
  (text-scale-increase 1))
;; Idea for improvement: use interactive P to distinguish arg type
(defun text-scale-decrease-noarg (arg)
  "Prevent ARG from getting to text-scale-decrease"
  (interactive "p")
  (text-scale-decrease 1))
(defhydra hydra-zoom-local () "Repeat C-(+/-/0) to zoom in/out/reset this window"
  ("C-+" text-scale-increase-noarg)
  ("C--" text-scale-decrease-noarg)
  ("C-0" text-scale-reset)
  ("C-=" text-scale-increase-noarg)
  ("C-_" text-scale-decrease-noarg))
;; Idea: somehow make these DRYer
;; Idea for improvement: use interactive P to distinguish arg type
(defun zoom-in (arg)
  "By default, zoom in globally. With ARG, locally instead."
  (interactive "p")
  (if (equal arg 1)
      (hydra-zoom/default-text-scale-increase)
    (hydra-zoom-local/text-scale-increase-noarg)))
;; Idea for improvement: use interactive P to distinguish arg type
(defun zoom-out (arg)
  "By default, zoom out globally. With ARG, locally instead."
  (interactive "p")
  (if (equal arg 1)
      (hydra-zoom/default-text-scale-decrease)
    (hydra-zoom-local/text-scale-decrease-noarg)))
;; Idea for improvement: use interactive P to distinguish arg type
(defun zoom-reset (arg)
  "By default, reset zoom globally. With ARG, locally instead."
  (interactive "p")
  (if (equal arg 1)
      (hydra-zoom/default-text-scale-reset)
    (hydra-zoom-local/text-scale-reset)))
(define-key default-text-scale-mode-map (kbd "C-x C-+") 'zoom-in)
(define-key default-text-scale-mode-map (kbd "C-x C--") 'zoom-out)
(define-key default-text-scale-mode-map (kbd "C-x C-0") 'zoom-reset)
(define-key default-text-scale-mode-map (kbd "C-x C-=") 'zoom-in)
(define-key default-text-scale-mode-map (kbd "C-x C-_") 'zoom-out)
;; Because I want C-- as neg-arg, disable both single-window default commands
(global-set-key (kbd "C--") 'negative-argument)
(global-unset-key (kbd "C-+"))
;; Also disable the s- commands that prelude added
(global-unset-key (kbd "s-+"))
(global-unset-key (kbd "s--"))
(global-unset-key (kbd "s-0"))
(global-unset-key (kbd "s-="))

(add-to-list 'avy-keys ?\; t)
(setq avy-keys (delq ?h avy-keys))
(setq avy-keys (delq ?g avy-keys))
(add-to-list 'avy-keys ?q t)
(add-to-list 'avy-keys ?w t)
(add-to-list 'avy-keys ?e t)
(add-to-list 'avy-keys ?r t)
(add-to-list 'avy-keys ?u t)
(add-to-list 'avy-keys ?i t)
(add-to-list 'avy-keys ?o t)
(add-to-list 'avy-keys ?p t)
(add-to-list 'avy-keys ?z t)
(add-to-list 'avy-keys ?x t)
(add-to-list 'avy-keys ?c t)
(add-to-list 'avy-keys ?v t)
(add-to-list 'avy-keys ?n t)
;; Having both , and . was visually confusing. Keep only one.
;; (add-to-list 'avy-keys ?\, t)
(add-to-list 'avy-keys ?\. t)
;; TIL: key-chord binds the mirror too! That makes so much sense.
(key-chord-define-global "jk" 'avy-goto-char-timer)
(global-set-key (kbd "M-g j") 'avy-goto-word-1)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g k") 'avy-goto-char-timer)
(key-chord-define-global "j;" 'other-window)
(key-chord-define-global "J:" (lambda () (interactive nil) (other-window -1)))

;; Want C-s and C-r to work like isearch, except using ivy always
(hgmacs-require-package 'swiper)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
;; swiper ideas:
;; Repeated C-s should never match backward. (Sometimes starts from last search end, not point.)
;; Same for repeated C-r.
(global-set-key (kbd "M-s s") 'isearch-forward) ; (Often overshadowed by smartparens)
(global-set-key (kbd "M-s r") 'isearch-backward) ; (Often overshadowed by smartparens)

(set-face-background 'mode-line "gray8")

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(hgmacs-require-packages '(lsp-mode lsp-ui google-c-style latex-math-preview math-preview ein))
(hgmacs-require-package 'eglot)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
;; First, generate compile_commands.json via:
;;   python -m tools.compdb.compdb --watch --remote-index-mode=grpc
;; Next, add "build --config=compdb" to av/user.bazelrc (can also add "build --config=remote")
;; Finally, this magic:
(add-to-list 'eglot-server-programs
              '((c++-mode c-mode)
                "/home/hmcclelland/av/tools/vscode/scripts/clangd.sh"
                "--remote-index-address=clangd-index-server-grpc-prod.bluel3.tools:443"
                "--project-root=/home/hmcclelland/av"))
;; I need this magic command when kaso headers are not auto-detected:
;; bazel build --compile_one_dependency --output_groups=compdb_files path/to/using_proto_kaso.cc
;; (might need to restart clangd?)
;; Can also do bazel build --output_groups=compdb_files //my/cc/bazel:target (or even better //my/cc/bazel:*)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

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

(add-to-list 'grep-files-aliases '("cpp" . "*.cc *.hh *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++ *.inl *.inc"))
(add-to-list 'grep-files-aliases '("bzl" . "BUILD *.bzl *.bazel"))
(add-to-list 'grep-files-aliases '("py" . "*.py *.ipynb"))
;; Wonder if I will ever want combinations of these... cpp+bzl, py+bzl, etc.

;; Rarely (never?) want to search bazel cache
(add-to-list 'grep-find-ignored-directories ".cache")

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

;;; On ivy (and counsel? And swiper?)
;; TODO single-tab (complete) and double-tab (complete-and-go) doesn't work well with tramp lag. Can I use other bindings to achieve what I want?
;; C-m: same as RET: done.
;; M-o: open a menu to select from ; TODO HGM: Does not work?
;; C-j: same as TAB TAB: complete-dir or done.
;; C-M-j: done-no-completion
;; C-c C-r: ivy-resume (useful if accidentally exited early)
;; C-: ivy-avy to select ; TODO HGM: Does not work?
;; C-M-m: non-exiting C-m ; TODO HGM: Does not work?
;; M-i: complete, but do not select
;; M-j: insert subword at point into ivy-search
;; S-SPC: restrict-to-these then restart matching
;; C-r: like shell C-r to search command history
;; M-w: copy all matches to kill-ring (Use M-i M-w to get just one)
;; C-o: ivy-hydra/body ; TODO HGM: Does not work
;; C-c C-o: ivy-occur saves current completion candidates to buffer for further operation
;; M-r: toggle 'regex-mode' (useful for literal ., for instance)
;; /ssh: C-j: complete for TRAMP hosts
;;
;; TODO An ivy command to "drop into non-ivy mode"?
;; TODO Why does tab-complete not always work? e.g. /ssh:explorer sometimes does not match


;;; Attempting to speed up tramp
;; This limits vc to using git only
(setq vc-handled-backends '(Git))
;; From https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html, this one disables vc on tramp directories
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
;; The following assumes different emacs sessions are not modifying the same file.
(setq remote-file-name-inhibit-locks t)


;; Recommended by https://github.com/bbatsov/projectile/issues/1232
(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))

(global-set-key (kbd "C-z") 'hl-line-mode)
(set-face-background 'hl-line "gray8")

(defun hgmacs-text-mode-whitespace-style-changes ()
  "Do some changes that I want"
  (make-local-variable 'whitespace-style)
  (delete 'lines-tail whitespace-style)
  (add-to-list 'whitespace-style 'lines-char))
(add-hook 'text-mode-hook 'hgmacs-text-mode-whitespace-style-changes)

;; Disabling until I learn how to configure this to not be slow.
;; (add-hook 'c-mode-hook (lambda () (company-mode -1)))
;; (add-hook 'c++-mode-hook (lambda () (company-mode -1)))

(add-hook 'python-mode-hook (lambda () (define-key python-mode-map (kbd "C-c C-g") 'bazel-find-build-file)))

;;; C mode (CC mode) customizations
(defun bind-ff ()
  (define-key c-mode-base-map (kbd "C-c C-f") 'ff-get-other-file))
(add-hook 'c-initialization-hook 'bind-ff)
;; TODO: Create a "find other other file" which skips the first match and seeks the 2nd one. Perhaps bind to C-c C-v?

(add-hook 'c-initialization-hook (lambda () (define-key c-mode-base-map (kbd "C-c C-g") 'bazel-find-build-file)))

(defun alist-get-equal (key alist &optional default remove)
  "A shortcut for alist-get using equal."
  (alist-get key alist default remove 'equal))

(defun cpp-ff-customization ()
  "Create some additional extensions for ff-find-other-file"
  (hgmacs-require-package 'find-file)
  (add-to-list 'cc-other-file-alist '("\\.inl\\'" (".hpp" ".h++" ".hxx" ".hh" ".h")) t)
  (add-to-list 'cc-other-file-alist '("\\.inc\\'" (".hpp" ".h++" ".hxx" ".hh" ".h")) t)
  ;; This appends ".inl" and ".inc" to ".hpp"s other files
  (setf (alist-get "\\.hpp\\'" cc-other-file-alist nil nil 'equal)
        (list (list (caar (alist-get "\\.hpp\\'" cc-other-file-alist nil nil 'equal)) ".inl" ".inc")))
  (setf (alist-get "\\.hh\\'" cc-other-file-alist nil nil 'equal)
        (list (list (caar (alist-get "\\.hh\\'" cc-other-file-alist nil nil 'equal)) ".inl" ".inc")))
  ;; I do not understand why this approach fails. (And it worked once?!?)
  ;; (setf (alist-get-equal "\\.hpp\\'" cc-other-file-alist)
  ;;       (list (list (caar (alist-get-equal "\\.hpp\\'" cc-other-file-alist)) ".inl" ".inc")))
  )
(add-hook 'c++-mode-hook 'cpp-ff-customization)

(defun apr-cpp-indentation-setup ()
  "Configure CC mode for APR"
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'access-label '/))
;; (add-hook 'c++-mode-hook 'apr-cpp-indentation-setup)

;; Smartparens uses sp-forward-slurp-sexp but that doesn't work as well in programming contexts.
;; (example: a forward-slurp in C++ slurps in the final semicolon)
(defun hgmacs-forward-slurp (&optional arg)
  "Select the best strictparens forward-slurp to use and use it"
  (interactive "*P")
  (if (equal major-mode 'c++-mode)
      (sp-slurp-hybrid-sexp)
    (sp-forward-slurp-sexp arg)))
(define-key smartparens-mode-map [remap sp-forward-slurp-sexp] 'hgmacs-forward-slurp)

;;; General functionality
(defun copy-buffer-file-name ()
  "Copy the current buffer's (full) file name to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

;;; bazel mode functionality
;; Prerequisite: git clone https://github.com/bazelbuild/emacs-bazel-mode.git ~/emacs-bazel-mode
;; Prerequisite: ln -s ~/emacs-bazel-mode/bazel.el ~/.emacs.d/personal/
(defun copy-bazel-identifier-at-point ()
  "Copy identifier at point (to the kill ring)."
  (interactive)
  (kill-new (xref-backend-identifier-at-point 'bazel-mode)))
(define-key bazel-mode-map (kbd "C-c M-q") 'copy-bazel-identifier-at-point)
;; Known issue with ivy in bazel-mode: https://github.com/bazelbuild/emacs-bazel-mode/blob/master/manual.org#known-issues
(dolist (function '(bazel-build bazel-run bazel-test bazel-coverage))
  (add-to-list 'ivy-completing-read-handlers-alist
               `(,function . completing-read-default)))
;; Don't need this anymore, I found the issue. Can delete sometime.
(defun disable-flycheck ()
  (flycheck-mode -1))
;; (add-hook 'bazel-mode-hook 'disable-flycheck)

;;; learning and teaching emacs fu
(defun learn ()
  "Give me a random tip to learn from"
  (interactive)
  (prelude-tip-of-the-day))

;; prelude-tips is stored by value, other stuff "pushes" to it. That's a good pattern for me.

(defun teach ()
  "Push a tip permanently to prelude-tips"
  (interactive)
  (message "TODO. In the meantime, do it manually at this func's def."))

(setq hgm-tips '(
                 "M-x teach to add a tip to the learn list."
                 "(makunbound 'varname) to erase/clear variable varname."
                 "(delete ELT SEQ) to drop an item from a list in-place."))
(dolist (tip hgm-tips prelude-tips) (add-to-list 'prelude-tips tip t))

(setq magit-branch-prefer-remote-upstream '("master" "develop" "main"))
;; HGM: WIP, what to do when not on internet?
;; (setq magit-repository-directories'(("/Users/hmcclelland/.emacs.d/" . 0)
;;                                     ("/ssh:explorer:~/av/" . 0)
;;                                     ("/ssh:explorer:~/adas-platform/" . 0)))

;; TODO: Learn to use bookmarks / registers instead.
(global-set-key (kbd "C-c C-l") (lambda () (interactive)(find-file "/Users/hmcclelland/.emacs.d/personal/init.el")))

;; TODO: Quick keybinding for "open this file's BUILD file/target" like C-f gets the other.
;; TODO: Prefix arg to get the "other" other file. (.inc)

;; For adding a new include:
(global-set-key (kbd "C-c M-w") 'copy-buffer-file-name)
;; Visit the file
;; copy-buffer-file-name (C-c M-w)
;; Create new #include statement
;; Yank copied buffer-file-name
;; Mark paragraph
;; Sort lines alphabetically

;; Need faster file recall. Projectile?

;; Want "go to this thing". LSP?

(hgmacs-require-package 'protobuf-mode)

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; So that emacs can use (local) ssh-agent, don't need to enter password on every git push/fetch
(hgmacs-require-package 'ssh-agency)

;; Idea: Get C-tab (and C-S-tab) briefly (0.1 sec) highlight the current line?
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

;;; General productivity tools
;;
;; (require 'prelude-ido) ;; Supercharges Emacs completion for C-x C-f and more
(require 'prelude-ivy) ;; A mighty modern alternative to ido
;; (require 'prelude-selectrum) ;; A powerful, yet simple, alternative to ivy
;; (require 'prelude-helm) ;; Interface for narrowing and search
;; (require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
(require 'prelude-key-chord) ;; Binds useful features to key combinations

;;; Org-mode (a legendary productivity tool that deserves its own category)
;;
;; Org-mode helps you keep TODO lists, notes and more.
(require 'prelude-org)

;;; Programming languages support
;;
;; Modules for a few very common programming languages
;; are enabled by default.
(require 'prelude-c)
;; (require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
(require 'prelude-css)
;; (require 'prelude-dart)
(require 'prelude-emacs-lisp)
;; (require 'prelude-erlang)
;; (require 'prelude-elixir)
;; (require 'prelude-fsharp)
;; (require 'prelude-go)
;; (require 'prelude-haskell)
(require 'prelude-js)
(require 'prelude-latex)
(require 'prelude-lisp) ;; Common setup for Lisp-like languages
;; (require 'prelude-literate-programming) ;; Setup for Literate Programming
(require 'prelude-lsp) ;; Base setup for the Language Server Protocol
;; (require 'prelude-lua)
;; (require 'prelude-ocaml)
(require 'prelude-perl)
(require 'prelude-python)
;; (require 'prelude-racket)
;; (require 'prelude-ruby)
;; (require 'prelude-rust)
;; (require 'prelude-scala)
;; (require 'prelude-scheme)
(require 'prelude-shell)
;; (require 'prelude-scss)
;; (require 'prelude-ts)
(require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)
(require 'prelude-yaml)

;;; Misc
(require 'prelude-erc) ;; A popular Emacs IRC client (useful if you're still into Freenode)

(provide 'prelude-modules)

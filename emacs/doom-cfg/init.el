;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       corfu
       vertico

       :ui
       deft
       doom
       hl-todo
       modeline
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       multiple-cursors
       parinfer
       rotate-text
       snippets
       word-wrap

       :emacs
       dired
       electric
       ibuffer
       undo
       vc

       :term

       :checkers
       syntax
       (spell +aspell +everywhere)

       :tools
       direnv
       (eval +overlay)
       lookup
       lsp
       magit
       terraform

       :os
       (:if IS-MAC macos)

       :lang
       (cc +lsp)
       (clojure +lsp)
       data
       dhall
       (elixir +lsp)
       emacs-lisp
       (go +lsp)
       (haskell +lsp)
       (json +lsp)
       (latex +lsp)
       ledger
       (lua +lsp)
       markdown
       nix
       (org +roam2)
       (python +lsp)
       rest
       (rust +lsp)
       (sh +lsp)
       (web +lsp)
       (yaml +lsp)

       :email
       notmuch

       :app
       everywhere

       :config
       ;; Disable literate mode until
       ;; https://github.com/doomemacs/doomemacs/issues/6902 is fixed. We just
       ;; manually tangle with "C-c C-v t" for now.
       ;;literate
       (default +bindings +smartparens))

(setq doom-leader-key ","
      doom-leader-alt-key "C-,"
      doom-localleader-key ", m"
      doom-localleader-alt-key "C-, m")

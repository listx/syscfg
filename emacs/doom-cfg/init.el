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
       editorconfig
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
       (javascript +lsp)
       (json +lsp)
       (latex +lsp)
       (lua +lsp)
       markdown
       nix
       (org +pretty +roam2)
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
       (default +bindings +smartparens))

(setq doom-leader-key ","
      doom-leader-alt-key "C-,"
      doom-localleader-key ", m"
      doom-localleader-alt-key "C-, m")

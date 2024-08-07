;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       company           ; the ultimate code completion backend
       vertico           ; the search engine of the future

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline         ; snazzy, Atom-inspired modeline, plus API
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       parinfer         ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +aspell +everywhere) ; tasing you for misspelling mispelling

       :tools
       direnv
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       lsp               ; M-x vscode
       magit             ; a git porcelain for Emacs
       terraform         ; infrastructure as code

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS

       :lang
       (cc +lsp)               ; C > C++ == 1
       (clojure +lsp)          ; java with a lisp
       data              ; config/data formats
       dhall
       (elixir +lsp)           ; erlang done right
       emacs-lisp        ; drown in parentheses
       (go +lsp)         ; the hipster dialect
       (haskell +lsp)  ; a language that's lazier than I am
       (json +lsp)            ; At least it ain't XML
       (latex +lsp)            ; writing papers in Emacs has never been so fun
       ledger            ; be audit you can be
       (lua +lsp)               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       nix               ; I hereby declare "nix geht mehr!"
       (org +roam2)              ; organize your plain life in plain text
       (python +lsp)            ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (sh +lsp)               ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp)              ; the tubes
       (yaml +lsp)             ; JSON, but readable

       :email
       notmuch

       :app
       everywhere        ; *leave* Emacs!? You must be joking

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

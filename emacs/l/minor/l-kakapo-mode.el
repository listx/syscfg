(use-package kakapo-mode)

(defun l/add-hook (hook tmode twidth &optional func)
  (add-hook hook `(lambda ()
    (when (not l/kakapo-settings-applied)
      (kakapo-mode)
      (electric-indent-mode -1)
      (setq indent-tabs-mode ,tmode)
      (setq tab-width ,twidth)
      (setq evil-shift-width ,twidth)
      (setq l/kakapo-settings-applied t)
      (eval ,func)
      (message (concat
        (if (boundp 'l/kakapo-project-id)
          (concat "kakapo project `" l/kakapo-project-id "'")
          "kakapo defaults") ": "
        "indent-tabs-mode=%s "
        "tab-width=%s "
        "evil-shift-width=%s ")
        indent-tabs-mode
        tab-width
        evil-shift-width)))))

; Check if a buffer's name matches the a project's path (regex). If it does,
; then also set the project-id.
(defun l/kakapo-set-project-id (project-id project-regex buffer-name)
  (when (string-match project-regex buffer-name)
    (setq l/kakapo-project-id project-id)))

; Adapted from KimStorm's solution from http://www.emacswiki.org/ProjectSettings.
(defun l/kakapo-indents ()
  (let
    (
      (b (buffer-file-name))
      (h (getenv "HOME")))
    (defun h (hook tmode twidth &optional func)
      (l/add-hook hook tmode twidth func))
    (setq l/kakapo-settings-applied nil)
    ; Default settings for unrecognized languages.
    (kakapo-mode)
    (setq indent-tabs-mode t)
    (setq tab-width 4)
    (setq evil-shift-width 4)
    ; Language-specific settings.
    (when b
      ; C
      (h 'c-mode-hook t 8
        '(progn
          (setq default-tab-width 8)))
      ; C++
      (h 'c++-mode-hook t 8
        '(progn
          (setq default-tab-width 8)))
      ; Clojure
      (h 'clojure-mode-hook nil 2)
      ; CSS
      (h 'css-mode-hook nil 2
        '(progn
          (setq css-indent-offset 2)))
      ; Elixir
      (h 'elixir-mode-hook nil 2)
      ; Emacs lisp
      (h 'emacs-lisp-mode-hook nil 2)
      ; Haskell
      (h 'haskell-mode-hook nil 2)
      ; Literate Haskell.
      (if (string-match "\\.lhs\\'" b)
        (progn
          (l/leader-def
            :keymaps 'local
            :states '(normal)
            "," 'hydra-literate-haskell/body)))
      ; HTML
      (h 'html-mode-hook t 4)
      ; Idris
      (h 'idris-mode-hook nil 2)
      ; Latex
      (h 'latex-mode-hook nil 2)
      ; Lua
      (h 'lua-mode-hook nil 2)
      ; Markdown
      (h 'markdown-mode-hook t 4
        '(progn
          (define-key markdown-mode-map [backspace] nil)
          (define-key markdown-mode-map [tab] nil)))
      ; Nix expression language
      (h 'nix-mode-hook nil 2)
      ; Org-mode
      (h 'org-mode-hook nil 2)
      ; Python
      (h 'python-mode-hook nil 4
        '(progn
          (setq python-indent 4)))
      ; Ruby
      (h 'ruby-mode-hook nil 2
        '(progn
          (setq ruby-indent-level 2)))
      ; Rust
      (h 'rust-mode-hook nil 4)
      ; Shell
      (h 'sh-mode-hook t 4)
      ; Shen
      (h 'shen-mode-hook nil 2)

      ; Additional project-specific settings.
      (if (or
          (l/kakapo-set-project-id "elementary-haskell" (concat h "/prog/elementary-haskell/") b)
          (l/kakapo-set-project-id "codex" "prog/codex/" b)
          (l/kakapo-set-project-id "new-keyboard (esrille nisse)" (concat h "/prog/nisse/new-keyboard/") b))
        (progn
          (h 'haskell-mode-hook nil 4)
          (h 'c-mode-hook nil 4
            '(progn
              (setq default-tab-width 4)))
          (h 'python-mode-hook nil 4
            '(progn
              (setq python-indent 4)))))
      (if (or
          (l/kakapo-set-project-id "miro" "prog/miro/" b)
          (l/kakapo-set-project-id "lovelace" "prog/lovelace/" b))
        (progn
          (add-hook 'haskell-mode-hook 'l/haskell-setup)
          (h 'haskell-mode-hook nil 2)
          (h 'c-mode-hook t 8
            '(progn
              (setq default-tab-width 8)))
          (h 'python-mode-hook nil 4
            '(progn
              (setq python-indent 4)))
          (h 'sh-mode-hook t 4)))
      (if (l/kakapo-set-project-id "2-space" "2-space" b)
        (progn
          (h 'sh-mode-hook nil 2)
          (h 'python-mode-hook nil 2
            '(progn
              (setq python-indent 2)))
          ))
      (if
          (l/kakapo-set-project-id "_git" "COMMIT_EDITMSG" b)
        (progn
          (setq-local indent-tabs-mode nil
                      show-trailing-whitespace nil)
          (global-git-commit-mode)))
      (if (l/kakapo-set-project-id "gv" "prog/gv/" b)
        (h 'sh-mode-hook nil 2)))))

; Per-project indentation rules.
(add-hook 'prog-mode-hook 'l/kakapo-indents)
(add-hook 'text-mode-hook 'l/kakapo-indents)
(add-hook 'conf-mode-hook 'l/kakapo-indents)
(add-hook 'css-mode-hook 'l/kakapo-indents)

(provide 'l-kakapo-mode)

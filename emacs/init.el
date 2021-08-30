; Add load-paths.
(add-to-list 'load-path "~/syscfg/emacs/l")
(add-to-list 'load-path "~/syscfg/emacs/l/major")
(add-to-list 'load-path "~/syscfg/emacs/l/minor")
(add-to-list 'load-path "~/syscfg/emacs/l/theme")
(add-to-list 'custom-theme-load-path "~/syscfg/emacs/l/theme")

; Load things in alphabetical order where possible, but grouped by their
; functionality.

; Load misc customizations.
(require 'l-misc)
(require 'l-scratch)

; Load minor-mode packages.
(require 'l-ace-window)
(require 'l-auto-dim-other-buffers)
(require 'l-column-enforce-mode)
(require 'l-conf-mode)
(require 'l-delight)
(require 'l-evil)
(require 'l-evil-collection)
(require 'l-evil-surround)
(require 'l-evil-visualstar)
(require 'l-evil-matchit)
(require 'l-flycheck)
(require 'l-general)
(require 'l-git-gutter)
(require 'l-helm)
(require 'l-helm-ag)
(require 'l-helm-projectile)
(require 'l-htmlize)
(require 'l-hydra)
(require 'l-kakapo-mode)
(require 'l-org-roam)
(require 'l-page-break-lines)
(require 'l-prog-mode)
(require 'l-rainbow-mode)
(require 'l-text-mode)
(require 'l-uim)
(require 'l-vdiff)

; Load major-mode packages.
(require 'l-bazel)
(require 'l-c)
(require 'l-c++)
(require 'l-clojure)
(require 'l-dhall)
(require 'l-dired)
(require 'l-docker)
(require 'l-elisp)
(require 'l-elixir)
(require 'l-erlang)
(require 'l-factor)
(require 'l-generic-x)
(require 'l-git-rebase)
(require 'l-go)
(require 'l-haskell-mode)
(require 'l-html)
(require 'l-idris)
(require 'l-latex)
(require 'l-ledger)
(require 'l-less-css)
(require 'l-lilypond)
(require 'l-lua)
(require 'l-magit)
(require 'l-markdown)
(require 'l-mmm-mode)
(require 'l-nix-mode)
(require 'l-notmuch)
(require 'l-org)
(require 'l-php)
(require 'l-polymode)
(require 'l-python)
(require 'l-ranger)
(require 'l-rust)
(require 'l-sass)
(require 'l-sh)
(require 'l-shell)
(require 'l-shen)
(require 'l-vterm)
(require 'l-yaml)

; Load things related to themes/appearance.
(require 'l-zenburn-theme)
(require 'l-hl-line+)
(require 'l-cosmetics)

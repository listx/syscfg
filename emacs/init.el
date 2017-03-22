; Bootstrap `use-package'. See
; http://www.lunaryorn.com/posts/my-emacs-configuration-with-use-package.html.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/")
	'("org" . "http://orgmode.org/elpa/"))
(package-initialize)
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

; Ensure that all packages handled by use-package are always installed if not
; present.
(setq use-package-always-ensure t)

; Add custom load-paths.
(add-to-list 'load-path "~/.emacs.d/l")
(add-to-list 'load-path "~/.emacs.d/l/major")
(add-to-list 'load-path "~/.emacs.d/l/minor")
(add-to-list 'load-path "~/.emacs.d/l/theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/l/theme")

; Macros.
; See
; http://emacs.stackexchange.com/questions/17420/how-to-pass-a-parameter-together-with-function.
(defmacro l/define-key-args (map key func &rest args)
	`(define-key ,map ,key (lambda () (interactive) (,func ,@args)))
)

(defmacro l/lambda-args (map key func &rest args)
	`(define-key ,map ,key (lambda () (interactive) (,func ,@args)))
)

(defmacro l/add-hook-args (hook func &rest args)
	`(add-hook ,hook (lambda () (interactive) (,func ,@args)))
)

; Load minor-mode packages.
(require 'l-ace-window)
(require 'l-column-enforce-mode)
(require 'l-conf-mode)
(require 'l-delight)
(require 'l-elscreen)
(require 'l-evil)
(require 'l-evil-ediff)
(require 'l-evil-magit)
(require 'l-evil-surround)
(require 'l-evil-visualstar)
(require 'l-evil-matchit)
(require 'l-general)
(require 'l-git-gutter)
(require 'l-helm)
(require 'l-helm-ag)
(require 'l-helm-projectile)
(require 'l-hiwin)
(require 'l-hl-line+)
(require 'l-hydra)
(require 'l-kakapo-mode)
(require 'l-page-break-lines)
(require 'l-prog-mode)
(require 'l-text-mode)
(require 'l-uim)

;; Load major-mode packages.
(require 'l-c)
(require 'l-c++)
(require 'l-clojure)
(require 'l-dired)
(require 'l-docker)
(require 'l-elisp)
(require 'l-generic-x)
(require 'l-git-rebase)
(require 'l-groovy)
(require 'l-haskell-mode)
(require 'l-html)
(require 'l-latex)
(require 'l-ledger)
(require 'l-less-css)
(require 'l-lilypond)
(require 'l-lua)
(require 'l-magit)
(require 'l-markdown)
(require 'l-mmm-mode)
(require 'l-nix-mode)
(require 'l-org)
(require 'l-php)
(require 'l-python)
(require 'l-sass)
(require 'l-sh)
(require 'l-shell)
(require 'l-yaml)

;; Load things related to themes/appearance.
(require 'l-alect-themes)
(require 'l-arjen-grey-theme)
(require 'l-zenmonk)
(require 'l-cosmetics)

;; Load misc customizations.
(require 'l-misc)
(require 'l-scratch)

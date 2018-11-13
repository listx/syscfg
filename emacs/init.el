; Helper function for determining system type.
(defun l/os (system)
  (interactive)
  (string-equal system-type system))

; If on Mac, make Emacs aware of special paths, esp. Homebrew (for `exec-path').
; Unfortunately, setting "PATH" does not automatically set `exec-path'. So, we
; do both for consistency. See
; http://ergoemacs.org/emacs/emacs_env_var_paths.html.
(if (l/os "darwin")
  (let*
    ((HOME (getenv "HOME"))
     (PATH (getenv "PATH"))
     (paths `(
      ,(concat HOME "/syscfg/script/sys")
      ,(concat HOME "/bin")
      ,(concat HOME "/homebrew/bin"))))
    (setenv "PATH" (concat (mapconcat 'identity paths ":") ":" PATH))
    ; The way in which `exec-path' is set here is hacky. It appears that
    ; exec-directory has little bearing to $PATH, so we just prepend $PATH to
    ; exec-directory and hope for the best. It could be that exec-directory is
    ; useless, but we give upstream code the benefit of the doubt.
    (setq exec-path
      (append
        (split-string (getenv "PATH") ":")
        (list exec-directory)))))

(if (l/os "gnu/linux")
  (let*
    ((HOME (getenv "HOME"))
     (PATH (getenv "PATH"))
     (paths `(
      ,(concat HOME "/syscfg/script/sys")
      ,(concat HOME "/.local/bin"))))
    (setenv "PATH" (concat (mapconcat 'identity paths ":") ":" PATH))
    (setq exec-path
      (append
        (split-string (getenv "PATH") ":")
        (list exec-directory)))))

; Increase garbage collection threshhold during startup to inhibit its
; activation. This way, startup can be a little bit faster. See
; https://github.com/nilcons/emacs-use-package-fast.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
  ;; restore after startup
  (setq gc-cons-threshold 800000)))

; For mac, increase initial frame size manually because we are not on XMonad.
(when (l/os "darwin")
  (add-to-list 'default-frame-alist '(height . 71))
  (add-to-list 'default-frame-alist '(width . 80)))

; Bootstrap `use-package'. See
; http://www.lunaryorn.com/posts/my-emacs-configuration-with-use-package.html.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives (append '(
  ("melpa" . "https://melpa.org/packages/")
  ("org" . "https://orgmode.org/elpa/")) package-archives))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

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
  `(define-key ,map ,key (lambda () (interactive) (,func ,@args))))

(defmacro l/add-hook-args (hook func &rest args)
  `(add-hook ,hook (lambda () (interactive) (,func ,@args))))

;; Load misc customizations.
(require 'l-misc)
(require 'l-scratch)

; Load minor-mode packages.
(require 'l-ace-window)
(require 'l-auto-dim-other-buffers)
(require 'l-column-enforce-mode)
(require 'l-conf-mode)
(require 'l-delight)
(require 'l-elscreen)
(require 'l-evil)
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
(require 'l-htmlize)
(require 'l-hydra)
(require 'l-kakapo-mode)
(require 'l-page-break-lines)
(require 'l-prog-mode)
(require 'l-text-mode)
(require 'l-uim)
(require 'l-vdiff)

;; Load major-mode packages.
(require 'l-c)
(require 'l-c++)
(require 'l-clojure)
(require 'l-dired)
(require 'l-docker)
(require 'l-elisp)
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
(require 'l-python)
(require 'l-ranger)
(require 'l-rust)
(require 'l-sass)
(require 'l-sh)
(require 'l-shell)
(require 'l-shen)
(require 'l-yaml)

;; Load things related to themes/appearance.
(require 'l-alect-themes)
(require 'l-arjen-grey-theme)
(require 'l-zenburn-theme)
(require 'l-zenmonk)
(require 'l-hl-line+)
(require 'l-cosmetics)

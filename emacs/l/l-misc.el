; Call the debugger when an error is signaled and not handled.
(setq debug-on-error t)
; Disable 'Package cl is deprecated' message.
; https://github.com/kiwanami/emacs-epc/issues/35#issuecomment-660639327
(setq byte-compile-warnings '(cl-functions))
; Work around a bug where esup tries to step into the byte-compiled version of
; `cl-lib', and fails horribly.
; See https://github.com/jschaf/esup/issues/54#issuecomment-651247749.
(setq esup-depth 0)
; Helper function for determining system type.
(defun l/os (system)
  (interactive)
  (string-equal system-type system))

; Replace all "yes or no" prompts with just "y or n".
(fset 'yes-or-no-p 'y-or-n-p)

; Try very hard to not split existing windows to open up links and other new
; buffers. https://stackoverflow.com/a/1856069/437583
(setq same-window-regexps '("."))
; Split the window vertically to the right (to create a new window as a
; "column") only if the current window width is greater than 162 character
; columns wide (163 minimum). This is basically to account for 2 windows of 80
; columns, plus 1 column each for the git-gutter, plus a 1-column separator (for
; terminal emacs) between the windows.
(setq split-width-threshold 162)
; Split the window horizontally (create a new window as a "row") only if we have
; at least 100 text lines available in the current window.
(setq split-height-threshold 45)

; If on Mac, make Emacs aware of special paths, esp. Homebrew (for `exec-path').
; Unfortunately, setting "PATH" does not automatically set `exec-path'. So, we
; do both for consistency. See
; http://ergoemacs.org/emacs/emacs_env_var_paths.html.
(if (l/os "darwin")
  (let*
    ((HOME (getenv "HOME"))
     (PATH (getenv "PATH"))
     (paths `(
      ,(concat HOME "/syscfg/script")
      ,(concat HOME "/bin")
      ,(concat HOME "/.nix-profile/bin"))))
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
      ,(concat HOME "/syscfg/script")
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

; Macros.
; See
; http://emacs.stackexchange.com/questions/17420/how-to-pass-a-parameter-together-with-function.
(defmacro l/define-key-args (map key func &rest args)
  `(define-key ,map ,key (lambda () (interactive) (,func ,@args))))

(defmacro l/add-hook-args (hook func &rest args)
  `(add-hook ,hook (lambda () (interactive) (,func ,@args))))

; Disable "*GNU Emacs*" startup buffer.
(setq inhibit-startup-screen t)

; Suppress defadvice redefinition warnings (such as by third party packages).
(setq ad-redefinition-action 'accept)

; Disable all version control minor modes (e.g., vc-git).
(setq vc-handled-backends nil)

; Emulate TextMate's "auto-paired characters".
(electric-pair-mode 1)

; Put all auto-saves/backups to the temp directory.
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

; Disable backups when on Mac. It really messes up org-mode's inter-file links
; for some reason, where a link to a file is replaced with a link to that file's
; backup when that file is edited. Gross.
(if (l/os "darwin")
  (setq make-backup-files nil))

; Save minibuffer history across sessions, limited to 1000 entries.
(savehist-mode 1)
(setq history-length 1000)

; Use different browsers for opening URLs, depending on the hostname. See
; https://stackoverflow.com/a/4506458.
(setq
  browse-url-browser-function 'browse-url-generic
  browse-url-generic-program
    (cond
      ((string-match "macbookpro" system-name) "google-chrome-mac.sh")
      (t "qutebrowser")))

; Scroll more like Vim (no jumping around).
(setq
  scroll-margin 3
  scroll-conservatively 100000
  scroll-preserve-screen-position 1)

; `M-x shell'
; Disable command echo.
(setq comint-process-echoes t)

; Set default line length (as used by 'fill-paragraph) to be 80 characters.
(setq-default fill-column 80)

; Add newline (silently) at the end of a file, just like Vim.
(setq require-final-newline t)

; Ignore "File changed on disk" minibuffer modal prompt for all buffers. We can
; always undo to get back buffers to a certain state. The relentless prompting
; whenever we edit a buffer from outside of emacs or through git is annoying and
; almost useless, given how frequently we save and stage into git.
(setq revert-without-query '(".*"))

; NixOS: This enables the various emacs scripts that are installed by
; nixos-rebuild. The script below is taken from
; https://gitorious.org/goibhniu/configuration-files. In particular, this ends
; up loading /run/current-system/sw/share/emacs/site-lisp/uim-el/* which has UIM
; elisp files (not found on Melpa).
(defun l/add-paths (paths)
  (mapc 'l/add-path paths))
(defun l/add-path (p)
  (add-to-list 'load-path p)
  (cd p)
  (normal-top-level-add-subdirs-to-load-path))
(let
  (
    (nixos-paths '("/run/current-system/sw/share/emacs/site-lisp")))
  (cond
    ((or
      (string-match "^k0" system-name)
      (string-match "^k1" system-name)
      (string-match "^m0" system-name))

      (l/add-paths nixos-paths))))

; Make paragraph-filling put 1 space after a period (full stop), not 2 spaces
; (the Vim equivalent of "gwap" in normal mode). Also see `fill-paragraph'.
(setq sentence-end-double-space nil)

; Set default major mode to text-mode.
(setq-default major-mode 'text-mode)

; For Mac, make 'get-device-terminal' always return DISPLAY. This fixes a bug
; where a graphical emacsclient will fail with a "Display ns does not exist"
; from this function whenever we try to open a new frame for a minibuffer, when
; we are invoking this from a graphical emacsclient. For reference the "ns"
; string is from (terminal-live-p nil), which denotes a Mac Cocoa graphical
; terminal.
(when (l/os "darwin")
  (defun get-device-terminal (device)
    (getenv "DISPLAY")))

; Disable alarm bell sound on Mac. For some reason, Emacs on Mac likes to sound
; the alarm a lot for small things, and what's worse, this alarm gets converted
; into a visual bell; but this visual alarm is horrible because Emacs has a GUI
; rendering bug, which results in a distorted buffer in the center of the screen
; (presumably where the visual bell would have been rendered). So, disable for
; now.
(when (l/os "darwin")
  (setq ring-bell-function 'ignore))

; For Mac, load brew paths. This is the analogue to NixOS's need to load system
; packages.
(if (l/os "darwin")
  ; Load brew paths
  (setq exec-path (append exec-path '("/usr/local/bin"))))

; TRAMP mode: use ssh by default.
(setq tramp-default-method "ssh")

; When going to a line, e.g. with the keys "200G" to go to line 200, make it so
; that that line is now at the center of the window. This avoids the situation
; where we open a buffer, then go to line 200 with "200G", but then have point
; be near the bottom of the screen, forcing us to scroll down to see more
; context.
(advice-add 'evil-goto-line :after #'evil-scroll-line-to-center)

; Inspired by evil-mark-replace
; (https://github.com/redguardtoo/evil-mark-replace).
(defun l/replace-in-buffer ()
  (interactive)
  (let
    (
      (replace-me (regexp-quote (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
        (thing-at-point 'word)))))
    ; Instead of calling vanilla `evil-ex', call it with a hook to set up
    ; the cursor's position as well. This way we can "pre-type" to the end
    ; of the string and then bring the cursor back where we want it.
    (minibuffer-with-setup-hook
      ; Move cursor back 3 columns, behind the trailing "/gc" string.
      (lambda () (backward-char 3))
      (evil-ex (concat
        "%s/"
        (if (region-active-p)
          replace-me
          (concat "\\<" replace-me "\\>"))
        "//gc")))))

(defun l/strip-leading-zeroes (str)
  (replace-regexp-in-string "^0+" "" str))

(defun l/copy-for-markdown (inject-line-numbers)
  "Copy region for markdown, and also optionally add line numbers."
  (interactive)
  (let*
    (
      (region-beg
        (save-excursion
          (goto-char (region-beginning))
          (line-beginning-position)))
      (region-end-no-newline
        (save-excursion
          (goto-char (region-end))
          (if (char-equal ?\n (progn (backward-char 1) (point)))
            (- (point) 1)
            (line-end-position))))
      (selection
        (buffer-substring-no-properties
          region-beg
          region-end-no-newline))
      ; Like `selection', but with line numbers.
      (selection-lines
        (let*
          ((line-beg (line-number-at-pos (region-beginning)))
            (line-end (line-number-at-pos
              region-end-no-newline))
            (line-num-list
              ; If we do an intra-line selection, the
              ; beginning and end regions will be on the same
              ; line. In this case, just return 1 single line.
              (if (= line-beg line-end)
                (list (number-to-string line-beg))
                (mapcar 'number-to-string
                  (number-sequence line-beg line-end))))
            (max-digits (length (car (last line-num-list)))))
          (mapcar
            (lambda (num-str)
              (if (< (length num-str) max-digits)
                (concat
                  (make-string
                    (- max-digits (length num-str)) ?0)
                  num-str)
                num-str))
            line-num-list)))
      (selection-with-lines
        (let
          (
            (line-num-and-line-pairs
              (cl-mapcar 'cons selection-lines
                (split-string selection "\n"))))
          (mapconcat
            (lambda (pair) (concat (car pair) " |" (cdr pair)))
            line-num-and-line-pairs "\n")))
      (copied
        (concat
          "```\n"
          (if inject-line-numbers selection-with-lines selection) "\n"
          "```\n"
          )))
    (progn
      (kill-new copied)
      ; Deselect selection.
      (deactivate-mark)
      (message "Copied markdown-friendly selection to clipboard"))))

(defun l/addrem-comment-region (b e f)
  "Use the `nox' command to comment the current region."
  (interactive)
  (shell-command-on-region
    ; beginning and end of buffer
    b e
    ; command and parameters
    (concat
      (if f
        "nox --sline "
        "nox --uncomment --sline ")
      (cl-case (with-current-buffer (current-buffer) major-mode)
        ('c-mode "//")
        ('clojure-mode "\\;")
        ('dhall-mode "--")
        ('emacs-lisp-mode "\\;")
        ('go-mode "//")
        ('haml-mode "-# --after-lw")
        ('haskell-mode "--")
        ('haskell-cabal-mode "--")
        ('java-mode "//")
        ('js-mode "//")
        ('ledger-mode "\\;")
        ('literate-haskell-mode "--")
        ('LilyPond-mode "%")
        ('latex-mode "%")
        ('plain-tex-mode "%")
        ('rust-mode "//")
        ('sass-mode "//")
        (t "\\#"))) ; default to shell syntax
    ; output buffer
    (current-buffer)
    ; replace?
    t
    ; name of the error buffer
    "*nox Error Buffer*"
    ; show error buffer?
    t))

(defun l/addrem-comment (f)
  (if (use-region-p)
    (progn
      (l/addrem-comment-region (region-beginning) (region-end) f)
      (evil-visual-char)
      (evil-exit-visual-state))
    (l/addrem-comment-region
      (line-beginning-position)
      (line-beginning-position 2)
      f)))

; Make xterm-paste turn on l/nonblank-inserted. This is so that when we go back
; into normal mode right after a paste, we don't undo the paste (as a result of
; invoking l/check-for-nonblank-insertion).
(advice-add 'xterm-paste :around #'l/mark-as-modified)
(defun l/mark-as-modified (orig-fun &rest args)
(prog1
  (apply orig-fun args)
  (setq l/nonblank-inserted t)))

; Fix "<dead-grave> is undefined" error.
(require 'iso-transl)

; Do not use init.el for auto-writing custom-set-variables definitions.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(provide 'l-misc)

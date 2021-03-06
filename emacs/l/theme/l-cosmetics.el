; Disable cursor blinking altogether.
(blink-cursor-mode 0)
; Do the same in Terminal emacs.
(setq visible-cursor nil)

; Disable GUI menu.
(menu-bar-mode 0)

; Disable GUI toolbar.
(tool-bar-mode 0)

; Disable buffer scrollbars.
(scroll-bar-mode 0)

; Enable native tabs.
(setq tab-bar-show t)
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
(setq tab-bar-separator "  ")

; Enable only left-side fringe.
(set-fringe-mode '(10 . 0))

; Visual line mode (word wrap on whole words) by default.
(global-visual-line-mode 1)

; Show column number of point.
(setq column-number-mode t)

; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

; Stretch the cursor (e.g., make it bigger if hovering over a tab).
(setq x-stretch-cursor 1)

; Show empty lines in the fringe area.
(setq-default indicate-empty-lines t)

; Show trailing whitespace.
(setq-default show-trailing-whitespace t)
; Disable trailing whitespace highlighting for certain non-text-editing modes.
(add-hook 'calendar-mode-hook 'l/hide-trailing-whitespace)
(add-hook 'special-mode-hook 'l/hide-trailing-whitespace)
; For the "*Messages*" buffer, we need the hook *and* the "with-current-buffer
; ..." invocation. This is because by the time this add-hook is run, the
; *Messages* buffer already exists and so the add-hook (at least initially) does
; nothing. Instead of trying to figure out where exactly we need to call this
; add-hook bit, instead we just take care of it with the "with-current-buffer"
; macro.
(add-hook 'messages-buffer-mode-hook 'l/hide-trailing-whitespace)
(defun l/hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))
(with-current-buffer "*Messages*" (l/hide-trailing-whitespace))
(advice-add 'org-switch-to-buffer-other-window :after
  #'(lambda (&rest _) (setq show-trailing-whitespace nil)))

; Highlight matching parentheses.
(show-paren-mode 1)

(defvar l/font-collection
  (cond
    ((l/os "darwin")
      '("Input Mono Condensed"))
    (t '("Terminus" "Input Mono Compressed"))))
(setq l/font-choice 0)
(defun l/cycle-font ()
  "Cycle through font collection."
  (interactive)
  (setq l/font-choice (mod (+ 1 l/font-choice) (length l/font-collection)))
  ; Only cycle if there is more than 1 font to cycle through.
  (when (> (length l/font-collection) 1)
    (progn
      (set-face-attribute
        'default
        nil
        :font
        (nth l/font-choice l/font-collection)
        :weight 'light)
      (set-face-attribute
        'tab-bar
        nil
        :font (nth l/font-choice l/font-collection))
      (set-face-attribute
        'tab-bar-tab
        nil
        :font (nth l/font-choice l/font-collection))
      (set-face-attribute
        'tab-bar-tab-inactive
        nil
        :font (nth l/font-choice l/font-collection))
      (redraw-display))))

(defvar l/themes
  '(
    zenburn)
  "Default themes")

(defvar l/theme-idx 0)

(defun l/theme-name ()
  (nth l/theme-idx l/themes))

; Install packages if they are missing.
(defun l/cycle-theme ()
  (interactive)
  (progn
    ; Increment current theme index
    (setq l/theme-idx (mod (+ 1 l/theme-idx) (length l/themes)))

    ; Apply the theme
    (load-theme (l/theme-name) t)
    (l/theme-hook)))

; Set colors depending on theme name.
(defun l/theme-hook ()
  (interactive)
  (let
    (
      (theme (format "%s" (l/theme-name))))
    ; TODO: Insert theme-specific adjustments here.
    (progn
      (set-face-attribute 'auto-dim-other-buffers-face nil :foreground "grey" :background "black")
      (set-face-attribute 'lazy-highlight nil :foreground "pink" :background "dark red" :weight 'normal)
      (set-face-attribute 'isearch nil :foreground "dark red" :background "pink" :weight 'bold)
      (set-face-attribute 'region nil :foreground "dark red" :background "pink" :weight 'bold)

      ; Tab bar colors.
      (set-face-attribute 'tab-bar nil :font (nth l/font-choice l/font-collection) :height 100 :background "grey32")
      (set-face-attribute 'tab-bar-tab nil :font (nth l/font-choice l/font-collection) :height 100 :weight 'bold :box nil :background "pink")
      (set-face-attribute 'tab-bar-tab-inactive nil :font (nth l/font-choice l/font-collection) :height 100 :weight 'bold :box nil :foreground "grey80" :background "grey32")

      ; Fix ugly colors for diffs. Prevalent because of git comit message buffers
      ; like COMMIT_EDITMSG.
      (set-face-attribute 'font-lock-comment-face nil :foreground "#9fc59f")
      (use-package git-commit
        :config (set-face-attribute 'git-commit-summary nil :foreground "cyan1" :background "grey25"))
      (set-face-attribute 'diff-added nil :foreground "green" :background "dark green")
      (set-face-attribute 'diff-removed nil :foreground "red" :background "dark red")
      (set-face-attribute 'diff-context nil :foreground "#ffffff" :background "grey25")
      (set-face-attribute 'diff-file-header nil :foreground "yellow" :background "grey25" :weight 'bold)
      (set-face-attribute 'diff-header      nil :foreground "yellow" :background "grey25" :weight 'bold)
      (set-face-attribute 'diff-hunk-header nil :foreground "cyan"   :background "grey25")
      (set-face-attribute 'diff-hunk-header nil :foreground "cyan"   :background "grey25")
      (set-face-attribute 'hl-line nil :background "grey32"))))

; If we're on our laptop, make the text slightly bigger to match my desktop's
; behavior.
(defun l/text-height ()
  (cond
    ((string-match "^k[123]" system-name)
      102
    )
    ((l/os "darwin")
      144
    )
    (t 91)))

(defun l/text-weight ()
  (cond
    ((l/os "darwin")
       ; Use lightest setting on Mac, because for some reason it's just a little bit "bolder" than usual.
      'ultra-light
    )
    (t 'normal)))

; auto-generated stuff by emacs itself...

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight ,(l/text-weight) :height ,(l/text-height) :width normal :foundry "xos4" :family ,(nth 0 l/font-collection))))))


; If we give emacs an argument (e.g., file or directory) when we invoke it,
; emacs automatically sets the default directory (to search for when we want to
; open other files) to the directory that holds the given file/directory
; argument. If there is no argument, then always set it to the home directory.
; We have to set it last because other packages/init stuff can change the value
; of this variable.
(if (= (length argv) 0)
  (setq default-directory "~/"))

; Set the first theme.
(load-theme (l/theme-name) t)
(l/theme-hook)

(provide 'l-cosmetics)

; Disable cursor blinking altogether.
(blink-cursor-mode 0)

; Disable GUI menu.
(menu-bar-mode 0)

; Disable GUI toolbar.
(tool-bar-mode 0)

; Disable buffer scrollbars.
(scroll-bar-mode 0)

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
; NOTE: doing
;
;   (setq-default show-trailing-whitespace t)
;
; interferes with scrolling with elscreen on, where scrolling up requires moving
; point up twice. To get around this, we enable global-whitespace-mode instead.
(setq whitespace-style '(face trailing))
(global-whitespace-mode 1)

; Highlight matching parentheses.
(show-paren-mode 1)

(defvar l/font-collection
  (cond
    ((l/os "darwin")
      '("Input Mono Condensed"))
    (t '("Terminus" "Input Mono Narrow" "Input Mono Compressed Book"))))
(setq l/font-choice 0)
(defun l/cycle-font ()
  "Cycle through font collection."
  (interactive)
  (setq l/font-choice (mod (+ 1 l/font-choice) (length l/font-collection)))
  (if (> 0 (length l/font-collection))
    (set-face-attribute
      'default
      nil
      :font
      (nth l/font-choice l/font-collection))
    (redraw-display)))

(defvar l/themes
  '(
    arjen-grey
    alect-light
    alect-dark
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
    (l/theme-hook)
    (when (string= "arjen-grey" (l/theme-name)) (l/arjen-hook))))

; Set colors depending on theme name.
(defun l/theme-hook ()
  (interactive)
  (let
    (
      (theme (format "%s" (l/theme-name))))
    (cond
      ((string= "alect-light" theme)
        (progn
          (set-face-background 'hiwin-face
            ; set default to alect-light
            (if (display-graphic-p) "#ded6c5" "gray16"))
          (set-face-background 'auto-dim-other-buffers-face
            (if (display-graphic-p) "#ded6c5" "gray16"))
          (setq evil-insert-state-cursor '("#000000" box))
          (setq evil-normal-state-cursor '("DodgerBlue1" box))))
      ((or
          (string= "alect-dark" theme)
          (string= "alect-black" theme))
        (progn
          (set-face-background 'hiwin-face
            (if (display-graphic-p)
              (if (string= "alect-dark" theme)
                "#0d0d0f"
                "gray0")
              "gray16"))
          (set-face-background 'auto-dim-other-buffers-face
            (if (display-graphic-p)
              (if (string= "alect-dark" theme)
                "#0d0d0f"
                "gray0")
              "gray16"))
          (setq evil-insert-state-cursor '("#ffffff" box))
          (setq evil-normal-state-cursor '("#00ff00" box))))
      ((string= "arjen-grey" theme)
        (progn
          (set-face-background 'hiwin-face
            (if (display-graphic-p)
              "#0d0d0f"
              "gray16"))
          (set-face-background 'auto-dim-other-buffers-face
            (if (display-graphic-p)
              "#0d0d0f"
              "gray16"))
          (setq evil-insert-state-cursor '("#ffffff" box))
          (setq evil-normal-state-cursor '("#00ff00" box)))))
  ; Colorize custom TODO-like keywords for org-mode.
  (setq org-todo-keyword-faces '(
    ("GOAL" . (:foreground "firebrick" :background "pink" :weight bold))
    ("TODO" . org-warning)
    ("TASK" . org-warning)
    ("WAITING" . "purple")
    ("STARTED" . "yellow")
    ("DONE" . org-done)
    ("CANCELED" . (:foreground "orange" :weight bold))))))

(defun l/arjen-hook ()
  (interactive)
  (set-face-attribute 'mode-line-buffer-id nil :foreground "black" :distant-foreground "red")
  (set-face-attribute 'mode-line nil :foreground "black" :background "pink" :box '(:line-width 2 :color "pink" :style nil))
  (set-face-attribute 'mode-line-inactive nil :foreground "grey64" :box '(:line-width 2 :color "grey32" :style nil))
  (set-face-attribute 'column-enforce-face nil :inherit 'default)
  (set-face-attribute 'hl-line nil :background "#1a2126")
  (set-face-attribute 'region nil :foreground "#7a1717" :background "pink" :weight 'bold)
  (set-face-attribute 'lazy-highlight nil :foreground "pink" :background "#7a1717" :weight 'normal)
  (set-face-attribute 'isearch nil :foreground "#7a1717" :background "pink" :weight 'bold)
  (set-face-attribute 'helm-candidate-number nil :foreground "#7a1717" :background "pink" :weight 'bold)
  (set-face-attribute 'org-todo nil :foreground "indian red" :weight 'bold)
  (set-face-attribute 'org-level-1 nil :foreground "pink" :weight 'bold)
  (set-face-attribute 'org-level-2 nil :foreground "aquamarine" :weight 'bold)
  (set-face-attribute 'org-level-3 nil :weight 'bold)
  (set-face-attribute 'org-level-4 nil :inherit 'org-level-7 :weight 'bold)
  (set-face-attribute 'org-level-5 nil :foreground "light cyan" :weight 'bold)
  (set-face-attribute 'org-level-6 nil :foreground "RosyBrown2" :weight 'bold)
  (set-face-attribute 'org-level-7 nil :weight 'bold)
  (set-face-attribute 'org-level-8 nil :weight 'bold)
  (set-face-attribute 'git-gutter:added nil :foreground "lime green")
  (set-face-attribute 'git-gutter:modified nil :foreground "purple")
  (set-face-attribute 'git-gutter:deleted nil :foreground "red")
  )

; Select theme based on GUI or ncurses mode.
(if (display-graphic-p)
  (progn
    (load-theme 'arjen-grey t)
    (l/theme-hook)
    (l/arjen-hook))
  (progn
    (set-face-attribute 'auto-dim-other-buffers-face nil :foreground "white" :background "black")
    (set-face-attribute 'lazy-highlight nil :foreground "pink" :background "dark red" :weight 'normal)
    (set-face-attribute 'isearch nil :foreground "dark red" :background "pink" :weight 'bold)
    (set-face-attribute 'region nil :foreground "dark red" :background "pink" :weight 'bold)
    ; Fix ugly colors for diffs. Prevalent because of git comit message buffers
    ; (COMMIT_EDITMSG).
    (set-face-attribute 'default nil :foreground "#ffffff" :background "gray25")
    (set-face-attribute 'font-lock-comment-face nil :foreground "light green" :background "gray25")
    (use-package git-commit
      :config (set-face-attribute 'git-commit-summary nil :foreground "cyan1" :background "gray25"))
    (set-face-attribute 'diff-added nil :foreground "green" :background "dark green")
    (set-face-attribute 'diff-removed nil :foreground "red" :background "dark red")
    (set-face-attribute 'diff-context nil :foreground "#ffffff" :background "gray25")
    (set-face-attribute 'diff-file-header nil :foreground "yellow" :background "gray25" :weight 'bold)
    (set-face-attribute 'diff-header      nil :foreground "yellow" :background "gray25" :weight 'bold)
    (set-face-attribute 'diff-hunk-header nil :foreground "cyan"   :background "gray25")
    (set-face-attribute 'hl-line nil :background "dim gray")))

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

; auto-generated stuff by emacs itself...

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height ,(l/text-height) :width normal :foundry "xos4" :family ,(nth 0 l/font-collection))))))


; If we give emacs an argument (e.g., file or directory) when we invoke it,
; emacs automatically sets the default directory (to search for when we want to
; open other files) to the directory that holds the given file/directory
; argument. If there is no argument, then always set it to the home directory.
; We have to set it last because other packages/init stuff can change the value
; of this variable.
(if (= (length argv) 0)
  (setq default-directory "~/"))

(provide 'l-cosmetics)

; Custom functions {{{
(setq my-current-font 0)
(defun my-toggle-font ()
    "Toggle font between Terminus and DejaVu Sans Mono"
    (interactive)
    (setq my-current-font (if (= my-current-font 0) 1 0))
    (set-face-attribute 'default nil :font (if (= my-current-font 1) "DejaVu Sans Mono" "Terminus"))
    (redraw-display))
;}}}

; Load paths and modes {{{
; Evil, the Extensible VI Layer! This makes Emacs worth using.
; see http://gitorious.org/evil/pages/Home
(require 'evil)
(evil-mode 1)

; add load path for custom scripts
(add-to-list 'load-path "~/.emacs.d/script")

; YAML major mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
; org-mode
; force use of installed org-mode (not the one that comes by default with emacs)
(require 'org-install)
; ditaa program (and integration with org-mode)
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar") ; load path for ditaa
(require 'org-exp-blocks)
; start up org-mode for .org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
; write timestamp when a TODO changes to DONE
(setq org-log-done t)
(setq org-agenda-files (list "~/org"))

; yasnippet
(add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "/usr/share/emacs/site-lisp/yas/snippets")
;}}}

; Appearance {{{
; zenburn color theme
(require 'color-theme-zenburn)
(color-theme-zenburn)
; highlight the current cursor line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#434443")
; remove splash screen
(setq inhibit-splash-screen t)
; remove toolbar
(tool-bar-mode -1)
; visual line mode (word wrap on whole words) by default
(global-visual-line-mode 1)
; green cursor
(set-cursor-color "#00ff00")
; stretch the cursor (e.g., make it bigger if hovering over a tab)
(setq x-stretch-cursor 1)
; show empty whitespace
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
; toggle between fonts
(define-key evil-normal-state-map ",f" 'my-toggle-font)

; settings used by "emacsclient -c" command
(setq default-frame-alist '((font-backend . "xft")
                            ;(font . "Terminus")
                            ;(background-color . "black")
                            ;(foreground-color . "white")
                            ;(vertical-scroll-bars)
                            (left-fringe . -1)
                            (right-fringe . -1)
                            (fullscreen . fullboth)
                            ;(menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            ))

; auto-generated stuff by emacs itself...
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(scroll-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#22222a" :foreground "#cccccf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "xos4" :family "Terminus")))))
;}}}

; Keymaps {{{
; some keymaps from ~/.vimrc
(define-key evil-insert-state-map [f1] 'save-buffer) ; save
(define-key evil-normal-state-map [f1] 'save-buffer) ; save
(define-key evil-normal-state-map ",w" 'save-buffer) ; save
(define-key evil-normal-state-map ",q" ":q") ; close current window
(define-key evil-normal-state-map ",x" 'save-buffers-kill-emacs) ; save and quit
; window management and navigation
(define-key evil-normal-state-map ",h" 'split-window-vertically)
(define-key evil-normal-state-map ",v" 'split-window-horizontally)
(define-key evil-normal-state-map [tab] 'other-window) ; move to other window
; make "kj" behave as ESC key, adapted from http://article.gmane.org/gmane.emacs.vim-emulation/980
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))
; simulate vim's "nnoremap <space> 10jzz"
(define-key evil-normal-state-map " " (lambda ()
                     (interactive)
                     (next-line 10)
                     (evil-scroll-line-down 10)
                     ))
; simulate vim's "nnoremap <backspace> 10kzz"
(define-key evil-normal-state-map (kbd "DEL") (lambda ()
                     (interactive)
                     (previous-line 10)
                     (evil-scroll-line-up 10)
                     ))

; make evil work for org-mode!
(defun always-insert-item ()
     (interactive)
     (if (not (org-in-item-p))
       (insert "\n- ")
       (org-insert-item)))

(evil-declare-key 'normal org-mode-map (kbd "M-o") (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-heading)
                     (evil-append nil)
                     ))
(evil-declare-key 'normal org-mode-map "O" (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-heading)
                     (org-metaright)
                     (evil-append nil)
                     ))
(evil-declare-key 'normal org-mode-map "o" (lambda ()
                     (interactive)
                     (end-of-line)
                     (always-insert-item)
                     (evil-append nil)
                     ))

(evil-declare-key 'normal org-mode-map "t" (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-todo-heading nil)
                     (evil-append nil)
                     ))
(evil-declare-key 'normal org-mode-map (kbd "M-t") (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-todo-heading nil)
                     (org-metaright)
                     (evil-append nil)
                     ))
(evil-declare-key 'normal org-mode-map (kbd "C-o") 'org-toggle-heading) ; convert a plain list into a heading
(evil-declare-key 'normal org-mode-map "T" 'org-todo) ; mark a TODO item as DONE
(evil-declare-key 'normal org-mode-map ";a" 'org-agenda) ; access agenda buffer
(evil-declare-key 'normal org-mode-map "-" 'org-cycle-list-bullet) ; change bullet style

; allow us to access org-mode keys directly from Evil's Normal mode
(evil-declare-key 'normal org-mode-map "L" 'org-shiftright)
(evil-declare-key 'normal org-mode-map "H" 'org-shiftleft)
(evil-declare-key 'normal org-mode-map "K" 'org-shiftup)
(evil-declare-key 'normal org-mode-map "J" 'org-shiftdown)
(evil-declare-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
(evil-declare-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
(evil-declare-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
(evil-declare-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
(evil-declare-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-declare-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-declare-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-declare-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)

(evil-declare-key 'normal org-mode-map (kbd "<f12>") 'org-export-as-html)
;}}}

; Backups {{{
; put all auto-saves/backups to the temp directory
(setq backup-directory-alist
                `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
                `((".*" ,temporary-file-directory t)))
;}}}

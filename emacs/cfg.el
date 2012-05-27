; TODO
; - fixup zenburn-emacs colors more to resemble Brockman's originals

; Load paths, scripts, packages, etc. {{{
; add load path for custom scripts
(add-to-list 'load-path "~/.emacs.d/script")
;}}}

; Custom functions {{{
(setq my-current-font 0)
(defun my-toggle-font ()
	"Toggle font between Terminus and DejaVu Sans Mono"
	(interactive)
	(setq my-current-font (if (= my-current-font 0) 1 0))
	(set-face-attribute 'default nil :font (if (= my-current-font 1) "DejaVu Sans Mono" "Terminus"))
	(redraw-display))
; custom indentation function: copy the indentation of the previous line
; adopted from http://sequence.complete.org/node/365
(defun newline-and-indent-relative ()
	(interactive)
	(newline)
	(indent-to-column (save-excursion
		(forward-line -1)
		(back-to-indentation)
		(current-column)))
)
(defun kill-this-buffer-volatile ()
	"Kill current buffer unconditionally."
	(interactive)
	(set-buffer-modified-p nil)
	(kill-this-buffer))
;}}}

; General indentation behavior {{{
; pressing TAB inserts a TAB
(define-key text-mode-map (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "TAB") 'self-insert-command)
;}}}

; Modes {{{

; Evil {{{
; Evil, the Extensible VI Layer! This makes Emacs worth using.
; see http://gitorious.org/evil/pages/Home
(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map [f1] 'save-buffer) ; save
(define-key evil-normal-state-map [f1] 'save-buffer) ; save
(define-key evil-normal-state-map ",w" 'save-buffer) ; save
(define-key evil-normal-state-map ",W" ":w!") ; force save
(define-key evil-normal-state-map ",q" ":q") ; close current window
(define-key evil-normal-state-map ",Q" ":q!") ; close current window, *even if modified*
(define-key evil-normal-state-map ",d" 'kill-this-buffer) ; kill current buffer without confirmation
(define-key evil-normal-state-map ",D" 'kill-this-buffer-volatile) ; kill current buffer without confirmation, *even if modified*
(define-key evil-normal-state-map ",x" 'save-buffers-kill-emacs) ; save and quit
(define-key evil-normal-state-map ",y" "\"+y") ; copy to X primary clipboard
(define-key evil-normal-state-map ",p" "\"+p") ; paste (after cursor) X primary clipboard
(define-key evil-normal-state-map ",P" "\"+P") ; paste (before cursor) X primary clipboard
; navigation
; simulate vim's "nnoremap <space> 10jzz"
(define-key evil-normal-state-map " "
	(lambda ()
		(interactive)
		(next-line 10)
		(evil-scroll-line-down 10)
	)
)
; simulate vim's "nnoremap <backspace> 10kzz"
(define-key evil-normal-state-map (kbd "DEL")
	(lambda ()
		(interactive)
		(previous-line 10)
		(evil-scroll-line-up 10)
	)
)
(define-key evil-normal-state-map ",h" 'split-window-vertically)
(define-key evil-normal-state-map ",v" 'split-window-horizontally)
(define-key evil-normal-state-map [tab] 'other-window) ; move to other window
; Change K from being mapped to interactive man pages to being used as the
; vanilla comma ',' key's functionality (intra-line backwards search repeat for
; any t, T, f, F searches).
(define-key evil-normal-state-map "K" 'evil-repeat-find-char-reverse)
; buffer movement
(define-key evil-normal-state-map "H" 'evil-next-buffer)
(define-key evil-normal-state-map "L" 'evil-prev-buffer)
; new buffer
(define-key evil-normal-state-map ",n" 'evil-window-new)
; remove trailing whitespace
(define-key evil-normal-state-map ",e" 'delete-trailing-whitespace)
; set line ending to UNIX
(define-key evil-normal-state-map ",E" (lambda () (interactive) (set-buffer-file-coding-system 'utf-8-unix t)))
; replace all /r/n with just /n
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

;}}}

; Elscreen {{{
(load "elscreen" "ElScreen" t)
;(require 'elscreen)
;(elscreen-start)
; new vimlike "tab", aka "screen"
(define-key evil-normal-state-map ",N" 'elscreen-create)
(define-key evil-normal-state-map ",c" 'elscreen-kill) ; close current elscreen screen (elscreen's screen is inside a window, and is thus more granular than 'close-window)
(define-key evil-normal-state-map (kbd "C-h") 'elscreen-previous)
(define-key evil-normal-state-map (kbd "C-l") 'elscreen-next)
; }}}

; Org-mode {{{
(require 'org-exp-blocks)
; force use of installed org-mode (not the one that comes by default with emacs)
(require 'org-install)
; ditaa program (and integration with org-mode)
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar") ; load path for ditaa
; start up org-mode for .org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
; write timestamp when a TODO changes to DONE
(setq org-log-done t)
(setq org-agenda-files (list "~/org"))

(defun always-insert-item ()
	(interactive)
	(if (not (org-in-item-p))
	(insert "\n- ")
	(org-insert-item))
)
(evil-declare-key 'normal org-mode-map (kbd "M-o")
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-heading)
		(evil-append nil)
	)
)
(evil-declare-key 'normal org-mode-map "O"
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-heading)
		(org-metaright)
		(evil-append nil)
	)
)
(evil-declare-key 'normal org-mode-map "o"
	(lambda ()
		(interactive)
		(end-of-line)
		(always-insert-item)
		(evil-append nil)
	)
)

(evil-declare-key 'normal org-mode-map "t"
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-todo-heading nil)
		(evil-append nil)
	)
)
(evil-declare-key 'normal org-mode-map (kbd "M-t")
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-todo-heading nil)
		(org-metaright)
		(evil-append nil)
	)
)
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

; C {{{
(add-hook 'c-mode-hook
	(lambda ()
		(c-set-style "linux")
	)
)
;}}}

; Haskell {{{
; adopted from http://sequence.complete.org/node/365
(load-library "haskell-site-file")
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)
; 4-space tabs
(add-hook 'haskell-mode-hook
	(lambda ()
		(turn-on-haskell-doc-mode)
		;(turn-on-haskell-simple-indent)
		;(setq indent-line-function 'tab-to-tab-stop)
		;(setq tab-stop-list
		;(loop for i from 2 upto 120 by 2 collect i))
		;(local-set-key (kbd "RET") 'newline-and-indent-relative)
		(setq tab-width 4)
		(setq indent-tabs-mode t)
	)
)
; make indentation saner when inserting new lines, whether from insert mode or normal mode
(evil-declare-key 'insert haskell-mode-map (kbd "RET") 'newline-and-indent-relative)
(evil-declare-key 'normal haskell-mode-map "o"
	(lambda ()
		(interactive)
		(evil-append-line 1)
		(newline-and-indent-relative)
	)
)
(evil-declare-key 'normal haskell-mode-map "O"
	(lambda ()
		(interactive)
		(evil-window-up)
		(evil-append-line 1)
		(newline-and-indent-relative)
	)
)
;}}}

; YAML {{{
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
; disable YAML keymaps, as they interfere with Evil (especially the [backspace] keymap)
(setq yaml-mode-map (make-sparse-keymap))
;}}}

;}}}

; Appearance {{{
; zenburn color theme
(require 'color-theme-zenburn-mod)
(color-theme-zenburn-mod)
; rainbow-colored matching parentheses, braces, etc.
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)
; change background of hex color strings to the actual color (activate with rainbow-mode)
(require 'rainbow-mode)
; highlight matching parenthese
(show-paren-mode 1)
; highlight the current cursor line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#323332")
; disable cursor line highlight during insert mode
(add-hook 'evil-insert-state-entry-hook
	(lambda ()
		(interactive)
		(global-hl-line-mode 0)
	)
)
(add-hook 'evil-insert-state-exit-hook
	(lambda ()
		(interactive)
		(global-hl-line-mode 1)
		; like Vim, remove whitespace if nothing was inserted
		(delete-trailing-whitespace)
	)
)
(add-hook 'evil-visual-state-entry-hook
	(lambda ()
		(interactive)
		(global-hl-line-mode)
	)
)
(add-hook 'evil-visual-state-exit-hook
	(lambda ()
		(interactive)
		(global-hl-line-mode)
	)
)
(add-hook 'evil-emacs-state-entry-hook
	(lambda ()
		(interactive)
		(global-hl-line-mode 0)
	)
)
(add-hook 'evil-emacs-state-exit-hook
	(lambda ()
		(interactive)
		(global-hl-line-mode 1)
	)
)
; cursor colors for the various states
(setq evil-insert-state-cursor '("#ffffff" box))
(setq evil-emacs-state-cursor '("#ff0000" box))
(setq evil-normal-state-cursor '("#00ff00" box))
; remove splash screen
(setq inhibit-splash-screen t)
; remove toolbar
(tool-bar-mode -1)
; visual line mode (word wrap on whole words) by default
(global-visual-line-mode 1)
; green cursor
(set-cursor-color "#00ff00")
; make the color in set-cursor-color be the default color recognized by Evil
; (for the non-customized states, such as visual mode)
; (this is required because we change the color *after* enabling evil)
(setq evil-default-cursor t)
; stretch the cursor (e.g., make it bigger if hovering over a tab)
(setq x-stretch-cursor 1)
; show empty whitespace
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
; toggle between fonts
(define-key evil-normal-state-map ",f" 'my-toggle-font)
; default tab width is 4
(setq default-tab-width 4)
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
	)
)
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

; Backups {{{
; put all auto-saves/backups to the temp directory
(setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
;}}}

; Misc {{{
; always follow symlink that points to a version-controlled file
(setq vc-follow-symlinks t)
;}}}

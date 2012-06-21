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
; Either close the current elscreen, or if only one screen, use the ":q" Evil
; command; this simulates the ":q" behavior of Vim when used with tabs.
(defun vimlike-quit ()
  "Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab (elscreen)."
  (interactive)
  (let ((one-elscreen (elscreen-one-screen-p))
        (one-window (one-window-p))
        )
    (cond
     ; if current tab has split windows in it, close the current live window
     ((not one-window)
      (delete-window) ; delete the current window
      (balance-windows) ; balance remaining windows
      nil)
     ; if there are multiple elscreens (tabs), close the current elscreen
     ((not one-elscreen)
      (elscreen-kill)
      nil)
     ; if there is only one elscreen, just try to quit (calling elscreen-kill
     ; will not work, because elscreen-kill fails if there is only one
     ; elscreen)
     (one-elscreen
      (evil-quit)
      nil)
     )))
; A function that behaves like Vim's ':tabe' commnad for creating a new tab and
; buffer (the name "[No Name]" is also taken from Vim).
(defun vimlike-:tabe ()
  "Vimlike ':tabe' behavior for creating a new tab and buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "[No Name]")))
      ; create new tab
      (elscreen-create)
      ; set window's buffer to the newly-created buffer
      (set-window-buffer (selected-window) buffer)
      ; set state to normal state
      (with-current-buffer buffer
        (evil-normal-state))
    )
  )
;; Adopted from mortenee's answer at http://stackoverflow.com/questions/2662655/automatically-closing-the-scratch-buffer
(defun my-close-scratch ()
  ; kill the annoying *scratch* lisp evaluation buffer
  (kill-buffer "*scratch*")
  ; create [No Name] buffer if we open emacs without specifying any files to edit
  (if (not (delq nil (mapcar 'buffer-file-name (buffer-list))))
      (new-untitled-buffer)
    ))
; custom startup hook
(defun my-emacs-startup-hook ()
  (my-close-scratch))
; add custom startup hook to emacs' startup hook
(add-hook 'emacs-startup-hook 'my-emacs-startup-hook)
; create a new empty buffer
(defun new-untitled-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "[No Name]")))
    (switch-to-buffer buf)
    (normal-mode)
    (setq buffer-offer-save t))
  )
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
(define-key evil-normal-state-map [f11] 'menu-bar-mode) ; toggle the menu bar
(define-key evil-normal-state-map ",w" 'save-buffer) ; save
(define-key evil-normal-state-map ",W" ":w!") ; force save
(define-key evil-normal-state-map ",q" 'vimlike-quit) ; close current elscreen, or current window if only one elscreen
(define-key evil-normal-state-map ",Q" ":q!") ; close current window, *even if modified*
(define-key evil-normal-state-map ",d" 'kill-this-buffer) ; kill current buffer without confirmation
(define-key evil-normal-state-map ",D" 'kill-this-buffer-volatile) ; kill current buffer without confirmation, *even if modified*
(define-key evil-normal-state-map ",x" 'save-buffers-kill-emacs) ; save and quit
(define-key evil-normal-state-map ",y" "\"+y") ; copy to X primary clipboard
(define-key evil-normal-state-map ",p" "\"+p") ; paste (after cursor) X primary clipboard
(define-key evil-normal-state-map ",P" "\"+P") ; paste (before cursor) X primary clipboard
(define-key evil-normal-state-map "gw" 'fill-paragraph) ; insert hard line breaks
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
(define-key evil-normal-state-map ",h" (lambda () (interactive) (split-window-vertically) (balance-windows)))
(define-key evil-normal-state-map ",v" (lambda () (interactive) (split-window-horizontally) (balance-windows)))
(define-key evil-normal-state-map [tab] 'other-window) ; move to other window
; Change K from being mapped to interactive man pages to being used as the
; vanilla comma ',' key's functionality (intra-line backwards search repeat for
; any t, T, f, F searches).
(define-key evil-normal-state-map "K" 'evil-repeat-find-char-reverse)
; buffer movement
(define-key evil-normal-state-map "H" 'evil-next-buffer)
(define-key evil-normal-state-map "L" 'evil-prev-buffer)
; new buffer
(define-key evil-normal-state-map ",n" 'vimlike-:tabe)
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
; new vimlike "tab", aka "screen"
(define-key evil-normal-state-map ",N" 'elscreen-create)
; tab navigation
(define-key evil-normal-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-normal-state-map (kbd "C-h") 'elscreen-previous)
(define-key evil-insert-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-insert-state-map (kbd "C-h") 'elscreen-previous)
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
		(setq default-tab-width 8)
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
		(previous-line)
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
; a custom syntax highlighting setup for just numbers (hex, decimal, etc.);
; adopted from http://stackoverflow.com/questions/8860050/emacs-c-mode-how-do-you-syntax-highlight-hex-numbers
(make-face 'font-lock-hex-face)
(setq font-lock-hex-face 'font-lock-hex-face)
(make-face 'font-lock-float-face)
(setq font-lock-float-face 'font-lock-float-face)
(make-face 'font-lock-decimal-face)
(setq font-lock-decimal-face 'font-lock-decimal-face)
(make-face 'font-lock-octal-face)
(setq font-lock-octal-face 'font-lock-octal-face)
(setq number-mode-list
	'(	c-mode-hook
		c++-mode-hook
		lisp-mode-hook
		emacs-lisp-mode-hook
		haskell-mode-hook
		python-mode-hook
		cperl-mode-hook
	)
)
(dolist (mode number-mode-list)
	(add-hook mode
		'(lambda ()
			(font-lock-add-keywords nil
				'(
					; Valid hex number (will highlight invalid suffix though)
					("\\b0x[[:xdigit:]]+[uUlL]*\\b" . font-lock-hex-face)
					; Valid floating point number.
					("\\(\\b[0-9]+\\|\\)\\(\\.\\)\\([0-9]+\\(e[-]?[0-9]+\\)?\\([lL]?\\|[dD]?[fF]?\\)\\)\\b" (1 font-lock-float-face) (2 font-lock-decimal-face) (3 font-lock-float-face))
					; Valid decimal number.  Must be before octal regexes otherwise 0 and 0l
					; will be highlighted as errors.  Will highlight invalid suffix though.
					("\\b\\(\\(0\\|[1-9][0-9]*\\)[uUlL]*\\)\\b" 1 font-lock-decimal-face)
					; Valid octal number
					("\\b0[0-7]+[uUlL]*\\b" . font-lock-octal-face)
				)
			)
		)
	)
)

; zenburn color theme
(add-to-list 'custom-theme-load-path "/usr/share/emacs/site-lisp/zenburn-emacs-mod")
(load-theme 'zenburn-mod t)
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
; remove menu
(menu-bar-mode -1)
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
; disable fringes
(fringe-mode 0)
; set default line length (as used by 'fill-paragraph) to be 80 characters
(setq-default fill-column 80)
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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "xos4" :family "Terminus")))))
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

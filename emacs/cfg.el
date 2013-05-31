; add load path for custom scripts
(add-to-list 'load-path "~/.emacs.d/script")

; load Packages
; -------------
; fix "<dead-grave> is undefined" error
(require 'iso-transl)
(require 'cl)
(require 'evil)
(evil-mode 1)
(require 'org-exp-blocks)
(require 'yaml-mode)

; read uim.el
(autoload 'uim-mode "uim" nil t)
; key-binding for activate uim (ex. C-\)
(global-set-key "\C-\\" 'my-uim-mode)
; Set Hiragana input mode at activating uim.
(setq uim-default-im-prop '("action_anthy_utf8_hiragana"))
; display candidates inline (near where the actual text is) instead of below the
; modeline
(setq uim-candidate-display-inline t)

;한글입니다.
(set-fontset-font "fontset-default"
	'korean-ksc5601
	'("Baekmuk Gulim" . "unicode-bmp")
)
;日本です。
(set-fontset-font "fontset-default"
	'japanese-jisx0208
	'("IPAGothic" . "unicode-bmp")
)

; Custom functions
(defun my-addrem-comment-region (b e f)
	"Use the `nox' command to comment the current region."
	(interactive)
	(shell-command-on-region
		; beginning and end of buffer
		b e
		; command and parameters
		(concat
			(if f
				"~/prog/nox/src/nox -l "
				"~/prog/nox/src/nox -u -l ")
			(case (with-current-buffer (current-buffer) major-mode)
				('c-mode "c")
				('emacs-lisp-mode "emacslisp")
				('haskell-mode "haskell")
				('LilyPond-mode "tex")
				('plain-tex-mode "tex")
				(t "shell") ; default to shell syntax
			)
		)
		; output buffer
		(current-buffer)
		; replace?
		t
		; name of the error buffer
		"*nox Error Buffer*"
		; show error buffer?
		t
	)
)

(defun my-addrem-comment (f)
	(if (use-region-p)
		(progn
			(my-addrem-comment-region (region-beginning) (region-end) f)
			(evil-visual-char)
			(evil-exit-visual-state)
		)
		(my-addrem-comment-region (line-beginning-position) (line-beginning-position 2) f)
	)
)

; http://www.emacswiki.org/emacs/GlobalTextScaleMode
(defvar text-scale-mode-amount)
(define-globalized-minor-mode
	global-text-scale-mode
	text-scale-mode
	(lambda () (text-scale-mode 1))
)
(defun global-text-scale-adjust (inc) (interactive)
	(text-scale-set 1)
	(kill-local-variable 'text-scale-mode-amount)
	(setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
	(global-text-scale-mode 1)
)

(setq my-current-font 0)
(defun my-toggle-font ()
	"Toggle font between Terminus and Liberation Mono"
	(interactive)
	(setq my-current-font (if (= my-current-font 0) 1 0))
	(set-face-attribute 'default nil :font (if (= my-current-font 1) "Liberation Mono" "Terminus"))
	(redraw-display)
)
(defun kill-this-buffer-volatile ()
	"Kill current buffer unconditionally."
	(interactive)
	(set-buffer-modified-p nil)
	(kill-this-buffer)
)
; Either close the current elscreen, or if only one screen, use the ":q" Evil
; command; this simulates the ":q" behavior of Vim when used with tabs.
(defun vimlike-quit ()
	"Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab (elscreen)."
	(interactive)
	(let
		((one-elscreen (elscreen-one-screen-p)) (one-window (one-window-p)))
		(cond
			; if current tab has split windows in it, close the current live window
			((not one-window) (delete-window) (balance-windows) nil)
			; if there are multiple elscreens (tabs), close the current elscreen
			((not one-elscreen) (elscreen-kill) nil)
			; if there is only one elscreen, just try to quit (calling elscreen-kill
			; will not work, because elscreen-kill fails if there is only one
			; elscreen)
			(one-elscreen (evil-quit) nil)
		)
	)
)
; A function that behaves like Vim's ':tabe' commnad for creating a new tab and
; buffer (the name "[No Name]" is also taken from Vim).
(defun vimlike-:tabe ()
	"Vimlike ':tabe' behavior for creating a new tab and buffer."
	(interactive)
	; create new tab
	(elscreen-create)
)
; start *scratch* buffer without the annoying ad
(setq initial-scratch-message "")
; set *scratch* mode to fundamental mode (the least specialized mode)
(setq initial-major-mode 'text-mode)
; disable all version control minor modes
(setq vc-handled-backends ())

; General indentation behavior
; pressing TAB inserts a TAB
(define-key text-mode-map (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "TAB") 'self-insert-command)

; Modes

; Evil, the Extensible VI Layer! This makes Emacs worth using.
; see http://gitorious.org/evil/pages/Home
(define-key evil-normal-state-map [f1] 'menu-bar-mode) ; toggle the menu bar
(define-key evil-normal-state-map ",w" 'save-buffer) ; save
(define-key evil-normal-state-map ",W" ":w!") ; force save
(define-key evil-normal-state-map ",q" 'vimlike-quit) ; close current elscreen, or current window if only one elscreen
(define-key evil-normal-state-map ",Q" ":q!") ; close current window, *even if modified*
(define-key evil-normal-state-map ",d" 'kill-this-buffer) ; kill current buffer without confirmation
(define-key evil-normal-state-map ",D" 'kill-this-buffer-volatile) ; kill current buffer without confirmation, *even if modified*
(define-key evil-normal-state-map ",x" 'save-buffers-kill-emacs) ; save and quit
(define-key evil-normal-state-map ",u" 'undo-tree-visualize) ; see undo history in tree format (this will be opened in a new split window)
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
		(evil-scroll-line-to-center nil)
	)
)
; simulate vim's "nnoremap <backspace> 10kzz"
(define-key evil-normal-state-map (kbd "DEL")
	(lambda ()
		(interactive)
		(previous-line 10)
		(evil-scroll-line-to-center nil)
	)
)
(define-key evil-normal-state-map ",h" (lambda () (interactive) (split-window-vertically) (balance-windows)))
(define-key evil-normal-state-map ",v" (lambda () (interactive) (split-window-horizontally) (balance-windows)))
(define-key evil-normal-state-map (kbd "TAB") 'other-window) ; move to other window
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
		(let ((evt (read-event (format "Insert %c to exit insert state" ?j) nil 0.2))) ; wait 200 milliseconds
			(cond
				((null evt) (message ""))
				((and (integerp evt) (char-equal evt ?j))
					(delete-char -1)
					(set-buffer-modified-p modified)
					(push 'escape unread-command-events)
		 		)
				(t (setq unread-command-events (append unread-command-events (list evt)))
				)
			)
		)
	)
)

(defun my-uim-mode ()
	"Toggle UIM minor mode, and also toggle #'cofi/maybe-exit keybinding as it conflicts with Anthy input."
	(interactive)
	(uim-mode)
	(if uim-mode
		(define-key evil-insert-state-map "k" nil)
		(define-key evil-insert-state-map "k" #'cofi/maybe-exit)
	)
)

(define-key evil-visual-state-map ",c" (lambda () (interactive) (my-addrem-comment t))) ; add comment
(define-key evil-visual-state-map ",C" (lambda () (interactive) (my-addrem-comment nil))) ; remove comment
(define-key evil-normal-state-map ",c" (lambda () (interactive) (my-addrem-comment t)))
(define-key evil-normal-state-map ",C" (lambda () (interactive) (my-addrem-comment nil)))

; Elscreen
(load "elscreen" "ElScreen" t)
; new vimlike "tab", aka "screen"
(define-key evil-normal-state-map ",N" 'elscreen-create)
; tab navigation
(define-key evil-normal-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-normal-state-map (kbd "C-h") 'elscreen-previous)
(define-key evil-insert-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-insert-state-map (kbd "C-h") 'elscreen-previous)

; Org-mode
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
; change item type
(evil-declare-key 'normal org-mode-map (kbd "M-i") 'org-shiftright)
(evil-declare-key 'normal org-mode-map (kbd "M-I") 'org-shiftleft)
; navigate on a per-item basis
(evil-declare-key 'normal org-mode-map (kbd "M-p") 'org-shiftup)
(evil-declare-key 'normal org-mode-map (kbd "M-n") 'org-shiftdown)
; heading-based navigation
(evil-declare-key 'normal org-mode-map (kbd "M-l") 'org-forward-same-level)
(evil-declare-key 'normal org-mode-map (kbd "M-h") 'org-backward-same-level)
(evil-declare-key 'normal org-mode-map (kbd "M-k") 'outline-previous-visible-heading)
(evil-declare-key 'normal org-mode-map (kbd "M-j") 'outline-next-visible-heading)
; move items around, including child nodes
(evil-declare-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-declare-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-declare-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-declare-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)
(add-hook 'org-mode-hook
	'(lambda ()
		; make TAB go to the other window, and map existing TAB
		; functionality to CTRL-TAB
		(define-key org-mode-map [(tab)] nil)
		(define-key org-mode-map [(control tab)] nil)
	)
)
(evil-declare-key 'normal org-mode-map (kbd "TAB") 'other-window)
(evil-declare-key 'normal org-mode-map [(control tab)] 'org-cycle)

(evil-declare-key 'normal org-mode-map (kbd "<f12>") 'org-export-as-html)

; C
(add-hook 'c-mode-hook
	(lambda ()
		(c-set-style "linux")
		(setq default-tab-width 8)
	)
)

; Emacs lisp
(evil-declare-key 'insert emacs-lisp-mode-map (kbd "DEL") 'backward-delete-char)
(evil-declare-key 'normal emacs-lisp-mode-map "o" 'raw-open-below)
(evil-declare-key 'normal emacs-lisp-mode-map "O" 'raw-open-above)
(defun raw-open-below ()
	"Insert a newline below, and indent relative to the current line."
	(interactive)
	(end-of-line)
	(insert "\n")
	(indent-relative)
	(evil-append nil)
)
(defun raw-open-above ()
	"Insert a newline above, and indent relative to the current line."
	(interactive)
	(forward-line -1)
	(end-of-line)
	(insert "\n")
	(indent-relative)
	(evil-append nil)
)

; Haskell
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
		(setq indent-line-function #'indent-relative)
		(setq tab-width 4)
		(setq indent-tabs-mode t)
	)
)

; Lilypond
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

; LUA
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

; Markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\.md$" . markdown-mode) auto-mode-alist))
(evil-declare-key 'normal markdown-mode-map (kbd "<tab>") 'other-window)


; Python
(add-hook 'python-mode-hook
	(lambda ()
		(setq indent-tabs-mode t)
		(setq python-indent 4)
		(setq tab-width 4)
	)
)

; YAML
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
; disable YAML keymaps, as they interfere with Evil (especially the [backspace] keymap)
(setq yaml-mode-map (make-sparse-keymap))


; Appearance
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
; highlight matching parenthese
(show-paren-mode 1)
; highlight the current cursor line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#434443")
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
(setq evil-visual-state-cursor '("#0000ff" box))
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
(define-key evil-normal-state-map ",-"
	(lambda ()
		(interactive)
        (global-text-scale-adjust -1)
	)
)
(define-key evil-normal-state-map ",_"
	(lambda ()
		(interactive)
        (global-text-scale-adjust 1)
	)
)
; default tab width is 4
(setq default-tab-width 4)
; disable fringes
(fringe-mode 0)
; set default line length (as used by 'fill-paragraph) to be 80 characters
(setq-default fill-column 80)
; add newline (silently) at the end of a file, just like Vim
(setq require-final-newline t)
; scroll more like Vim (no jumping around)
(setq
	scroll-margin 3
	scroll-conservatively 100000
	scroll-preserve-screen-position 1)
; auto-generated stuff by emacs itself...
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-buffer-menu t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(scroll-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "xos4" :family "Terminus")))))

; Backups
; put all auto-saves/backups to the temp directory
(setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))

; Misc
; always follow symlink that points to a version-controlled file
(setq vc-follow-symlinks t)

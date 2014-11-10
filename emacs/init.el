; remove splash screen
(setq inhibit-splash-screen t)
; remove scroll bars
(scroll-bar-mode -1)
; remove toolbar
(tool-bar-mode -1)
; remove menu
(menu-bar-mode -1)

; add load path for custom scripts
(add-to-list 'load-path "~/.emacs.d/script")

; load per-project indentation style settings
(load "~/.emacs.d/kakapo-project-settings")

; MELPA
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

; load Packages
; -------------
; fix "<dead-grave> is undefined" error
(require 'iso-transl)
(require 'cl)
(require 'evil)
(evil-mode 1)
(require 'yaml-mode)

; darken inactive windows
(require 'hiwin)
(hiwin-activate)
(set-face-background 'hiwin-face "gray20")

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
				"~/prog/nox/src/nox -s "
				"~/prog/nox/src/nox -u -s ")
			(case (with-current-buffer (current-buffer) major-mode)
				('c-mode "//")
				('emacs-lisp-mode ";")
				('haml-mode "-# --after-lw")
				('haskell-mode "--")
				('literate-haskell-mode "--")
				('LilyPond-mode "%")
				('latex-mode "%")
				('plain-tex-mode "%")
				('sass-mde "//")
				(t "#") ; default to shell syntax
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
		(my-addrem-comment-region
			(line-beginning-position)
			(line-beginning-position 2)
			f
		)
	)
)

; from Hans-Peter Deifel's email on Nov 1 2012, to the implementations-list@lists.ourproject.org mailing list, which makes visual selections from Evil update the X11 primary selection
(defun evil-visual-update-x-selection (&optional buffer)
	"Update the X selection with the current visual region."
	(with-current-buffer (or buffer (current-buffer))
		(when	(and (evil-visual-state-p)
			(fboundp 'x-select-text)
			(or (not (boundp 'ns-initialized))
				(with-no-warnings ns-initialized))
			(not (eq evil-visual-selection 'block)))
			(x-set-selection 'PRIMARY
				(buffer-substring-no-properties
					evil-visual-beginning
					evil-visual-end))
			(setq x-last-selected-text-primary))))

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
	(when (getenv "DISPLAY")
		; Callback for when user cuts
		(defun xsel-cut-function (text &optional push)
			; Insert text to temp-buffer, and "send" content to xsel stdin
			(with-temp-buffer
				(insert text)
				; I prefer using the "clipboard" selection (the one the
				; typically is used by c-c/c-v) before the primary selection
				; (that uses mouse-select/middle-button-click)
				(call-process-region
					(point-min)
					(point-max)
					"xsel"
					nil
					0
					nil
					"--clipboard" "--input"
				)
			)
		)
		; Call back for when user pastes
		(defun xsel-paste-function()
			; Find out what is current selection by xsel. If it is different
			; from the top of the kill-ring (car kill-ring), then return
			; it. Else, nil is returned, so whatever is in the top of the
			; kill-ring will be used.
			(let
				(
					(xsel-output
						(shell-command-to-string "xsel --clipboard --output")
					)
				)
			(unless (string= (car kill-ring) xsel-output) xsel-output))
		)
		; Attach callbacks to hooks
		(setq interprogram-cut-function 'xsel-cut-function)
		(setq interprogram-paste-function 'xsel-paste-function)
		; Idea from
		; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
		; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
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
	(set-face-attribute
		'default
		nil
		:font
		(if (= my-current-font 1) "Liberation Mono" "Terminus")
	)
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
; set *scratch* mode to text mode
(setq initial-major-mode 'text-mode)
; disable all version control minor modes
(setq vc-handled-backends ())

; General indentation behavior
(require 'kakapo-mode)

; Per-project indentation rules, from ~/.emacs.d/kakapo-project-settings.el
(add-hook 'prog-mode-hook 'my-kakapo-indents)
(add-hook 'text-mode-hook 'my-kakapo-indents)
(add-hook 'css-mode-hook 'my-kakapo-indents)

;(setq kakapo-debug nil)
; first, set default mode to text-mode
(setq-default major-mode 'text-mode)
; Use kakapo's "o" and "O" for opening new lines.
(define-key evil-normal-state-map "o"
	(lambda () (interactive) (kakapo-open nil)))
(define-key evil-normal-state-map "O"
	(lambda () (interactive) (kakapo-open t)))
; make ENTER key insert indentation after inserting a newline (noticeable when
; editing C files)
(define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
(define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)
; for all minor modes, make backspace behave like backspace in insert mode
(define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace)

; Modes

; show 80-character limit on long lines for all source code files
(require 'column-enforce-mode)
; disable highlighting long lines if they are comment lines
(add-hook 'prog-mode-hook (lambda ()
	(setq column-enforce-comments nil)
	(column-enforce-mode)
	)
)

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
(define-key evil-insert-state-map [S-insert]
	(lambda () (interactive) (insert (x-selection 'PRIMARY)))) ; paste X primary
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
(define-key evil-normal-state-map ",h"
	(lambda () (interactive) (split-window-vertically) (balance-windows)))
(define-key evil-normal-state-map ",v"
	(lambda () (interactive) (split-window-horizontally) (balance-windows)))
; move to next window
(define-key evil-normal-state-map (kbd "TAB") 'other-window)
; move to previous window
(define-key evil-normal-state-map [backtab]
	(lambda () (interactive) (other-window -1))
)
; Change K from being mapped to interactive man pages to being used as the
; vanilla comma ',' key's functionality (intra-line backwards search repeat for
; any t, T, f, F searches).
(define-key evil-normal-state-map "K" 'evil-repeat-find-char-reverse)
(define-key evil-visual-state-map "K" 'evil-repeat-find-char-reverse)
; buffer movement
(define-key evil-normal-state-map "H" 'evil-next-buffer)
(define-key evil-normal-state-map "L" 'evil-prev-buffer)
; new buffer
(define-key evil-normal-state-map ",n" 'vimlike-:tabe)
; find file
(define-key evil-normal-state-map ",e" 'ido-find-file)
; set line ending to UNIX
(define-key evil-normal-state-map ",E"
	(lambda () (interactive) (set-buffer-file-coding-system 'utf-8-unix t)))
; replace all /r/n with just /n
; make "kj" behave as ESC key, adapted from http://article.gmane.org/gmane.emacs.vim-emulation/980
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
	:repeat change
	(interactive)
	(let ((modified (buffer-modified-p)))
		(insert "k")
		(let (
			; wait 200 milliseconds
			(evt
				(read-event
					(format "Insert %c to exit insert state" ?j)
					nil
					0.2
				)
			)
		)
			(cond
				((null evt) (message ""))
				((and (integerp evt) (char-equal evt ?j))
					(delete-char -1)
					(set-buffer-modified-p modified)
					(push 'escape unread-command-events)
		 		)
				(t
					(setq unread-command-events
						(append unread-command-events (list evt))
					)
				)
			)
		)
	)
)

(defun my-uim-mode ()
	"Toggle UIM minor mode, and also toggle #'cofi/maybe-exit
keybinding as it conflicts with Anthy input."
	(interactive)
	(uim-mode)
	(if uim-mode
		(define-key evil-insert-state-map "k" nil)
		(define-key evil-insert-state-map "k" #'cofi/maybe-exit)
	)
)

(define-key evil-visual-state-map ",c"
	(lambda () (interactive) (my-addrem-comment t))) ; add comment
(define-key evil-visual-state-map ",C"
	(lambda () (interactive) (my-addrem-comment nil))) ; remove comment
(define-key evil-normal-state-map ",c"
  (lambda () (interactive) (my-addrem-comment t)))
(define-key evil-normal-state-map ",C"
  (lambda () (interactive) (my-addrem-comment nil)))

; Elscreen
(load "elscreen" "ElScreen" t)
(elscreen-start)
; new vimlike "tab", aka "screen"
(define-key evil-normal-state-map ",N" 'elscreen-create)
; tab navigation
(define-key evil-normal-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-normal-state-map (kbd "C-h") 'elscreen-previous)
(define-key evil-insert-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-insert-state-map (kbd "C-h") 'elscreen-previous)

; Ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-ignore-files (append ido-ignore-files '("\\.hi$")))

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
(evil-define-key 'normal org-mode-map (kbd "M-o")
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-heading)
		(evil-append nil)
	)
)
(evil-define-key 'normal org-mode-map "O"
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-heading)
		(org-metaright)
		(evil-append nil)
	)
)
(evil-define-key 'normal org-mode-map "o"
	(lambda ()
		(interactive)
		(end-of-line)
		(always-insert-item)
		(evil-append nil)
	)
)

(evil-define-key 'normal org-mode-map "t"
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-todo-heading nil)
		(evil-append nil)
	)
)
(evil-define-key 'normal org-mode-map (kbd "M-t")
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-todo-heading nil)
		(org-metaright)
		(evil-append nil)
	)
)
(evil-define-key 'normal org-mode-map (kbd "C-o") 'org-toggle-heading) ; convert a plain list into a heading
(evil-define-key 'normal org-mode-map "T" 'org-todo) ; mark a TODO item as DONE
(evil-define-key 'normal org-mode-map ";a" 'org-agenda) ; access agenda buffer
(evil-define-key 'normal org-mode-map "-" 'org-cycle-list-bullet) ; change bullet style

; allow us to access org-mode keys directly from Evil's Normal mode
; change item type
(evil-define-key 'normal org-mode-map (kbd "M-i") 'org-shiftright)
(evil-define-key 'normal org-mode-map (kbd "M-I") 'org-shiftleft)
; navigate on a per-item basis
(evil-define-key 'normal org-mode-map (kbd "M-p") 'org-shiftup)
(evil-define-key 'normal org-mode-map (kbd "M-n") 'org-shiftdown)
; heading-based navigation
(evil-define-key 'normal org-mode-map (kbd "M-l")
	'org-forward-heading-same-level)
(evil-define-key 'normal org-mode-map (kbd "M-h")
	'org-backward-heading-same-level)
(evil-define-key 'normal org-mode-map (kbd "M-k")
	'outline-previous-visible-heading)
(evil-define-key 'normal org-mode-map (kbd "M-j") 'outline-next-visible-heading)
; move items around, including child nodes
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)
(add-hook 'org-mode-hook
	'(lambda ()
		; make TAB go to the other window, and map existing TAB
		; functionality to CTRL-TAB
		(define-key org-mode-map [(tab)] nil)
		(define-key org-mode-map [(control tab)] nil)
	)
)
(evil-define-key 'normal org-mode-map (kbd "TAB") 'other-window)
(evil-define-key 'normal org-mode-map [(control tab)] 'org-cycle)

(evil-define-key 'normal org-mode-map (kbd "<f12>") 'org-html-export-to-html)

; C
(add-hook 'c-mode-hook
	(lambda ()
		(c-set-style "linux")
		(modify-syntax-entry ?_ "w") ; add underscore as a word character, like in Vim
	)
)

; C++
(add-hook 'c++-mode-hook
	(lambda ()
		(c-set-style "linux")
		(modify-syntax-entry ?_ "w") ; add underscore as a word character, like in Vim
	)
)

; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
	(lambda ()
		(modify-syntax-entry ?- "w") ; add hyphen as a word character
	)
)

; Haskell
; adopted from http://sequence.complete.org/node/365
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list
	'Info-default-directory-list
	"/usr/share/emacs/site-lisp/haskell-mode/")
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(defun hs-literate-begend ()
	(interactive)
	(end-of-line)
	(insert "\n")
	(delete-blank-lines)
	(insert "\n\\begin{code}\n\n\\end{code}\n")
	(forward-line -2)
	(evil-append nil)
)

(defun hs-literate-endbeg ()
	(interactive)
	(end-of-line)
	(insert "\n")
	(delete-blank-lines)
	(insert "\\end{code}\n\n\n\n\\begin{code}")
	(forward-line -2)
	(evil-append nil)
)

; literate haskell: write end/begin code blocks, and enter insert mode
(evil-define-key 'normal haskell-mode-map ",B" 'hs-literate-begend)
(evil-define-key 'normal haskell-mode-map ",b" 'hs-literate-endbeg)

; Hazelnut
(add-to-list 'auto-mode-alist '("\\.hzl$" . text-mode))

; Ledger
(add-to-list
	'load-path
	(expand-file-name "/usr/share/emacs/site-lisp/ledger-mode"))
(load "ledger-mode")
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(add-hook 'ledger-mode-hook 'evil-goto-line) ; go to the lastest entries at the end

; Lilypond
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

; LUA
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

; Markdown
(autoload
	'markdown-mode
	"markdown-mode"
	"Major mode for editing Markdown files"
	t)
(setq auto-mode-alist (cons '("\.md$" . markdown-mode) auto-mode-alist))
(evil-define-key 'normal markdown-mode-map (kbd "<tab>") 'other-window)

; Ruby
(add-hook 'ruby-mode-hook
	(lambda ()
		(modify-syntax-entry ?_ "w") ; add underscore as a word character
	)
)

; Sass
(autoload 'sass-mode "sass-mode" "Major mode for editing Sass files" t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

; Shell script
(add-hook 'sh-mode-hook
    (lambda ()
		(modify-syntax-entry ?_ "w") ; add underscore as a word character
    )
)

; YAML
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
; disable YAML keymaps, as they interfere with Evil (especially the [backspace] keymap)
(setq yaml-mode-map (make-sparse-keymap))


; Appearance

; zenmonk color theme
(add-to-list 'custom-theme-load-path "/usr/share/emacs/site-lisp/zenmonk")
(if window-system
	(load-theme 'zenmonk t)
	(progn
		(require 'zenburn)
		(zenburn)
	)
)
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
(define-key evil-normal-state-map ",_"
	(lambda ()
		(interactive)
        (global-text-scale-adjust -1)
	)
)
(define-key evil-normal-state-map ",-"
	(lambda ()
		(interactive)
        (global-text-scale-adjust 1)
	)
)
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

; If we're on our laptop, make the text slightly bigger to match my desktop's
; behavior.
(defun my-text-height ()
	(if (member system-name '("k1.localdomain" "k2"))
		95
		90
	)
)
; auto-generated stuff by emacs itself...
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-buffer-menu t)
 '(blink-cursor-mode nil)
 '(column-number-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height ,(my-text-height) :width normal :foundry "xos4" :family "Terminus")))))

; Backups
; put all auto-saves/backups to the temp directory
(setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))

; Misc
; always follow symlink that points to a version-controlled file
(setq vc-follow-symlinks t)

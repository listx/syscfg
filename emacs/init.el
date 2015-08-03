(require 'cl)

(load "~/.emacs.d/vars")
; remove splash screen
(setq inhibit-splash-screen t)
; remove scroll bars
(scroll-bar-mode -1)
; remove toolbar
(tool-bar-mode -1)
; remove menu
(menu-bar-mode -1)

; NixOS: This enables the various emacs scripts that are installed by
; nixos-rebuild. The script below is taken from
; https://gitorious.org/goibhniu/configuration-files.
(when (not (or
	(string= system-name "k1")
	(string= system-name "w1")
	(string-match "^Linuss" system-name)
	))
	(defconst nixos-sys-packages
		'("/run/current-system/sw/share/emacs/site-lisp"))
		(mapcar
			'(lambda(p)
			(add-to-list 'load-path p)
			(cd p)
			(normal-top-level-add-subdirs-to-load-path)
			)
			nixos-sys-packages))

; MELPA
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

; Define packages that we'll be using.
(defvar my/packages
	'(
	ace-jump-mode
	ace-window
	alect-themes
	color-theme ; needed for dbrock's old zenburn
	column-enforce-mode
	coffee-mode
	delight
	dockerfile-mode
	elscreen
	evil
	evil-leader
	haml-mode
	haskell-mode
	helm
	hiwin
	htmlize
	hydra
	kakapo-mode
	markdown-mode
	mmm-mode
	nix-mode
	org
	yaml-mode
	) "Default packages")

; Install packages if they are missing.
(defun my/packages-installed-p ()
	(loop for pkg in my/packages
		when (not (package-installed-p pkg)) do (return nil)
		finally (return t)))

(unless (my/packages-installed-p)
	(message "%s" "Refreshing package database...")
	(package-refresh-contents)
	(dolist (pkg my/packages)
		(when (not (package-installed-p pkg))
		(package-install pkg))))

; http://www.emacswiki.org/emacs/GenericMode
(require 'generic-x)

(define-generic-mode
	'xdefaults-mode
	'("!") ; comments
	'() ; static keywords e.g., ("if" "else" "return")
	'(
		; regexes for matching aginst font-lock
		("^\\w+" . 'font-lock-type-face)
		("[*.:]" . 'font-lock-builtin-face)
	)
	'("\\.Xdefaults$") ; files to activate this mode (FIXME: maybe move it to kakapo instead?)
	nil ; other functions to call
	"A mode for ~/.Xdefaults and ~/.Xresources files" ; docstring for this mode
)

; evil-leader-mode
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(setq evil-leader/in-all-states 1)

(require 'evil)
(evil-mode 1)

; load non-MELPA scripts or themes
(add-to-list 'load-path "~/.emacs.d/script")
(add-to-list 'custom-theme-load-path "~/.emacs.d/script")

; load per-project indentation style settings
(load "~/.emacs.d/kakapo-project-settings")

; load keymaps
(load "~/.emacs.d/maps")

; load publishing settings for org-mode
(load "~/.emacs.d/publish")

; load zenmonk theme settings
(load "~/.emacs.d/script/zenmonk")

; http://stackoverflow.com/a/3217206/437583
(defun save-buffer-always ()
	"Save the buffer even if it is not modified."
	(interactive)
	(set-buffer-modified-p t)
	(save-buffer)
)

; load Packages
; -------------

; ace-window

; Number each window's unique number into the mode line.
(ace-window-display-mode)
; Disable background color-changing when entering ace-window mode.
(setq aw-background nil)
; Prefer home row over the 0-9 candidate characters.
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

; fix "<dead-grave> is undefined" error
(require 'iso-transl)

; Emulate TextMate's "auto-paired characters"
(electric-pair-mode 1)

; darken inactive windows
(require 'hiwin)
(hiwin-activate)
(set-face-background 'hiwin-face
	; set default to alect-light
	(if window-system "#ded6c5" "gray16"))

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
;日本語です。
(set-fontset-font "fontset-default"
	'japanese-jisx0208
	'("IPAGothic" . "unicode-bmp")
)

(setq tramp-default-method "ssh")

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
				"~/prog/nox/dist/build/nox/nox --sline "
				"~/prog/nox/dist/build/nox/nox --uncomment --sline ")
			(case (with-current-buffer (current-buffer) major-mode)
				('c-mode "//")
				('emacs-lisp-mode "\\;")
				('haml-mode "-# --after-lw")
				('haskell-mode "--")
				('haskell-cabal-mode "--")
				('js-mode "//")
				('literate-haskell-mode "--")
				('LilyPond-mode "%")
				('latex-mode "%")
				('plain-tex-mode "%")
				('sass-mode "//")
				(t "\\#") ; default to shell syntax
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

; Vim-like visual selection. Make the visual selection update the X primary
; buffer.
(defun evil-visual-update-x-selection (&optional buffer)
	"Update the X selection with the current visual region."
	(let ((buf (or buffer (current-buffer))))
		(when (buffer-live-p buf)
		(with-current-buffer buf
			(when (and (evil-visual-state-p)
					(fboundp 'x-select-text)
					(or (not (boundp 'ns-initialized))
						(with-no-warnings ns-initialized))
					(not (eq evil-visual-selection 'block)))
;			; Vanilla evil, as of commit
;			; 1def26dc9c4d084e766364eff2d9f3aa51613cf7.

;          (x-select-text (buffer-substring-no-properties
;                          evil-visual-beginning
;                          evil-visual-end))

;           ; from Hans-Peter Deifel's email on Nov 1 2012, to the
;           ; implementations-list@lists.ourproject.org mailing list
;           ; (http://permalink.gmane.org/gmane.emacs.vim-emulation/1715), which
;           ; makes visual selections from Evil update the X11 primary
;           ; selection; this version does not work with the latest Evil
;           ; 1def26dc9c4d084e766364eff2d9f3aa51613cf7 because of a "X selection
;           ; unavailable for this frame" error.

;			(x-set-selection 'PRIMARY
;				(buffer-substring-no-properties
;					evil-visual-beginning
;					evil-visual-end))
;			(setq x-last-selected-text-primary)

;			; We explicitly use 'xsel' to update the primary selection; this
;			; way, we can emulate Vim's behavior in both terminal and GUI emacs.
			(call-process-region
				evil-visual-beginning
				evil-visual-end
				"xsel"
				nil
					0
				nil
				"--primary" "--input"
			)
)))))

; When yanking ('y' key) in terminal Emacs, copy into the X clipboard. As
; suggested from http://permalink.gmane.org/gmane.emacs.vim-emulation/2023, the
; blog post at
; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
; touches on this topic; here is a modified version of that blog post:
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

(defvar my/font-collection '("Terminus" "Liberation Mono"))
(setq my/font-choice 0)
(defun my-toggle-font ()
	"Cycle through font collection."
	(interactive)
	(setq my/font-choice (mod (+ 1 my/font-choice) (length my/font-collection)))
	(set-face-attribute
		'default
		nil
		:font
		(nth my/font-choice my/font-collection)
	)
	(redraw-display)
)
(defun kill-this-buffer-volatile ()
	"Kill current buffer unconditionally."
	(interactive)
	(set-buffer-modified-p nil)
	(kill-this-buffer)
)

; See http://www.emacswiki.org/emacs/DelightedModes.
(delight
	'(
		(column-enforce-mode nil column-enforce-mode)
		(helm-mode nil helm-mode)
		(undo-tree-mode nil undo-tree)
		(visual-line-mode nil simple)
	)
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
(setq my/before-open-line nil)
(setq my/something-inserted nil)
(defun my/check-for-some-insertion ()
	(if (and
			(eq my/something-inserted nil)
			(save-excursion
				(forward-char -1)
				(looking-at "[^[:space:]]")
			)
		)
		(setq my/something-inserted t)
	)
)
(add-hook 'post-self-insert-hook 'my/check-for-some-insertion)

; Modes

; show 80-character limit on long lines for all source code files
(require 'column-enforce-mode)
; disable highlighting long lines if they are comment lines
(add-hook 'prog-mode-hook (lambda ()
	(setq column-enforce-comments nil)
	(column-enforce-mode)
	)
)

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

; Elscreen
(load "elscreen" "ElScreen" t)
(elscreen-start)

; Helm
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-ff-skip-boring-files t)
; Disable help string. See http://stackoverflow.com/a/19954900/437583.
(defadvice helm-display-mode-line (after undisplay-header activate)
	(setq header-line-format nil))

; Fuzzy matching for "M-x". We have to add the binding as well, because without
; it we get vanilla M-x (which, although it is 'helmified', does not use the
; fuzzy matching behavior).
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-x") 'helm-M-x)

(helm-mode 1)

; Org-mode
; start up org-mode for .org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
; write timestamp when a TODO changes to DONE
(setq org-log-done t)
(setq org-agenda-files (list "~/org"))

; Allow single/double quote marks in inline '='-delimited verbatim formatting.
; see https://lists.gnu.org/archive/html/emacs-orgmode/2014-04/msg00199.html
(setq org-emphasis-regexp-components
	'(" \t('\"{" "- \t.,:!?;'\")}\\" " \t\r\n" "." 1))


(defun always-insert-item ()
	(interactive)
	(if (not (org-in-item-p))
	(insert "\n- ")
	(org-insert-item))
)

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
; conf-mode
(add-hook 'conf-mode-hook
	(lambda ()
		(setq indent-tabs-mode nil)
	)
)

; Emblem --- use SLIM for now
(add-to-list 'auto-mode-alist '("\\.emblem$" . slim-mode))

; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
	(lambda ()
		(modify-syntax-entry ?- "w") ; add hyphen as a word character
	)
)

; Haskell
; adopted from http://sequence.complete.org/node/365
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)
; Remove the hard-coded 'literate-haskell-mode' activation for `.lhs' files that
; haskell-mode comes with. In exchange, enable LaTeX mode whenever we open up a
; `.lhs' file. Using mmm-mode, we will activate `haskell-mode' in the code
; sections.
(setq auto-mode-alist (remove (rassoc 'literate-haskell-mode auto-mode-alist) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.lhs$" . latex-mode))

; The "Haskell-Cabal" mode that comes built-in with haskell-mode needs some
; manual tooth-removal to get it to submit and behave.
(evil-define-key 'insert haskell-cabal-mode-map (kbd "<tab>") 'kakapo-tab)
(evil-define-key 'insert haskell-cabal-mode-map (kbd "DEL") 'kakapo-backspace)
(add-hook 'haskell-cabal-mode-hook
	(lambda ()
		(kakapo-mode)
		(setq indent-tabs-mode nil)
		(setq tab-width 2)
		(setq evil-shift-width 2)
	)
)

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
	(insert "\\end{code}\n\n\n\n\\begin{code}\n")
	(forward-line -3)
	(evil-append nil)
)

; literate haskell: write end/begin code blocks, and enter insert mode
(evil-define-key 'normal haskell-mode-map ",B" 'hs-literate-begend)
(evil-define-key 'normal haskell-mode-map ",b" 'hs-literate-endbeg)

; Literate Haskell - mmm-mode. Adopted from
; https://wiki.haskell.org/Literate_programming#Multi-mode_support_in_Emacs
(require 'mmm-auto)

(mmm-add-classes
	'((literate-haskell-latex
		:submode haskell-mode
		:front "^\\\\begin{code}\n"
		:back "^\\\\end{code}"
	)))

; Haml
(evil-define-key 'insert haml-mode-map (kbd "<tab>") 'kakapo-tab)
(evil-define-key 'insert haml-mode-map (kbd "<backspace>") 'kakapo-backspace)
(add-hook 'haml-mode-hook
	(lambda ()
		(kakapo-mode)
		(setq indent-tabs-mode nil)
		(setq tab-width 2)
		(setq evil-shift-width 2)
	)
)

; Hazelnut
(add-to-list 'auto-mode-alist '("\\.hzl$" . text-mode))

; Haml Coffee mode
(add-to-list 'auto-mode-alist '("\\.hamlc$" . haml-mode))

; JavaScript
(add-hook 'js-mode-hook
	(lambda ()
		(modify-syntax-entry ?_ "w")
	)
)

; Ledger
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
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

; Nix Expression Language
(add-hook 'nix-mode-hook
	(lambda ()
		(modify-syntax-entry ?_ "w") ; add underscore as a word character, like in Vim
	)
)

; Python
(add-hook 'python-mode-hook
	(lambda ()
		(modify-syntax-entry ?_ "w")
	)
)

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
(autoload 'yaml-mode "yaml-mode" "A major mode for YAML" t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(evil-define-key 'insert yaml-mode-map (kbd "<tab>") 'kakapo-tab)
(add-hook 'yaml-mode-hook
	(lambda ()
		(kakapo-mode)
		(setq indent-tabs-mode nil)
		(setq tab-width 2)
		(setq evil-shift-width 2)
	)
)

; Appearance
(defvar my/themes
	'(
	alect-light
	alect-dark
	alect-black
	) "Default themes")

(defvar my/theme-idx 0)

(defun my/theme-name ()
	(nth my/theme-idx my/themes)
)

; Install packages if they are missing.
(defun my/cycle-theme ()
		(progn
			; Increment current theme index
			(setq my/theme-idx (mod (+ 1 my/theme-idx) (length my/themes)))

			; Apply the theme
			(load-theme (my/theme-name) t)

			; Set colors depending on theme name.
	(let
		(
			(theme (format "%s" (my/theme-name)))
		)
			(cond
				((string= "alect-light" theme)
					(progn
						(set-face-background 'hiwin-face
							; set default to alect-light
							(if window-system "#ded6c5" "gray16"))
						(setq evil-insert-state-cursor '("#000000" box))
						(setq evil-normal-state-cursor '("DodgerBlue1" box))
					)
				)
				(
					(or
						(string= "alect-dark" theme)
						(string= "alect-black" theme)
					)
					(progn
						(set-face-background 'hiwin-face
							(if window-system
								(if (string= "alect-dark" theme)
									"#14141b"
									"gray0"
								)
								"gray16"
							)
						)
						(setq evil-insert-state-cursor '("#ffffff" box))
						(setq evil-normal-state-cursor '("#00ff00" box))
					)
				)
			)
		)
	)
)

; Select theme based on GUI or ncurses mode.
(if window-system
	(load-theme 'alect-light t)
	(progn
		(require 'zenburn)
		(zenburn)
	)
)

; highlight matching parenthese
(show-paren-mode 1)
; highlight the current cursor line
(global-hl-line-mode 1)
; disable cursor line highlight during insert mode
(add-hook 'evil-insert-state-entry-hook
	(lambda ()
		(interactive)
		(global-hl-line-mode 0)
	)
)
; Aggressively undo any "o" or "O" command if we only enter whitespace.
(add-hook 'evil-insert-state-exit-hook
	(lambda ()
		(interactive)
		(global-hl-line-mode 1)
		; like Vim, remove whitespace if nothing was inserted
		(delete-trailing-whitespace)
		; Every time we enter insert mode with `kakapo-open', we mark the
		; current line with `my/before-open-line'. If all we did was just enter
		; a bunch of whitespace, we try to keep unoing until we reach a point
		; where `point' is on a line that matches `my/before-open-line'. The
		; edge case we have to look out for is if we enter insert mode without
		; `kakapo-open' --- in this case, it is important to check if
		; `my/before-open-line' is set; if it is set, it means that we did use
		; `kakapo-open', so it's safe to undergo the undo chain. Lastly, if all
		; we want to do is enter a blank newline, simply use `kakapo-open' and
		; then manually redo (because there will be nothing to 'undo' because of
		; the while-loop of undos below) after exiting insert state, OR just
		; don't use `kakapo-open' (e.g., ESC -> A -> enter -> ESC).
		(if (and
				(not my/something-inserted)
				my/before-open-line
			)
			(progn
				(while (not (string= (kakapo-lc) my/before-open-line))
					(undo-tree-undo)
				)
				; We match Vim's behavior. In Vim, if you press "o" or "O" and
				; then immediately do ESC, and undo, point is exactly where it
				; was. For Evil, point is actually "consistent" because it is
				; placed one character *before* where point was, much like how
				; repeatedly pressing "i" (for insert mode) and ESC results in
				; the cursor slowly moving backward one character at a time. To
				; counter this, we manually move our point over one character.
				(forward-char)
			)
		)
		(setq my/before-open-line nil)
		(setq my/something-inserted nil)
	)
)
(defun my/paste-X-primary ()
	(interactive)
	(insert xpribuf)
	; Prevent our 'undo usage of 'o' Insert state exit hook from undoing this
	; paste.
	(setq my/before-open-line nil)
)
(defun my/paste-X-primary-smart (backward)
	(interactive)
	(let
		(
			(pos (point))
			(pos-column (current-column))
			(line-max (save-excursion (end-of-line) (point)))
			(xpribuf (x-selection 'PRIMARY))
		)
		; If what we want to paste has a newline in it, then we should paste it
		; starting at the beginning of a line, not at point (which could be in
		; the middle of a line on some text).
		(if (string-match "\n" xpribuf)
			(if backward
				(progn
					(beginning-of-line)
					(my/paste-X-primary)
					(move-to-column pos-column)
				)
				(progn
					(if (= line-max (point-max))
						(progn
							(end-of-line)
							(insert "\n")
						)
						(forward-line 1)
					)
					(beginning-of-line)
					(my/paste-X-primary)
					(goto-char pos)
				)
			)
			(my/paste-X-primary)
		)
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
; cursor colors for the various states; default to alect-light for normal-state cursor
; default cursor colors for alect-light
(setq evil-insert-state-cursor '("#000000" box))
(setq evil-emacs-state-cursor '("#ff0000" box))
(setq evil-normal-state-cursor '("DodgerBlue1" box))
(setq evil-visual-state-cursor '("#0000ff" box))
; visual line mode (word wrap on whole words) by default
(global-visual-line-mode 1)
; make the color in set-cursor-color be the default color recognized by Evil
; (for the non-customized states, such as visual mode)
; (this is required because we change the color *after* enabling evil)
(setq evil-default-cursor t)
; stretch the cursor (e.g., make it bigger if hovering over a tab)
(setq x-stretch-cursor 1)
; show empty whitespace
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
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
	(cond
		(
			(or
				(string-match "^k[12]" system-name)
				(string-match "^w0" system-name)
			)
			95
		)
		((string-match "^k3" system-name)
			102
		)
		(t 91)
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

; Customize how hydra displays keybindings (no brackets/colons).
(setq hydra-head-format "%s ")
; Emacs
(define-key evil-normal-state-map [f1] 'menu-bar-mode) ; toggle the menu bar
; ace-jump-mode
; This robs "f" of its normal function (finding the given character on the
; current line), but as ace-jump is essentially acting as a superset of normal
; "f", this makes the most sense.
(define-key evil-normal-state-map "f" 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "TAB") 'other-window)
(define-key evil-normal-state-map [backtab]
	(lambda ()
		(interactive)
		(other-window -1)
	)
)

; UIM mode
(define-key evil-insert-state-map (kbd "C-.") 'my-uim-mode)

; Interactive search-and-replace "bindings". These are not real bindings in the
; sense that they call functions --- this is a limitation of upstream's design
; of query-replace. See
; http://www.gnu.org/software/emacs/manual/html_node/elisp/Search-and-Replace.html.
(define-key query-replace-map [return] 'act)
(define-key query-replace-map "N" 'backup)
(define-key query-replace-map "a" 'automatic)
(define-key query-replace-map "e" 'edit-replacement)

; Parens in Vim normal mode bind to prev/forward sentence. We generally follow
; the "1-sentence-per-line" rule for long prose documents so these keybindings
; are basically useless. Perfect for jumping!
(define-key evil-normal-state-map "(" 'evil-jump-backward)
(define-key evil-normal-state-map ")" 'evil-jump-forward)

(defhydra hydra-zoom ()
	"zoom"
	("j"
		(lambda () (interactive) (global-text-scale-adjust 1))
		"in")
	("k"
		(lambda () (interactive) (global-text-scale-adjust -1))
		"out")
	("q" nil "exit" :exit t)
)

(defhydra hydra-window ()
	"Window navigation with hydra."
	("h" windmove-left)
	("j" windmove-down)
	("k" windmove-up)
	("l" windmove-right)
	(","
		(lambda ()
			(interactive)
			(ace-window 1)
			(add-hook 'ace-window-end-once-hook 'hydra-window/body))
		"ace"
		:exit t)
	("v"
		(lambda ()
			(interactive)
			(split-window-right)
			(windmove-right))
		"vert"
		:exit t)
	("x"
		(lambda ()
			(interactive)
			(split-window-below)
			(windmove-down))
		"horz"
		:exit t)
	("s"
		(lambda ()
			(interactive)
			(ace-window 4)
			(add-hook 'ace-window-end-once-hook 'hydra-window/body))
		"swap"
		:exit t)
	("d"
		(lambda ()
			(interactive)
			(ace-window 16)
			(add-hook 'ace-window-end-once-hook 'hydra-window/body))
		"del"
		:exit t)
	("i" nil "choose" :exit t)
	("I" delete-other-windows "max" :exit t)
	("o" ace-maximize-window "max-choose" :exit t)
)

; See https://github.com/asok/.emacs.d/.
(defhydra hydra-magit (:color blue)
	"magit"
	("s" magit-status "status")
	("c" magit-checkout "checkout")
	("b" magit-branch-manager "branch manager")
	("m" magit-merge "merge")
	("l" magit-log "log")
	("!" magit-git-command "command")
	("$" magit-process "process")
)

(evil-leader/set-key
	"-" 'hydra-zoom/body
	"TAB" 'hydra-window/body
	"<SPC>" 'magit-status
	; set line ending to UNIX
	"\\" (lambda () (interactive) (set-buffer-file-coding-system 'utf-8-unix t))

	"a" 'projectile-helm-ag
	"A" 'helm-ag

	"b" 'my/copy-file-name-to-clipboard

	; Nox integration (comment/uncomment regions)
	"c" (lambda () (interactive) (my-addrem-comment t))
	"C" (lambda () (interactive) (my-addrem-comment nil))

	; kill buffer
	"d" 'kill-this-buffer

	; kill current buffer without confirmation, *even if modified*
	"D" 'kill-this-buffer-volatile

	; find files (like dired, but better)
	"e" 'helm-find-files
	; buffers list
	"E" 'helm-mini

	"f" 'helm-projectile
	"F" 'helm-projectile-switch-project

	"g" 'helm-all-mark-rings

	"h" (lambda () (interactive) (split-window-vertically) (balance-windows))

	; literate haskell: write end/begin code blocks, and enter insert mode
	"j" 'hs-literate-endbeg
	"J" 'hs-literate-begend

	"m" 'hydra-magit/body

	; new tab
	"n" 'elscreen-create

	; new tab, but clone the current tab's window-splits (if any) layout
	"N" 'elscreen-clone

	; close current elscreen, or current window if only one elscreen
	"q" 'vimlike-quit

	; close current window, *even if modified*
	"Q" (lambda () (interactive) (evil-quit t))

	; Place selected region (or word-area under point in Normal mode) into
	; buffer-wise search-and-replace interactive prompt.
	"s" 'my/replace-in-buffer

	"t" 'my-toggle-font

	; cycle through various themes
	"T" (lambda () (interactive) (my/cycle-theme))

	; see undo history in tree format (this will be opened in a new split
	; window)
	"u" 'undo-tree-visualize

	"v" (lambda () (interactive) (split-window-horizontally) (balance-windows))

	; save
	"w" 'save-buffer

	; force save
	"W" 'save-buffer-always

	; save and quit
	"x" 'save-buffers-kill-emacs

	"y" (lambda () (interactive) (my/copy-for-slack nil))
	"Y" (lambda () (interactive) (my/copy-for-slack t))

	; put emacs into the background; only works in terminal mode
	"Z" 'suspend-emacs
	)

; We need to use 'eval-after-load' because otherwise we get an error about
; `helm-map' not existing yet.
(with-eval-after-load "helm-mode"
	(loop for ext in '("\\.elc$")
		do (add-to-list 'helm-boring-file-regexp-list ext))

	; From http://emacs.stackexchange.com/a/7896. Slightly modified as usual.

	; `helm-maybe-exit-minibuffer' opens the argument and exits helm.
	; `helm-execute-persistent-action' pseudo-opens the argument, and stays in
	; helm. We want to use the second one and stay in helm as much as possible
	; (e.g., esp. when navigating directories), because then we basically get
	; most of the power of Dired-mode but combined with helm's fuzzy-matching.
	; We could technically just call `fu/helm-find-files-navigate-forward'
	; directly instead of using `advice-add', but it's worth keeping for
	; historical reasons.
	(defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
		(cond
			(
				(and
					(eq 'string (type-of (helm-get-selection)))
					(not (string-match "\/\\.$" (helm-get-selection)))
					(file-directory-p (helm-get-selection))
				)
				(apply orig-fun args)
			)
			(t (helm-maybe-exit-minibuffer))
		)
	)
	(advice-add 'helm-execute-persistent-action
		:around #'fu/helm-find-files-navigate-forward)

	(defun fu/helm-find-files-navigate-back (orig-fun &rest args)
	(if (= (length helm-pattern) (length (helm-find-files-initial-input)))
		(helm-find-files-up-one-level 1)
		(apply orig-fun args)))
	(advice-add 'helm-ff-delete-char-backward
		:around #'fu/helm-find-files-navigate-back)

	(define-key helm-map " " 'hydra-helm/body)
	; As of 576cc21f381977e1d3c509d94f73853a74612cff, the
	; `helm-find-files-doc-header' hardcodes the default `C-l' binding. We set
	; it to nil to suppress the message from `helm-find-files'.
	(setq helm-find-files-doc-header nil)
	(define-key helm-find-files-map (kbd "RET") 'helm-execute-persistent-action)
)

; From http://angelic-sedition.github.io/blog/2015/02/03/a-more-evil-helm/.
(defhydra hydra-helm (:foreign-keys warn)
	("h" (helm-find-files-up-one-level 1) "parent")
	("j" helm-next-line "down")
	("k" helm-previous-line "up")
	("l" helm-execute-persistent-action "open")
	("H" helm-previous-source "next source")
	("L" helm-next-source "prev source")
	("RET" helm-maybe-exit-minibuffer "open")
	("<SPC>" nil "exit hyrda")
	("TAB" helm-select-action "action")
	("m" helm-toggle-visible-mark)
	("u" helm-unmark-all)
	("g" helm-beginning-of-buffer "top")
	("G" helm-end-of-buffer "bottom")
	("J" helm-scroll-other-window)
	("K" helm-scroll-other-window-down)
	("?" helm-help "help")
	("q" keyboard-escape-quit "exit")
)

; paste X primary
(define-key evil-normal-state-map (kbd "C-p")
	(lambda ()
		(interactive)
		(evil-append nil)
		(my/paste-X-primary-smart nil)
		(evil-normal-state)
		(message nil)
	)
)
(define-key evil-normal-state-map (kbd "C-S-p")
	(lambda ()
		(interactive)
		(evil-insert nil)
		(my/paste-X-primary-smart t)
		(evil-normal-state)
		(message nil)
	)
)
(define-key evil-insert-state-map [S-insert]
	(lambda ()
		(interactive)
		(my/paste-X-primary-smart nil)
	)
)
; For Mac OSX laptops without the insert key.
(define-key evil-insert-state-map (kbd "C-p")
	(lambda ()
		(interactive)
		(my/paste-X-primary-smart nil)
	)
)

; insert hard line breaks
(define-key evil-normal-state-map "gw" 'fill-paragraph)

; Add newlines above/below, without going through kakapo-open. We have a hook
; that runs on exit of insert mode, which discards purely whitespace insertions,
; so this way we can insert whitespace (newlines) without worrying about the
; hook.
(defun my-insert-newline-below ()
	(interactive)
	(forward-line 1)
	(beginning-of-line)
	(insert "\n")
	(forward-line -1)
)
(defun my-insert-newline-above ()
	(interactive)
	(beginning-of-line)
	(insert "\n")
	(forward-line -1)
)
(define-key evil-insert-state-map (kbd "C-o") 'my-insert-newline-below)
(define-key evil-normal-state-map (kbd "C-o") 'my-insert-newline-below)
(define-key evil-insert-state-map (kbd "C-S-o") 'my-insert-newline-above)
(define-key evil-normal-state-map (kbd "C-S-o") 'my-insert-newline-above)

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
; Change K from being mapped to interactive man pages to being used as the
; vanilla comma ',' key's functionality (intra-line backwards search repeat for
; any t, T, f, F searches).
(define-key evil-normal-state-map "K" 'evil-repeat-find-char-reverse)
(define-key evil-visual-state-map "K" 'evil-repeat-find-char-reverse)
; buffer movement
(define-key evil-normal-state-map "H" 'evil-next-buffer)
(define-key evil-normal-state-map "L" 'evil-prev-buffer)
; make "kj" behave as ESC key, adapted from
; http://article.gmane.org/gmane.emacs.vim-emulation/980
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)

; Elscreen
; tab navigation
(define-key evil-normal-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-normal-state-map (kbd "C-h") 'elscreen-previous)
(define-key evil-insert-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-insert-state-map (kbd "C-h") 'elscreen-previous)

; org-mode
(evil-define-key 'normal org-mode-map (kbd "M-o")
	(lambda ()
		(interactive)
		(end-of-line)
		(insert "\n")
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
(evil-define-key 'normal org-mode-map "T" 'org-todo) ; mark a TODO item as DONE
(defhydra hydra-org (:foreign-keys warn)
	"org"
	("b" org-cycle-list-bullet "cycle-bullet-type")
	("l" mmm-parse-buffer "turn on mmm-mode")
	("m" mmm-mode "turn on mmm-mode")
	("s" org-beamer-export-to-pdf "save beamer to pdf")
	("w" org-publish-current-project "publish current project")
	("q" nil "exit" :exit t)
)
(evil-leader/set-key-for-mode 'org-mode
	"1" 'hydra-org/body
)

(evil-define-key 'normal org-mode-map (kbd "M-i") 'org-insert-link)
(evil-define-key 'insert org-mode-map (kbd "M-i") 'org-insert-link)
(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-open-at-point)
(evil-define-key 'normal org-mode-map (kbd "M-n") 'org-shiftright)
; heading-based navigation
(evil-define-key 'normal org-mode-map (kbd "M-k")
	'outline-previous-visible-heading)
(evil-define-key 'normal org-mode-map (kbd "M-j")
	'outline-next-visible-heading)
; move items around, including child nodes
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)

(evil-define-key 'normal org-mode-map (kbd "<f12>") 'org-html-export-to-html)

; Fold/expand all headings globally one level (use C-TAB) for expanding/folding
; a particular level.
; The 'iso-lefttab' just means 'tab'. It probably has something to do with the
; keyboard layout also.
(evil-define-key 'normal org-mode-map (kbd "<C-S-iso-lefttab>") 'org-shifttab)


(evil-define-key 'normal org-mode-map (kbd "M-p")
	(lambda ()
		(interactive)
		(evil-append nil)
		(forward-line 1)
		(beginning-of-line)
		(insert "#+begin_src\n")
		(my/paste-X-primary)
		(if (not (string= "\n" (string (char-before (point)))))
			(insert "\n")
		)
		(insert "#+end_src\n")
		(evil-normal-state)
		(forward-line -1)
		(message nil)
	)
)

; Disable default orgmode hotkeys that interfere with our global hotkeys defined
; elsewhere.
(add-hook 'org-mode-hook
	'(lambda ()
	(define-key org-mode-map [(tab)] nil)
	(define-key org-mode-map (kbd "<S-iso-lefttab>") nil)
	)
)

; Dired mode.
(evil-define-key 'normal dired-mode-map "H" 'evil-next-buffer)
(evil-define-key 'normal dired-mode-map "L" 'evil-prev-buffer)
(evil-define-key 'normal dired-mode-map "K" 'dired-up-directory)

; Kakapo
(define-key evil-normal-state-map "o"
	(lambda ()
		(interactive)
		(setq my/before-open-line (kakapo-lc))
		(kakapo-open nil)
	)
)
(define-key evil-normal-state-map "O"
	(lambda ()
		(interactive)
		(setq my/before-open-line (kakapo-lc))
		(kakapo-open t)
	)
)
; make ENTER key insert indentation after inserting a newline (noticeable when
; editing C files)
(define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
(define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)
; for all minor modes, make backspace behave like backspace in insert mode
(define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace)

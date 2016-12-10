(use-package general
	:config
	(setq general-default-keymaps 'evil-normal-state-map)
	(setq general-default-prefix ",")

	; Global bindings.
	(general-define-key
		"-" 'hydra-zoom/body
		"<tab>" 'hydra-window/body
		"<SPC>" 'magit-status
		; Set line ending to UNIX.
		"\\" 'l/force-unix-line-endings

		"a" 'l/projectile-helm-ag
		"A" 'helm-ag

		"b" 'helm-buffers-list
		"B" 'l/copy-file-name-to-clipboard

		; Nox integration (comment/uncomment regions).
		"c" (lambda () (interactive) (l/addrem-comment t))
		"C" (lambda () (interactive) (l/addrem-comment nil))

		; Kill buffer.
		"d" 'kill-this-buffer
		; Kill current buffer without confirmation, even if modified.
		"D" 'l/kill-this-buffer!
		; Find files (like dired, but better).
		"e" 'helm-find-files
		; buffers list
		"E" 'helm-mini

		"f" 'helm-projectile
		"F" 'helm-projectile-switch-project

		"gj" 'git-gutter:revert-hunk
		"gn" 'git-gutter:next-hunk
		"gN" 'git-gutter:previous-hunk
		"gs" 'git-gutter:popup-hunk

		"G" 'helm-all-mark-rings
		"h" 'l/split-vertically

		; Literate haskell: write end/begin code blocks, and enter insert mode.
		; TODO: make this just 1 funcion that does the right thing.
		"j" 'l/hs-literate-endbeg
		"J" 'l/hs-literate-begend

		"kj" 'hlint-refactor-refactor-at-point
		"kJ" 'hlint-refactor-refactor-buffer
		"kk" 'flycheck-list-errors
		"kn" 'flycheck-next-error
		"kN" 'flycheck-previous-error

		"m" 'hydra-magit/body
		; New tab.
		"n" 'elscreen-create
		; New tab, but clone the current tab's window-split layout (if any).
		"N" 'elscreen-clone
		"q" 'l/quit-buffer
		; Close current window, _even if modified_.
		"Q" (lambda () (interactive) (evil-quit t))

		; Place selected region (or word-area under point in Normal mode) into
		; buffer-wise search-and-replace interactive prompt.
		"s" 'l/replace-in-buffer
		"t" 'l/toggle-font
		; Cycle through various themes.
		"T" 'l/cycle-theme

		; See undo history in tree format (this will be opened in a new split
		; window).
		"u" 'undo-tree-visualize

		"v" 'l/split-horizontally
		"w" 'save-buffer
		"W" 'l/save-buffer!

		; Save and quit.
		"x" 'save-buffers-kill-emacs

		"y" (lambda () (interactive) (l/copy-for-slack nil))
		"Y" (lambda () (interactive) (l/copy-for-slack t))
		; Evaluate source code block.
		"z" 'org-ctrl-c-ctrl-c
		; Put emacs into the background; only works in terminal mode.
		"Z" 'suspend-emacs
	)
)

(defun l/force-unix-line-endings ()
	(interactive)
	(set-buffer-file-coding-system 'utf-8-unix t)
)

; Adopted from
; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun l/copy-file-name-to-clipboard ()
	"Copy the current buffer file name to the clipboard."
	(interactive)
	(let
		(
			(filename
				(if (equal major-mode 'dired-mode)
					default-directory
					(buffer-file-name)
				)
			)
		)
		(progn
			(kill-new filename)
			(message "Clipboard: '%s'" filename)
		)
	)
)

; Either close the current elscreen, or if only one screen, use the ":q" Evil
; command; this simulates the ":q" behavior of Vim when used with tabs.
(defun l/quit-buffer ()
	"Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab (elscreen)."
	(interactive)
	(let
		((one-elscreen (elscreen-one-screen-p)) (one-window (one-window-p)))
		(cond
			; if current tab has split windows in it, close the current live
			; window
			((not one-window) (delete-window) (balance-windows) nil)
			; if there are multiple elscreens (tabs), close the current elscreen
			((not one-elscreen) (elscreen-kill) nil)
			; if there is only one elscreen, just try to quit (calling
			; elscreen-kill will not work, because elscreen-kill fails if there
			; is only one elscreen)
			(one-elscreen (evil-quit) nil)
		)
	)
)

(defun l/kill-this-buffer! ()
	"Kill current buffer even if it is modified."
	(interactive)
	(set-buffer-modified-p nil)
	(kill-this-buffer)
)

; Window-splitting functions.
(defun l/split-vertically ()
	"Split window verically."
	(interactive)
	(split-window-vertically)
	(balance-windows)
)
(defun l/split-horizontally ()
	"Split window horizontally."
	(interactive)
	(split-window-horizontally)
	(balance-windows)
)

; http://stackoverflow.com/a/3217206/437583
(defun l/save-buffer! ()
	"Save current buffer even if it is not modified."
	(interactive)
	(set-buffer-modified-p t)
	(save-buffer)
)

(provide 'l-general)

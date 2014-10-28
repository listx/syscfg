(defun my-add-hook (hook tmode twidth &optional func)
	(lexical-let
		(
			(tmode tmode)
			(twidth twidth)
			(func func)
		)
		(add-hook hook
			(lambda ()
				(kakapo-mode)
				(setq indent-tabs-mode tmode)
				(setq tab-width twidth)
				(setq evil-shift-width twidth)
				func
			)
		)
	)
)

; Adapted from KimStorm's solution from http://www.emacswiki.org/ProjectSettings
(defun my-kakapo-indents ()
	(let
		(
			(b (buffer-file-name))
		)
		(defun h (hook tmode twidth &optional func)
			(my-add-hook hook tmode twidth func)
		)
		(if b
		(cond
			((string-match "webdev/depot/.+\\.rb$" b)
				(h 'ruby-mode-hook nil 2
					(progn
						(setq ruby-indent-level 2)
						(message
							"ruby-indent-level set to %d"
							ruby-indent-level
						)
					)
				)
			)

			; Default options by language

			; C
			((string-match "\\.[ch]$" b)
				(h 'c-mode-hook t 8
					(progn
						(setq default-tab-width 8)
						(message
							"default-tab-width set to %d"
							default-tab-width
						)
					)
				)
			)
			; C++
			((string-match "\\.[ch]pp$" b)
				(h 'c++-mode-hook t 8
					(progn
						(setq default-tab-width 8)
						(message
							"default-tab-width set to %d"
							default-tab-width
						)
					)
				)
			)
			; Emacs lisp
			((string-match "\\.el$" b)
				(h 'emacs-lisp-mode-hook t 4)
			)
			; Haskell
			((string-match "\\.[l]?hs$" b)
				(h 'haskell-mode-hook t 4)
			)
			; Hazelnut
			((string-match "\\.hzl$" b)
				(h 'text-mode-hook t 4)
			)
			; HTML
			((string-match "\\.htm[l]?$" b)
				(h 'html-mode-hook t 4)
			)
			; Latex
			((string-match "\\.tex$" b)
				(h 'latex-mode-hook t 4)
			)
			; Python
			((string-match "\\.py$" b)
				(h 'python-mode-hook t 4
					(progn
						(setq python-indent 4)
						(message "python-indent set to %d" python-indent)
					)
				)
			)
			; Ruby
			((string-match "\\.rb$" b)
				(h 'ruby-mode-hook t 4
					(progn
						(setq ruby-indent-level 4)
						(message
							"ruby-indent-level set to %d"
							ruby-indent-level
						)
					)
				)
			)
			; Shell
			((string-match "\\.sh$" b)
				(h 'sh-mode-hook t 4)
			)
		)
		)
	)
)

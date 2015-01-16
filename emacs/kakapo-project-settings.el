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
			; TODO: import these project-specific conditions somehow from an
			; external .el file.
			(
				(or
					(string-match "webdev/depot/.+\\.rb$" b)
					(string-match "webdev/fido/.+\\.rb$" b)
					(string-match "webdev/pf/.+\\.rb$" b)
					(string-match "webdev/asc/.+\\.rb$" b)
					(string-match "webdev/ember-crm/.+\\.rb$" b)
					(string-match "prog/foreign/rails/.+\\.rb$" b)
					(string-match "prog/dyla/.+\\.rb$" b)
				)
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
			; CSS
			((string-match "\\.css$" b)
				(progn
					(kakapo-mode)
					(setq indent-tabs-mode nil)
					(setq tab-width 2)
					(setq evil-shift-width 2)
					(setq css-indent-offset 2)
					(message
						"css-indent-offset set to %d"
						2
					)
				)
			)
			; Emacs lisp
			((string-match "\\.el$" b)
				(h 'emacs-lisp-mode-hook t 4)
			)
			; Haml
			((string-match "\\.haml$" b)
				(progn
					(h 'haml-mode-hook nil 2)
					; Haml mode intrusively redefines the backspace key; let's
					; get rid of that!
					(define-key haml-mode-map [backspace] nil)
				)
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
			; HTML + ERB
			((string-match "\\.html\\.erb?$" b)
				(h 'html-mode-hook nil 2)
			)
			; Latex
			((string-match "\\.tex$" b)
				(h 'latex-mode-hook t 4)
			)
			; Markdown
			((string-match "\\.md$" b)
				(progn
					(h 'markdown-mode-hook t 4)
					(define-key markdown-mode-map [backspace] nil)
					(define-key markdown-mode-map [tab] nil)
				)
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
			; Sass
			((string-match "\\.sass$" b)
				(progn
					(h 'sass-mode-hook nil 2)
					; Sass mode intrusively redefines the backspace key; let's
					; get rid of that!
					(define-key sass-mode-map [backspace] nil)
				)
			)
			; Shell
			((string-match "\\.sh$" b)
				(h 'sh-mode-hook t 4)
			)
			; Slim
			((string-match "\\.slim$" b)
				(h 'slim-mode-hook nil 2)
			)

			; Default
			(t
				(progn
					(kakapo-mode)
					(setq indent-tabs-mode t)
					(setq tab-width 4)
					(setq evil-shift-width 4)
				)
			)
		)
		)
	)
)

(defmacro with-zenmonk-color-vars (&rest body)
	"`let'-bind all colors defined in `zenmonk-colors-alist' within BODY."
	`(let
		 (
			,@(mapcar (lambda (cons)
				(list (intern (car cons)) (cdr cons)))
				zenmonk-colors-alist)
		)
		,@body
	)
)

(defvar zenmonk-colors-alist
	'(
		("zm-fg+1"      . "#ffffef")
		("zm-fg"        . "#cccccf")
		("zm-fg-1"      . "#656555")

		("zl-fg"        . "#000000")

		("zm-bg+3"      . "#6f6f6f")
		("zm-bg+2"      . "#5f5f5f")
		("zm-bg+1"      . "#4f4f4f")
		("zm-bg+05"     . "#494949")
		("zm-bg"        . "#22222a")
		("zm-bg-05"     . "#383838")
		("zm-bg-1"      . "#2b2b2b")
		("zm-bg-2"      . "#000000")

		("zm-red+2"     . "#f18c96")
		("zm-red+1"     . "#dca3a3")
		("zm-red"       . "#cc9393")
		("zm-red-1"     . "#bc8383")
		("zm-red-2"     . "#ac7373")
		("zm-red-3"     . "#9c6363")
		("zm-red-4"     . "#8c5353")

		("zl-red"       . "firebrick2")

		("zm-orange+1"  . "#ffcfaf")
		("zm-orange"    . "#dfaf8f")

		("zm-yellow+1"  . "#dfdfbf")
		("zm-yellow"    . "#f0dfaf")
		("zm-yellow-1"  . "#e0cf9f")
		("zm-yellow-2"  . "#d0bf8f")

		("zm-green+4"   . "#bfebbf")
		("zm-green+3"   . "#afd8af")
		("zm-green+2"   . "#9fc59f")
		("zm-green+1"   . "#8fb28f")
		("zm-green"     . "#7f9f7f")
		("zm-green-1"   . "#5f7f5f")
		("zm-green-2"   . "#1e2320")

		("zm-cyan"      . "#93e0e3")

		("zm-blue+1"    . "#94bff3")
		("zm-blue"      . "#8cd0d3")
		("zm-blue-1"    . "#7cb8bb")
		("zm-blue-2"    . "#6ca0a3")
		("zm-blue-3"    . "#5c888b")
		("zm-blue-4"    . "#4c7073")
		("zm-blue-5"    . "#366060")

		("zm-magenta"   . "#dc8cc3")
	)
	"List of Zenmonk colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker."
)

; Highlight decimal, float, hex, and octal numbers.
(make-face 'font-lock-hex-face)
(setq font-lock-hex-face 'font-lock-hex-face)
(make-face 'font-lock-float-face)
(setq font-lock-float-face 'font-lock-float-face)
(make-face 'font-lock-decimal-face)
(setq font-lock-decimal-face 'font-lock-decimal-face)
(make-face 'font-lock-octal-face)
(setq font-lock-octal-face 'font-lock-octal-face)
(add-hook 'prog-mode-hook
	'(lambda ()
		(font-lock-add-keywords nil
			'(
				; Hex number (will highlight invalid suffix though).
				("\\b0x[[:xdigit:]]+[uUlL]*\\b" . font-lock-hex-face)
				; Floating point number.
				(
					"\\(\\b[0-9]+\\)\\(\\.\\)\\{1\\}\\([0-9]+\\(e[-]?\d+\\)?\\([lL]?\\|[dD]?[fF]?\\)\\)\\b"
					(1 font-lock-float-face)
					(2 font-lock-decimal-face)
					(3 font-lock-float-face))
				; Decimal number. Must be before octal regexes otherwise
				; 0 and 0l will be highlighted as errors. Will highlight
				; invalid suffix though.
				("\\b\\(\\(0\\|[1-9][0-9]*\\)[uUlL]*\\)\\b"
					(1 font-lock-decimal-face))
				; Octal number.
				("\\b0[0-7]+[uUlL]*\\b" . font-lock-octal-face)
			)
		)
	)
)

(with-zenmonk-color-vars
	(setq alect-overriding-faces
		`(
			(ace-jump-face-foreground
				((t
				:foreground "red"
				:background bg-1
				:underline t
				)))
			; column-enforce-mode
			(column-enforce-face ((t :underline t)))
			(default ((t :foreground magenta-1 :background bg-1 :weight bold)))
			; FIXME: make the alect-dark palette more zenmonk-like, and then use
			; those regular alect colors as much as possible.
			(font-lock-builtin-face  ((t :foreground magenta-1 :weight bold)))
			(font-lock-comment-delimiter-face ((t :foreground ,zm-green-1)))
			(font-lock-constant-face ((t :foreground cyan-1 :weight bold)))
			(font-lock-doc-face ((t :foreground fg-1)))
			(font-lock-function-name-face ((t :foreground blue-1 :weight bold)))
			(font-lock-negation-char-face
				((((background light))
				:foreground red+1
				:weight bold
				(((background dark))
				:foreground ,zm-yellow
				:weight bold
				))))
			(font-lock-operator-face ((t :foreground ,zm-blue-1 :weight bold)))
			(font-lock-preprocessor-face ((t :foreground green-1 :weight bold)))
			(font-lock-pseudo-keyword-face ((t :foreground ,zm-orange :weight bold)))
			(font-lock-regexp-grouping-construct ((t :foreground ,zm-yellow :weight bold)))
			(font-lock-regexp-grouping-backslash ((t :foreground ,zm-green :weight bold)))
			(font-lock-type-face ((t :foreground magenta+1 :weight bold)))
			(font-lock-warning-face ((t :foreground ,zm-red+1 :weight bold)))

			(font-lock-decimal-face ((t :foreground cyan-2)))
			(font-lock-hex-face ((t :foreground cyan-2 :weight bold)))
			(font-lock-float-face ((t :foreground magenta-2)))
			(font-lock-octal-face ((t :foreground magenta-2 :weight bold)))
			(isearch
				((t
				:inherit lazy-highlight
				)))
			(lazy-highlight
				((t
				:foreground "blue"
				:background "yellow"
				:underline t
				)))
			(mode-line              ((((background light))
										:foreground fg :background bg
										:box (:line-width 2 :style released-button))
										(((background dark))
										:foreground ,zm-green+1 :background ,zm-green-2
										:box (:line-width 2 :color bg-2 :style released-button))))
			(mode-line-inactive     ((((background light))
										:foreground fg+1 :background "#ded6c5"
										:box (:line-width 2 :color bg-2 :style nil))
										(((background dark))
										:foreground "grey32" :background "#0d0d0f"
										:box (:line-width 2 :color "gray32" :style nil))))
			(mode-line-buffer-id    ((t :foreground magenta+1 :weight bold)))
			(mmm-default-submode-face
				((((background light))
				:background "#fbf9f3")
				(((background dark))
				:background "#1d1d22")
				))
			(org-done
					((((background light))
					:foreground fg :weight bold)
					(((background dark))
					:foreground fg :weight bold)))
			(org-special-keyword
					((((background light))
					:foreground fg :underline t)
					(((background dark))
					:foreground fg :underline t)))
			(org-todo
					((((background light))
					:foreground "#ff0000" :weight bold)
					(((background dark))
					:foreground "#ff0000" :weight bold)))
			(org-level-1
					((((background light))
					:foreground "#ff0000" :weight bold)
					(((background dark))
					:foreground "#ff0000" :weight bold)))
			(org-level-2
					((((background light))
					:foreground "#0000ff" :weight bold)
					(((background dark))
					:foreground ,zm-blue+1 :weight bold)))
			(org-level-3
					((((background light))
					:foreground green+1 :weight bold)
					(((background dark))
					:foreground green+1 :weight bold)))
			(org-level-4
					((((background light))
					:foreground magenta+1 :weight bold)
					(((background dark))
					:foreground magenta+1 :weight bold)))
			(region ((((background light))
					:foreground "#ffffcc" :background "#ff66cc" :weight bold)
					(((background dark))
					:foreground ,zm-blue-5 :background ,zm-green+4 :weight bold)))
		)
	)

	; The modifications to the 'light' palette are for slightly altering the default
	; alect-light theme. The modifications to the 'dark' palette are to make it look
	; more like zenmonk.
	(eval-after-load 'alect-themes
		`(progn
			; general background
			(alect-set-color 'light 'bg-1 "#f6f0e1")
			; cursor line highlight
			(alect-set-color 'light 'bg "#e2efbf")
			; visual line selection
			(alect-set-color 'light 'cursor "DodgerBlue1")
			(alect-set-color 'light 'red ,zm-red+2)
			(alect-set-color 'light 'magenta-1 ,zl-fg)
			(alect-set-color 'light 'magenta+1 "maroon3")
			(alect-set-color 'light 'red-2 ,zl-red)
			(alect-set-color 'light 'cyan-2 "DodgerBlue1")
			(alect-set-color 'light 'green-1 "firebrick3")
			; ace-jump's background face's foreground color
			(alect-set-color 'light 'bg+2 "#8e876c")
			; elisp docstring color
			(alect-set-color 'light 'fg-1 "#059a05")

			; general background
			(alect-set-color 'dark 'bg-1 ,zm-bg)
			; cursor line highlight
			(alect-set-color 'dark 'bg "#434443")
			; trailing whitespace (red)
			(alect-set-color 'dark 'red ,zm-red+2)
			(alect-set-color 'dark 'red-2 ,zm-red)
			(alect-set-color 'dark 'green+1 ,zm-green)
			(alect-set-color 'dark 'green-1 ,zm-orange+1)
			(alect-set-color 'dark 'fg-1 ,zm-green+2)
			(alect-set-color 'dark 'magenta-1 ,zm-fg)
			(alect-set-color 'dark 'magenta+1 ,zm-yellow+1)
			(alect-set-color 'dark 'blue-1 ,zm-fg)
			(alect-set-color 'dark 'blue+1 ,zm-orange+1)
			(alect-set-color 'dark 'cyan-1 ,zm-red+1)
			(alect-set-color 'dark 'cursor "#00ff00")
			(alect-set-color 'dark 'yellow+2 ,zm-yellow)

			(alect-set-color 'black 'bg-1 "gray10")
		)
	)
)

(provide 'l-zenmonk)

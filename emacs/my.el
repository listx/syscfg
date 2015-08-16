; Inspired by evil-mark-replace (https://github.com/redguardtoo/evil-mark-replace).
(defun my/replace-in-buffer ()
	(interactive)
	(let
		(
			(replace-me (regexp-quote (if (region-active-p)
				(buffer-substring-no-properties (region-beginning) (region-end))
				(thing-at-point 'word))
				))
		)
		; Instead of calling vanilla `evil-ex', call it with a hook to set up
		; the cursor's position as well. This way we can "pre-type" to the end
		; of the string and then bring the cursor back where we want it.
		(minibuffer-with-setup-hook
			; Move cursor back 3 columns, behind the trailing "/gc" string.
			(lambda () (backward-char 3))
			(evil-ex (concat
				"%s/"
				(if (region-active-p)
					replace-me
					(concat "\\<" replace-me "\\>")
				)
				"//gc"))
		)
	)
)

; Adopted from http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun my/copy-file-name-to-clipboard ()
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

(defun my/github-link-prefix (project-folder)
	"Generate a github upstream link. It is assumed that projectile is
functioning already here."
	(interactive)
	(let*
		(
			(com
				(concat
					"cd "
					project-folder
					" && git remote show -n upstream | grep Push"
				)
			)
			(github-user/repo (replace-regexp-in-string "\n" ""
				(car (last (split-string (shell-command-to-string com) ":")))))
			(upstream-url
				(concat
					"https://github.com/"
					github-user/repo
					"/blob/develop"
				)
			)
		)
		upstream-url
	)
)

(defun my/copy-for-slack ()
	"Copy region for Slack, and also add metadata/formatting around it for easy
pasting. If no region is selected, copy just the buffer's filename."
	(interactive)
	(let*
		(
			(filename
				(if (equal major-mode 'dired-mode)
					default-directory
					(buffer-file-name)
				)
			)
			(project-name
				(nth 1 (reverse (split-string (projectile-project-root) "/")))
			)
			(delete-me
				(regexp-quote
					(concat
						project-name
					)
				)
			)
			(project-filename
				(replace-regexp-in-string
					(concat
						"\/.+??"
						delete-me
					)
					"" filename)
			)
			(region-end-no-newline
				(if (use-region-p)
					(- (region-end)
						(if (char-equal ?\n (char-before (region-end))) 1 0)
					)
					nil
				)
			)
			(selection
				(if (use-region-p)
					(buffer-substring-no-properties
						(region-beginning)
						region-end-no-newline
					)
					(buffer-substring-no-properties
						(line-beginning-position)
						(line-end-position)
					)
				)
			)
			(selection-lines
				(if (use-region-p)
					(let
						(
							(line-beg (line-number-at-pos (region-beginning)))
							(line-end (line-number-at-pos
								region-end-no-newline))
						)
						; If we do an intra-line selection, the beginning and
						; end regions will be on the same line. In this case,
						; just return 1 single line.
						(if (= line-beg line-end)
							(list (number-to-string line-beg))
							(mapcar 'number-to-string
								(list line-beg line-end))
						)
					)
					(list (number-to-string (line-number-at-pos
						(line-beginning-position))))
				)
			)
			(selection-lines-header
				(if (< 1 (length selection-lines))
					(concat
						"lines "
						(car selection-lines)
						" - "
						(nth 1 selection-lines)
						"\n"
					)
					""
				)
			)
			(github-link
				(if (projectile-project-root)
					; We're in a projectile-handled folder. Presumably this
					; means we are in a github repo. If so, look for a github
					; remote called "upstream" and lift parts of that to build
					; our link to github (which is presumably where upstream is
					; located).
					(concat
						" "
						(my/github-link-prefix (projectile-project-root))
						project-filename
						"#"
						(if (< 1 (length selection-lines))
							(concat
								"L"
								(car selection-lines)
								"-"
								"L"
								(nth 1 selection-lines)
							)
							(concat "L" (car selection-lines))
						)
					)
					""
				)
			)
			(slack-msg
				(concat
					"`"
					(concat project-name project-filename)
					"`\n"
					github-link
					":\n```"
					""
					selection-lines-header
					(replace-regexp-in-string "." "-"
						selection-lines-header)
					selection
					"```"
				)
			)
		)
		(progn
			(kill-new slack-msg)
			(message "Clipboard: '%s' contents for Slack" project-filename)
		)
	)
)

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
					(goto-char pos)
					(evil-first-non-blank)
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
					(forward-line 1)
					(evil-first-non-blank)
					(forward-char 1)
				)
			)
			(my/paste-X-primary)
		)
	)
)

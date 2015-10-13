; Inspired by evil-mark-replace
; (https://github.com/redguardtoo/evil-mark-replace).
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

; Adopted from
; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
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
			(github-user/repo (replace-regexp-in-string "\.git$" ""
				(replace-regexp-in-string "\n" ""
				(car (last (split-string (shell-command-to-string com) ":"))))))
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

(defun my/strip-leading-zeroes (str)
	(replace-regexp-in-string "^0+" "" str)
)

(defun my/copy-for-slack (insert-github-link)
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
			(github-link-prefix
				(my/github-link-prefix (projectile-project-root)))
			(region-beg
				(if (use-region-p)
					(save-excursion
						(goto-char (region-beginning))
						(line-beginning-position)
					)
					nil
				)
			)
			(region-end-no-newline
				(if (use-region-p)
					(save-excursion
						(goto-char (region-end))
						(if (char-equal ?\n (progn (backward-char 1) (point)))
							(- (point) 1)
							(line-end-position)
						)
					)
					nil
				)
			)
			(selection
				(if (use-region-p)
					(buffer-substring-no-properties
						region-beg
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
					(let*
						(
							(line-beg (line-number-at-pos (region-beginning)))
							(line-end (line-number-at-pos
								region-end-no-newline))
							(line-num-list
								; If we do an intra-line selection, the
								; beginning and end regions will be on the same
								; line. In this case, just return 1 single line.
								(if (= line-beg line-end)
									(list (number-to-string line-beg))
									(mapcar 'number-to-string
										(number-sequence line-beg line-end))
								)
							)
							(max-digits (length (car (last line-num-list))))
						)
						(mapcar
							(lambda (num-str)
								(if (< (length num-str) max-digits)
									(concat
										(make-string
											(- max-digits (length num-str)) ?0)
										num-str
									)
									num-str
								)
							)
							line-num-list
						)
					)
					(list (number-to-string (line-number-at-pos
						(line-beginning-position))))
				)
			)
			(selection-with-lines
				(let
					(
						(line-num-and-line-pairs
							(mapcar* 'cons selection-lines
								(split-string selection "\n")))
					)
					(mapconcat
						(lambda (pair) (concat (car pair) " |" (cdr pair)))
						line-num-and-line-pairs "\n")
				)
			)
			(github-link
				(if (and
						(projectile-project-root)
						(not (string-match " " github-link-prefix)))
					; We're in a projectile-handled folder. Presumably this
					; means we are in a github repo. If so, look for a github
					; remote called "upstream" and lift parts of that to build
					; our link to github (which is presumably where upstream is
					; located).
					(concat
						" "
						github-link-prefix
						project-filename
						"#"
						(if (< 1 (length selection-lines))
							(concat
								"L"
								(my/strip-leading-zeroes
									(car selection-lines))
								"-"
								"L"
								(my/strip-leading-zeroes
									(car (last selection-lines)))
							)
							(concat "L" (car selection-lines))
						)
						"\n"
					)
					""
				)
			)
			(slack-msg
				(concat
					"`"
					(concat project-name project-filename)
					"`\n"
					(when insert-github-link github-link)
					"```"
					selection-with-lines
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
	(let
		(
			(xpribuf (x-selection 'PRIMARY))
		)
		(insert xpribuf)
		; Prevent our 'undo usage of 'o' Insert state exit hook from undoing
		; this paste.
		(setq my/before-open-line nil)
	)
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

(defun my-buffer-restart ()
	(interactive)
	(let
		(
			(file (buffer-file-name))
			(p (point))
		)
		(undo-tree-save-history)
		(kill-this-buffer)
		(find-file file)
		(if (string-match "\\.lhs$" file)
			(latex-mode)
		)
		(undo-tree-load-history)
		(goto-char p)
	)
)

; For mode names that match the 'lang' in '#+begin_src lang', we don't need to
; provide an optional submode. But for those that don't match, we can do it like
; this:
;
;   (my-mmm-org-auto-class "fortran" 'f90-mode)
;   (my-mmm-org-auto-class "perl" 'cperl-mode)
;   (my-mmm-org-auto-class "shell" 'shell-script-mode)
;
; Adopted from http://jblevins.org/log/mmm.
(defun my-mmm-org-auto-class (lang &optional submode)
	"Define a mmm-mode class for LANG in `org-mode' using SUBMODE.
	If SUBMODE is not provided, use `LANG-mode' by default."
	(let
		(
			(class (intern (concat "org-my-mmm-" lang)))
			(submode (or submode (intern (concat lang "-mode"))))
			(front (concat "^\\#\\+begin_src " lang "\n"))
			(back "^\\#\\+end_src$")
		)
		(mmm-add-classes (list (list class :submode submode :front front :back back)))
		(mmm-add-mode-ext-class 'org-mode nil class)
	)
)

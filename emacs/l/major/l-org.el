(use-package org
	:config
	(add-hook 'org-mode-hook 'l/org-mode-hook)
	; Write timestamp when a TODO changes to DONE.
	(setq org-log-done t)
	; Mark a TODO item as DONE.
	(evil-define-key 'normal org-mode-map "T" 'org-todo)
	(setq org-agenda-files (list "~/org"))
	; Org-babel settings (for evaluating code blocks).
	(org-babel-do-load-languages
		'org-babel-load-languages
		'(
			(C . t)
			(clojure . t)
			(haskell . t)
			(java . t)
			(python . t)
			(shell . t)
			(sql . t)
		)
	)
	; Use cider for evaluating clojure source code blocks. This requires starting
	; up cider first, with `M-x cider-jack-in' before doing C-c C-c.
	(setq org-babel-clojure-backend 'cider)

	; Disable source-code evaluation prompt. The prompt is enabled by default for
	; security reasons, but disable it because I'm not downloading other people's
	; org-mode files and running babel on them.
	(setq org-confirm-babel-evaluate nil)

	; Allow single/double quote marks in inline '='-delimited verbatim formatting.
	; see https://lists.gnu.org/archive/html/emacs-orgmode/2014-04/msg00199.html
	(setq org-emphasis-regexp-components
		'(" \t('\"{" "- \t.,:!?;'\")}\\" " \t\r\n" "." 1))
	; Do not convert TAB characters in source code blocks into spaces.
	(setq org-src-preserve-indentation t)

	; Hide formatting characters (e.g., the `/' in /italic text/).
	(setq org-hide-emphasis-markers t)

	; Use Unicode bullets for headings with `org-bullets' package.
	(use-package org-bullets)

	; Make leading heading asterisks invisible.
	(font-lock-add-keywords 'org-mode
		'(("^\\([*]+\\)[*] "
		(0 (add-text-properties (match-beginning 1) (match-end 1) '(invisible t))))))

	; Use Unicode bullet character for bullets and lists.
	(font-lock-add-keywords 'org-mode
		'(("^ *\\([-]\\) "
		(0 (compose-region (match-beginning 1) (match-end 1) "•")))))

	(setq org-publish-project-alist
		'(
			("eh"
				:base-directory "~/prog/elementary-haskell/"
				:publishing-directory "~/prog/elementary-haskell/public_html"
				:publishing-function org-html-publish-to-html
				:section-numbers t
				:with-toc t
				:headline-levels 4
				:html-preamble t
				:html-head "<link rel=\"stylesheet\"
					href=\"css/style.css\"
					type=\"text/css\"/>"
			)
			("babbage"
				:base-directory "~/prog/babbage/"
				:publishing-directory "~/prog/babbage/public_html"
				:publishing-function org-html-publish-to-html
				:section-numbers t
				:with-toc t
				:headline-levels 4
				:html-preamble t
				:html-head "<link rel=\"stylesheet\"
					href=\"css/style.css\"
					type=\"text/css\"/>"
			)
			("day"
				:base-directory "~/org/day"
				:publishing-directory "~/org/day/public_html"
				:publishing-function org-html-publish-to-html
				:section-numbers t
				:with-toc nil
				:html-preamble nil
				:html-postamble nil
				:html-head "<link rel=\"stylesheet\"
					href=\"css/style.css\"
					type=\"text/css\"/>"
			)
	))

)

(defun l/org-mode-hook ()
	(modify-syntax-entry ?_ "w")
	; Disable default orgmode hotkeys that interfere with our global hotkeys
	; defined elsewhere.
	(define-key org-mode-map [(tab)] nil)
	(define-key org-mode-map (kbd "<S-iso-lefttab>") nil)

	; Enable org-bullets-mode.
	(org-bullets-mode 1)
)

(defhydra hydra-org (:foreign-keys warn)
	"org"
	("b" org-cycle-list-bullet "cycle-bullet-type")
	("l" mmm-parse-buffer "turn on mmm-mode")
	("m" mmm-mode "turn on mmm-mode")
	("s" org-beamer-export-to-pdf "save beamer to pdf")
	("w" org-publish-current-project "publish current project")
	("q" nil "exit" :exit t)
)

(general-define-key
	:keymaps 'org-mode-map
	:states '(normal)
	"1" 'hydra-org/body
)

(evil-define-key 'normal org-mode-map (kbd "M-i") 'org-insert-link)
(evil-define-key 'insert org-mode-map (kbd "M-i") 'org-insert-link)
(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-open-at-point)
(evil-define-key 'normal org-mode-map (kbd "M-n") 'org-shiftright)
; Heading-based navigation.
(evil-define-key 'normal org-mode-map (kbd "M-k")
	'outline-previous-visible-heading)
(evil-define-key 'normal org-mode-map (kbd "M-j")
	'outline-next-visible-heading)
; Move items around, including child nodes.
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-demote-subtree)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-promote-subtree)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-move-subtree-up)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-move-subtree-down)

(evil-define-key 'normal org-mode-map (kbd "<f12>") 'org-html-export-to-html)

; Fold/expand all headings globally one level (use C-TAB) for expanding/folding
; a particular level.
; The 'iso-lefttab' just means 'tab'. It probably has something to do with the
; keyboard layout also.
(evil-define-key 'normal org-mode-map (kbd "<C-S-iso-lefttab>") 'org-shifttab)

(evil-define-key 'normal org-mode-map (kbd "M-p") 'l/org-paste-src)

(defun l/org-paste-src ()
	(interactive)
	(evil-append nil)
	(forward-line 1)
	(beginning-of-line)
	(insert "#+begin_src\n")
	(l/paste-X-primary)
	(if (not (string= "\n" (string (char-before (point)))))
		(insert "\n")
	)
	(insert "#+end_src\n")
	(evil-normal-state)
	(forward-line -1)
	(message nil)
)

(provide 'l-org)

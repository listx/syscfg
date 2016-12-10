(use-package markdown-mode
	:config
	(autoload
		'markdown-mode
		"markdown-mode"
		"Major mode for editing Markdown files"
		t)
	(setq auto-mode-alist (cons '("\.md$" . markdown-mode) auto-mode-alist))
	(add-hook 'markdown-mode-hook 'l/markdown-setup)
)

(defun l/markdown-setup ()
	(define-key markdown-mode-map (kbd "<S-tab>") nil)
)

(provide 'l-markdown)

(use-package magit
	:config
	(add-hook 'git-commit-mode-hook 'evil-insert-state)
)

(provide 'l-magit)

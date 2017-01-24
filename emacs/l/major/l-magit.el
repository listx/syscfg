(use-package magit
	:config
	(add-hook 'git-commit-mode-hook 'evil-insert-state)
	(add-hook 'magit-mode-hook 'l/magit-setup)
)

(defun l/magit-setup ()
	(setq kakapo-mode nil)
)

(provide 'l-magit)

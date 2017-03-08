(use-package evil-magit
	:config
	(defun l/nop (orig-fun &rest args)
		"Ignore `orig-fun'."
	)
	(advice-add 'evil-magit-add-rebase-messages :around #'l/nop)
	(advice-add 'git-rebase-mode-show-keybindings :around #'l/nop)
)

(provide 'l-evil-magit)

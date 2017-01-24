(use-package evil-ediff
	:config
	(add-hook 'ediff-mode-hook 'l/ediff-setup)
)

(defun l/ediff-setup ()
	(setq kakapo-mode nil)
)

(provide 'l-evil-ediff)

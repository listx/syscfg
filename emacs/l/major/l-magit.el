(use-package magit
  :config
  (add-hook 'magit-mode-hook 'l/magit-setup))

(defun l/magit-setup ()
  (setq kakapo-mode nil))

(provide 'l-magit)

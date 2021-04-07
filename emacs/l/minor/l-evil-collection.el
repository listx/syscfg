(use-package evil-collection
  :after evil
  :defer t
  :ensure t
  :config
  (evil-collection-init)
  (defun l/nop (orig-fun &rest args)
    "Ignore `orig-fun'.")
  (advice-add 'evil-magit-add-rebase-messages :around #'l/nop)
  (advice-add 'git-rebase-mode-show-keybindings :around #'l/nop))

(provide 'l-evil-collection)

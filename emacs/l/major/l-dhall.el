(use-package dhall-mode
  :mode "\\.dhall\\'"
  :config
  (add-hook 'dhall-mode-hook 'l/dhall-setup)
  (defun l/dhall-setup ()
    (kakapo-mode)
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq evil-shift-width 4)))

(provide 'l-dhall)

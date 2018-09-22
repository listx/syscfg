(use-package rust-mode
  :config
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (use-package flycheck-rust)
  (add-hook 'rust-mode-hook 'l/rust-setup))

(defun l/rust-setup ()
  (setq rust-format-on-save t)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (flycheck-mode))

(provide 'l-rust)

(use-package elixir-mode
  :mode "\\.ex\\'"
  :config
  (use-package flycheck-credo)
  (use-package flycheck-elixir)
  (add-hook 'elixir-mode-hook 'l/elixir-setup))

(defun l/elixir-setup ()
  (flycheck-mode)
  (flycheck-credo-setup)
  (flycheck-add-next-checker 'elixir 'elixir-credo)
  ; Format when saving.
  (add-hook 'before-save-hook 'elixir-format nil t))

(provide 'l-elixir)

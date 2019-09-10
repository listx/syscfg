(use-package elixir-mode
  :config
  (use-package flycheck-elixir)
  (add-hook 'elixir-mode-hook 'l/elixir-setup))

(defun l/elixir-setup ()
  (flycheck-mode)
  ; Format when saving.
  (add-hook 'before-save-hook 'elixir-format nil t))

(provide 'l-elixir)

(use-package erlang
  :mode "\\.erl\\'"
  :config
  (add-hook 'erlang-mode-hook 'l/erlang-setup))

(defun l/erlang-setup ()
  (flycheck-mode))

(provide 'l-erlang)

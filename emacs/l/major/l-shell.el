; This is shell-mode, which enters an interactive shell inside emacs.

(add-hook 'shell-mode-hook 'l/shell-setup)

(defun l/shell-setup ()
  ; Make M-j/k move up and down shell history.
  (define-key shell-mode-map (kbd "M-j") 'comint-next-input)
  (define-key shell-mode-map (kbd "M-k") 'comint-previous-input))

(provide 'l-shell)

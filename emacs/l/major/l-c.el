(add-hook 'c-mode-hook 'l/c-setup)

(defun l/c-setup ()
  (c-set-style "linux"))

(provide 'l-c)

(add-hook 'prog-mode-hook 'l/prog-mode-hook)

(defun l/prog-mode-hook ()
  ; Disable highlighting long lines if they are comment lines.
  (setq column-enforce-comments nil)
  (column-enforce-mode)
  ; Add underscore as a word character, like in Vim.
  (modify-syntax-entry ?_ "w"))

(provide 'l-prog-mode)

(add-hook 'html-mode-hook 'l/html-setup)

(defun l/html-setup ()
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w"))

(provide 'l-html)

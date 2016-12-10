(add-hook 'conf-mode-hook 'l/conf-mode-hook)

(defun l/conf-mode-hook ()
	(modify-syntax-entry ?_ "w")
	(modify-syntax-entry ?- "w")
)

(provide 'l-conf-mode)

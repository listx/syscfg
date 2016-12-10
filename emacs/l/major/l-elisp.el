(add-hook 'emacs-lisp-mode-hook 'l/elisp-setup)

(defun l/elisp-setup ()
	(modify-syntax-entry ?- "w")
)

(provide 'l-elisp)

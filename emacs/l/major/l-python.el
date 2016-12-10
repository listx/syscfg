(add-hook 'python-mode-hook 'l/python-setup)

(defun l/python-setup ()
	; Disable Python <backspace> binding.
	(define-key python-mode-map (kbd "<backspace>") nil)
)

(provide 'l-python)

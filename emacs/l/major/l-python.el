(add-hook 'python-mode-hook 'l/python-setup)

(defun l/python-setup ()
	; Disable Python <backspace> binding.
	(define-key python-mode-map (kbd "<backspace>") nil)
	; Check with both flake8 and pylint.
	(flycheck-mode)
	(flycheck-add-next-checker 'python-flake8 'python-pylint)
)

(provide 'l-python)

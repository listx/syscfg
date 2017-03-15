(add-hook 'python-mode-hook 'l/python-setup)

(defun l/python-setup ()
	; Disable Python <backspace> binding.
	(define-key python-mode-map (kbd "<backspace>") nil)
	; Check with pylint. Flycheck automatically tries pylint if it is installed.
	(flycheck-mode)
)

(provide 'l-python)

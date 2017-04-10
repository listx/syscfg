(add-hook 'python-mode-hook 'l/python-setup)

(defun l/python-setup ()
	; Disable Python <backspace> binding.
	(define-key python-mode-map (kbd "<backspace>") nil)
	; Check with both flake8 and pylint.
	(flycheck-mode)
	(flycheck-add-next-checker 'python-flake8 'python-pylint)
	; Set max line length to 79 characters (from PEP8). (Although Emacs columns
	; are 0-indexed, column-enforce-mode counts from 1, so we use 79 here and
	; not 78.)
	(setq column-enforce-column 79)
	; We need to tell Emacs to do paragrah-filling at 79 caharacters
	; (column-enforce-mode only highlights regions --- it does not change how
	; paragraph filling is done).
	(setq fill-column 79)
)

(provide 'l-python)

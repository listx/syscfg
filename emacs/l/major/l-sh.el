(add-hook 'sh-mode-hook 'l/sh-setup)

(defun l/sh-setup ()
	; Check with shellcheck. Flycheck uses shellcheck if it is installed.
	(flycheck-mode)
)

(provide 'l-sh)

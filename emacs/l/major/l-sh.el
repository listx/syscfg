(add-hook 'sh-mode-hook 'l/sh-setup)

(defun l/sh-setup ()
  ; Check with shellcheck. Flycheck uses shellcheck if it is installed.
  (flycheck-mode)
  ; Disable checking for POSIX compliance.
  (setq flycheck-disabled-checkers '(sh-posix-bash)))

(provide 'l-sh)

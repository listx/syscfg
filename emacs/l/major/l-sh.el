(add-hook 'sh-mode-hook 'l/sh-setup)

(defun l/sh-setup ()
  ; Disable checking for POSIX compliance.
  (setq flycheck-disabled-checkers '(sh-posix-bash))
  ; Check with shellcheck. Flycheck uses shellcheck if it is installed.
  (flycheck-mode))

(provide 'l-sh)

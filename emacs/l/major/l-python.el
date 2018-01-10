(add-hook 'python-mode-hook 'l/python-setup)

(defun l/python-setup ()
  ; Disable Python <backspace> binding.
  (define-key python-mode-map (kbd "<backspace>") nil)
  ; Check with both flake8 and pylint.
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (when (and
    (string-match "enif" system-name)
    (l/is-google-codebase))
    (l/python-setup-google))
  ; Start Flycheck.
  (flycheck-mode)
  ; Set max line length to 79 characters (from PEP8). (Although Emacs columns
  ; are 0-indexed, column-enforce-mode counts from 1, so we use 79 here and
  ; not 78.)
  (setq column-enforce-column 79)
  ; We need to tell Emacs to do paragrah-filling at 79 caharacters
  ; (column-enforce-mode only highlights regions --- it does not change how
  ; paragraph filling is done).
  (setq fill-column 79))

(defun l/is-google-codebase ()
  (member "google3" (split-string buffer-file-name "/")))

(defun l/python-setup-google ()
  (add-to-list 'flycheck-checkers 'python-google)
  (flycheck-select-checker 'python-google))

; See https://groups.google.com/a/google.com/forum/#!msg/emacs-users/soot-15MWzs/26miZLMswigJ.
(flycheck-define-checker python-google
  "A Python syntax checker for Google-style code."
  :command ("gpylint"
    "--msg-template"
    "{path}:{line}:{column}:{C}:{msg} ({symbol}/{msg_id})"
    source-inplace)
  :error-patterns
  ((error line-start
    (file-name) ":" line ":" column ":" (or "E" "F") ":" (message)
    line-end)
  (warning line-start
    (file-name) ":" line ":" column ":" (or "W" "R") ":" (message)
    line-end)
  (info line-start
    (file-name) ":" line ":" column ":" "C:" (message)
    line-end))
  :modes (python-mode))

(provide 'l-python)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :after (flycheck)
  :config
  (add-hook 'python-mode-hook 'l/python-setup)

  ; Based on
  ; https://stackoverflow.com/questions/31443527/how-can-i-make-flycheck-use-virtualenv.
  ; Depends on modifying Python's sys.path in .pylintrc as in
  ; https://stackoverflow.com/a/39207275/437583 for this to work.
  (defun set-flychecker-executables ()
    "Configure virtualenv for flake8 and lint."
    (when (executable-find "flake8")
    (flycheck-set-checker-executable (quote python-flake8)
                                  (executable-find "flake8")))
    (when (executable-find "pylint")
    (flycheck-set-checker-executable (quote python-pylint)
                                  (executable-find "pylint"))))

  (defun l/python-setup ()
    ; Disable Python <backspace> binding.
    (define-key python-mode-map (kbd "<backspace>") nil)
    ; Check with both flake8 and pylint.
    (flycheck-add-next-checker 'python-flake8 'python-pylint)
    (add-hook 'flycheck-before-syntax-check-hook #'set-flychecker-executables
      'local)
    ; Start Flycheck.
    (flycheck-mode)
    ; Set max line length to 79 characters (from PEP8). (Although Emacs columns
    ; are 0-indexed, column-enforce-mode counts from 1, so we use 79 here and
    ; not 78.)
    (setq column-enforce-column 79)
    ; We need to tell Emacs to do paragrah-filling at 79 caharacters
    ; (column-enforce-mode only highlights regions --- it does not change how
    ; paragraph filling is done).
    (setq fill-column 79)))

(provide 'l-python)

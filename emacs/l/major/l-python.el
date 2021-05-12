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
                                  (executable-find "pylint")))
    (when (executable-find "mypy")
    (flycheck-set-checker-executable (quote python-mypy)
                                  (executable-find "mypy"))))

  (defun l/python-setup ()
    ; Disable Python <backspace> binding.
    (define-key python-mode-map (kbd "<backspace>") nil)

    (when (not (bound-and-true-p poly-org-mode))
      ; Check with flake8, pylint, and mypy. python-mypy already runs
      ; python-flake8, so there's no need to mention it here. However, we still
      ; need to mention python-pylint to run after python-flake8. This is a
      ; so-called "checker chain", as per
      ; https://www.flycheck.org/en/latest/user/syntax-checkers.html#configuring-checker-chains.
      (flycheck-add-next-checker 'python-flake8 'python-pylint)
      (add-hook 'flycheck-before-syntax-check-hook #'set-flychecker-executables
        'local)
      ; Start Flycheck.
      (flycheck-mode)
      ; Set max line length to 79 characters (from PEP8). (Although Emacs columns
      ; are 0-indexed, column-enforce-mode counts from 1, so we use 79 here and
      ; not 78.)
      ;(setq column-enforce-column 79)
      ; We need to tell Emacs to do paragrah-filling at 79 caharacters
      ; (column-enforce-mode only highlights regions --- it does not change how
      ; paragraph filling is done).
      (setq fill-column 79))))


(provide 'l-python)

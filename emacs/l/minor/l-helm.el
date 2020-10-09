(use-package helm
  :config
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-ff-skip-boring-files t)

  ; Disable help string. See http://stackoverflow.com/a/19954900/437583.
  (defadvice helm-display-mode-line (after undisplay-header activate)
    (setq header-line-format nil))

  ; Fuzzy matching for "M-x". We have to add the binding as well, because without
  ; it we get vanilla M-x (which, although it is 'helmified', does not use the
  ; fuzzy matching behavior).
  (setq helm-M-x-fuzzy-match t)
  (global-set-key (kbd "M-x") 'helm-M-x)

  ; We need to use 'eval-after-load' because otherwise we get an error about
  ; `helm-map' not existing yet.
  (with-eval-after-load "helm-mode"
    (cl-loop for ext in '("\\.elc$")
      do (add-to-list 'helm-boring-file-regexp-list ext))

    ; From http://emacs.stackexchange.com/a/7896. Slightly modified as usual.

    ; `helm-maybe-exit-minibuffer' opens the argument and exits helm.
    ; `helm-execute-persistent-action' pseudo-opens the argument, and stays in
    ; helm. We want to use the second one and stay in helm as much as possible
    ; (e.g., esp. when navigating directories), because then we basically get
    ; most of the power of Dired-mode but combined with helm's fuzzy-matching.
    ; We could technically just call `fu/helm-find-files-navigate-forward'
    ; directly instead of using `advice-add', but it's worth keeping for
    ; historical reasons.
    (defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
      (cond
        (
          (and
            (eq 'string (type-of (helm-get-selection)))
            (not (string-match "\/\\.$" (helm-get-selection)))
            (file-directory-p (helm-get-selection)))
          (apply orig-fun args))
        (t (helm-maybe-exit-minibuffer))))
    (advice-add 'helm-execute-persistent-action
      :around #'fu/helm-find-files-navigate-forward)

    (defun fu/helm-find-files-navigate-back (orig-fun &rest args)
    (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
      (apply orig-fun args)))
    (advice-add 'helm-ff-delete-char-backward
      :around #'fu/helm-find-files-navigate-back)

    (define-key helm-map (kbd "<tab>") 'hydra-helm/body)

    ; As of 576cc21f381977e1d3c509d94f73853a74612cff, the
    ; `helm-find-files-doc-header' hardcodes the default `C-l' binding. We set
    ; it to nil to suppress the message from `helm-find-files'.
    (setq helm-find-files-doc-header nil)
    (define-key helm-find-files-map (kbd "RET") 'helm-execute-persistent-action))

  (helm-mode 1))

(provide 'l-helm)

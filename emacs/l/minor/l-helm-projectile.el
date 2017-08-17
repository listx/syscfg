(use-package helm-projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile))

; Take from README of https://github.com/syohex/emacs-helm-ag.
(defun l/projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

(provide 'l-helm-projectile)

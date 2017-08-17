(use-package git-gutter
  :config
  ; Git diff +/- marks.
  (global-git-gutter-mode +1)
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign " ")
  (setq git-gutter:deleted-sign " ")
  (setq git-gutter:update-interval 2)
  (set-face-background 'git-gutter:added "lime green")
  (set-face-background 'git-gutter:modified "purple")
  (set-face-background 'git-gutter:deleted "red"))

(provide 'l-git-gutter)

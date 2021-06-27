(use-package git-gutter
  :config
  ; Git diff +/- marks.
  (global-git-gutter-mode +1)
  ; Update git-gutter every time we lose/regain focus to the frame.
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows)
  ; Update git-gutter every time we lose/regain focus of the current window.
  ; This is to catch cases where we are SSH-ed in to a machine and are running
  ; emacs in terminal mode, which doesn't get the same "frame" focus signals as
  ; above because there is literally no frame.
  (advice-add 'select-window :around #'l/git-gutter-refresh)
  (defun l/git-gutter-refresh (orig-fun &rest args)
    (prog1
      (apply orig-fun args)
      (git-gutter:update-all-windows)))
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign " ")
  (setq git-gutter:deleted-sign " ")
  (setq git-gutter:update-interval 2))

(provide 'l-git-gutter)

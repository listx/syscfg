(add-hook 'git-rebase-mode-hook 'l/git-rebase-keymap)

; git-rebase.el is provided by Magit, and is pretty awful.
(defun l/git-rebase-keymap ()
  "My secret for git-rebase mode: just treat it as text, the way it was meant
to be used."
  (interactive)
  (toggle-read-only nil)
  (text-mode))

(provide 'l-git-rebase)

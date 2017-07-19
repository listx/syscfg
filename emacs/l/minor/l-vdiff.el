(use-package vdiff
  :config
  (add-hook 'vdiff-mode-hook 'l/vdiff-setup))

(defun l/vdiff-setup ()
  (general-define-key
    :keymaps 'vdiff-mode-map
    :states '(normal)
    "0" 'vdiff-hydra/body))

(provide 'l-vdiff)

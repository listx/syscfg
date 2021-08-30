(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-db-autosync-mode)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/lo/note")
  (org-roam-completion-everywhere t)
  :config
  ; Some keybindings that work in Evil's Insert mode, lifted from
  ; https://youtu.be/AyhPmypHDEw?t=1238.
  :bind (("C-c n E" . org-roam-buffer-toggle)
         ("C-c n e" . org-roam-node-find)
         ("C-c n u" . org-roam-node-insert)))

(provide 'l-org-roam)

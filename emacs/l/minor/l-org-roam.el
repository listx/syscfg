(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-db-autosync-mode)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/lo/note"))

(provide 'l-org-roam)

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-db-autosync-mode)
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory "~/lo/note"))

(provide 'l-org-roam)

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :config
  (require 'org-roam-protocol)
  (use-package org-roam-server
    :ensure t
    :config
    (add-hook 'emacs-startup-hook '(lambda ()
      (if (daemonp) (org-roam-server-mode))))
    (setq org-roam-server-host
            (cond
              ; If we are on k0, set local static IP as the address, so that
              ; other computers on the network can access it.
              ((string-match "^k0" (system-name)) "192.168.0.4")
              (t "127.0.0.1"))
          org-roam-server-port 8010
          org-roam-server-export-inline-images t
          org-roam-server-authenticate nil
          org-roam-server-network-poll t
          org-roam-server-network-arrows nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20))
  :custom
  (org-roam-directory "~/lo/note")
  (org-roam-index-file "~/lo/note/index.org")
  (org-roam-capture-templates
    '(("d" "default" plain (function org-roam--capture-get-point)
        "%?"
        :file-name "${slug}"
        :head "#+TITLE: ${title}\n"
        :unnarrowed t)))
  (org-roam-capture-ref-templates
    '(("r" "ref" plain (function org-roam-capture--get-point)
        "%?"
        :file-name "ref/${slug}"
        :head "#+roam_key: ${ref}
#+roam_tags: website
#+TITLE: ${title}
- source :: ${ref}"
        :unnarrowed t))))

(provide 'l-org-roam)

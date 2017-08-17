(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockertemplate$" . dockerfile-mode)))

(provide 'l-docker)

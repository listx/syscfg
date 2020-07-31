(use-package dockerfile-mode
  :mode (
    ("Dockerfile\\'" . dockerfile-mode)
    ("Dockertemplate$" . dockerfile-mode)))

(provide 'l-docker)

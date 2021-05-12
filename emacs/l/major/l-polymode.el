(use-package polymode
  :config
  (use-package poly-org
    :after (org))
  (use-package poly-markdown
    :after (polymode))
  (use-package poly-noweb
    :after (polymode)))
(provide 'l-polymode)

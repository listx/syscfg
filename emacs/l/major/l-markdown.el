(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (autoload
    'markdown-mode
    "markdown-mode"
    "Major mode for editing Markdown files"
    t)
  (add-hook 'markdown-mode-hook 'l/markdown-setup)
  (defun l/markdown-setup ()
    (define-key markdown-mode-map (kbd "<S-tab>") nil)
    (define-key markdown-mode-map (kbd "<S-iso-lefttab>") nil)
    (define-key markdown-mode-map (kbd "<backtab>") nil)))

(provide 'l-markdown)

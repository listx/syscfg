(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode)
  (add-hook 'text-mode-hook 'rainbow-mode)
  (add-hook 'conf-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook  'rainbow-mode))

(provide 'l-rainbow-mode)

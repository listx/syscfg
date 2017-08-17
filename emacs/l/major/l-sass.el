(use-package sass-mode
  :config
  (autoload 'sass-mode "sass-mode" "Major mode for editing Sass files" t)
  (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode)))

(provide 'l-sass)

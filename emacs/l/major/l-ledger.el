(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (set-face-attribute 'ledger-font-xact-highlight-face nil :weight 'bold :inherit nil)
  (autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))

(provide 'l-ledger)

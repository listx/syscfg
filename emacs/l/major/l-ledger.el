(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))

(provide 'l-ledger)

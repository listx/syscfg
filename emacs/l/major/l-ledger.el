(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (set-face-attribute 'ledger-font-xact-highlight-face nil :weight 'bold :inherit nil)
  (autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
  ; Go down to the bottom of the file, because the latest entries are on the
  ; bottom. Although (h)ledger allows transactions to be written in any order
  ; (regardless of date), the balance assertions are read in-order (top to
  ; bottom). This means that if we use balance assertions, the only sane way to
  ; input new transactions is to always put them at the bottom. That is, the
  ; ledger must be sorted in chronological order.
  ;
  ; So, automatically go to the bottom of the buffer after opening a ledger
  ; file, because we most likely need to be there to add in new entries.
  (add-hook 'ledger-mode-hook 'end-of-buffer))

(provide 'l-ledger)

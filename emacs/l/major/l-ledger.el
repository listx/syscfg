(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
; Go to the lastest entries at the end.
(add-hook 'ledger-mode-hook 'evil-goto-line)

(provide 'l-ledger)

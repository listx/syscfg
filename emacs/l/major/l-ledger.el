(use-package ledger-mode
	:config
	(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
	(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
	(add-to-list 'auto-mode-alist '("\\.hledger$" . ledger-mode))
)

(provide 'l-ledger)

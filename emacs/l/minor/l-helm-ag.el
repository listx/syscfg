(use-package helm-ag
	:config
	; Use ripgrep, not ag.
	(setq helm-ag-base-command "rg --vimgrep --no-heading")
	(setq helm-ag-insert-at-point 'word)
)

(provide 'l-helm-ag)

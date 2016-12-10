(use-package helm-ag
	:config
	; Use ripgrep, not ag.
	(setq helm-ag-base-command "rg --vimgrep --no-heading")
)

(provide 'l-helm-ag)

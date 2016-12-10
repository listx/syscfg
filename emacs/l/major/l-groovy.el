(use-package groovy-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
	(add-to-list 'auto-mode-alist '("Jenkinsfile$" . groovy-mode))
)

(provide 'l-groovy)

(use-package page-break-lines
	:config
	(global-page-break-lines-mode)
	(set-fontset-font
		"fontset-default"
		(cons page-break-lines-char page-break-lines-char)
		(face-attribute 'default :family)
	)
)

(provide 'l-page-break-lines)

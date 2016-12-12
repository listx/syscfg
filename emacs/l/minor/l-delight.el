(use-package delight
	:config
	; See http://www.emacswiki.org/emacs/DelightedModes.
	(delight
		'(
			(auto-revert-mode nil magit)
			(column-enforce-mode nil column-enforce-mode)
			(git-gutter-mode nil git-gutter)
			(global-whitespace-mode nil whitespace)
			(helm-mode nil helm-mode)
			(org-indent-mode nil org-indent)
			(page-break-lines-mode nil page-break-lines)
			(undo-tree-mode nil undo-tree)
			(visual-line-mode nil simple)
		)
	)
)

(provide 'l-delight)

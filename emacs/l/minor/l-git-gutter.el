(use-package git-gutter
	:config
	; Git diff +/- marks.
	(global-git-gutter-mode +1)
	(custom-set-variables
	'(git-gutter:modified-sign " ")
	'(git-gutter:added-sign " ")
	'(git-gutter:deleted-sign " "))
	(set-face-background 'git-gutter:added "lime green")
	(set-face-background 'git-gutter:modified "purple")
	(set-face-background 'git-gutter:deleted "red")
)

(provide 'l-git-gutter)

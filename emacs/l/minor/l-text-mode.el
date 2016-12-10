(add-hook 'text-mode-hook 'l/text-mode-hook)

(defun l/text-mode-hook ()
	(modify-syntax-entry ?_ "w")
	(modify-syntax-entry ?- "w")

	; Set TAB to be kakapo-tab for text mode.
	(evil-define-key 'insert text-mode-map (kbd "<tab>") 'kakapo-tab)
)

(provide 'l-text-mode)

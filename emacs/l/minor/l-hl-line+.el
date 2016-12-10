(use-package hl-line+
	:config
	; Highlight the current cursor line; set overlay to a high number to override
	; other properties (e.g., mmm-default-submode-face).
	(setq hl-line-overlay-priority (/ most-positive-fixnum (expt 2 55)))
	(global-hl-line-mode 1)
	; disable cursor line highlight during insert mode
	(l/add-hook-args 'evil-insert-state-entry-hook global-hl-line-mode 0)
	(add-hook 'evil-visual-state-entry-hook 'global-hl-line-mode)
	(add-hook 'evil-visual-state-exit-hook 'global-hl-line-mode)
	(l/add-hook-args 'evil-emacs-state-entry-hook global-hl-line-mode 0)
	(add-hook 'evil-emacs-state-exit-hook 'global-hl-line-mode)
)

(provide 'l-hl-line+)

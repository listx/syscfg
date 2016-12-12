(use-package hl-line+
	:config
	; Highlight the current cursor line; set overlay to a high number to override
	; other properties (e.g., mmm-default-submode-face).
	(setq hl-line-overlay-priority (/ most-positive-fixnum (expt 2 55)))
	; Only highlight when idle.
	(toggle-hl-line-when-idle)
	(setq global-hl-line-mode nil)
	(hl-line-when-idle-interval 0.5)
)

(provide 'l-hl-line+)

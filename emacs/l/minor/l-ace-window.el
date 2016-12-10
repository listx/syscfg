(use-package ace-window
	:config
	; Number each window's unique number into the mode line.
	(ace-window-display-mode)

	; Disable background color-changing when entering ace-window mode.
	(setq aw-background nil)

	; Prefer home row over the 0-9 candidate characters.
	(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
)

(provide 'l-ace-window)

(use-package hydra
	:config
	; Customize how hydra displays keybindings (no brackets/colons).
	(setq hydra-head-format "%s ")

	(defhydra hydra-zoom ()
		"zoom"
		("j"
			(global-text-scale-adjust 1)
			"in")
		("k"
			(global-text-scale-adjust -1)
			"out")
		("q" nil "exit" :exit t)
	)
	(defhydra hydra-window ()
		"Window navigation with hydra."
		("h" windmove-left)
		("j" windmove-down)
		("k" windmove-up)
		("l" windmove-right)
		(","
			(progn
				(ace-window 1)
				(add-hook 'ace-window-end-once-hook 'hydra-window/body))
			"ace"
			:exit t)
		("v"
			(progn
				(split-window-right)
				(windmove-right))
			"vert"
			:exit t)
		("x"
			(progn
				(split-window-below)
				(windmove-down))
			"horz"
			:exit t)
		("s"
			(progn
				(ace-window 4)
				(add-hook 'ace-window-end-once-hook 'hydra-window/body))
			"swap"
			:exit t)
		("d"
			(progn
				(ace-window 16)
				(add-hook 'ace-window-end-once-hook 'hydra-window/body))
			"del"
			:exit t)
		("i" nil "choose" :exit t)
		("I" delete-other-windows "max" :exit t)
		("o" ace-maximize-window "max-choose" :exit t)
	)

	; From http://angelic-sedition.github.io/blog/2015/02/03/a-more-evil-helm/.
	(defhydra hydra-helm (:foreign-keys warn)
		("h" (helm-find-files-up-one-level 1) "parent")
		("j" helm-next-line "down")
		("k" helm-previous-line "up")
		("l" helm-execute-persistent-action "open")
		("H" helm-previous-source "next source")
		("L" helm-next-source "prev source")
		("RET" helm-maybe-exit-minibuffer "open")
		; We have to use "<tab>" and not "TAB" because of some unknown quirk in
		; hydra. Not even "<TAB>" works.
		("<tab>" nil "exit hyrda")
		("<SPC>" helm-select-action "action")
		("m" helm-toggle-visible-mark "mark")
		("u" helm-unmark-all "unmark all")
		("g" helm-beginning-of-buffer "top")
		("G" helm-end-of-buffer "bottom")
		("J" helm-scroll-other-window "scroll other down")
		("K" helm-scroll-other-window-down "scroll other up")
		("?" helm-help "help")
		("q" keyboard-escape-quit "exit")
	)

	; See https://github.com/asok/.emacs.d/.
	(defhydra hydra-magit (:color blue)
		"magit"
		("s" magit-status "status")
		("c" magit-checkout "checkout")
		("b" magit-branch-manager "branch manager")
		("m" magit-merge "merge")
		("l" magit-log "log")
		("!" magit-git-command "command")
		("$" magit-process "process")
	)
)

; System for text scaling (adjusting font size).
; http://www.emacswiki.org/emacs/GlobalTextScaleMode
(defvar text-scale-mode-amount)
(define-globalized-minor-mode
	global-text-scale-mode
	text-scale-mode
	(lambda () (text-scale-mode 1))
)
(defun global-text-scale-adjust (inc) (interactive)
	(text-scale-set 1)
	(kill-local-variable 'text-scale-mode-amount)
	(setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
	(global-text-scale-mode 1)
)

(provide 'l-hydra)

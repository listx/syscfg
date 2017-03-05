(add-hook 'git-rebase-mode-hook 'l/git-rebase-keymap)

; git-rebase.el is provided by Magit.
(defun l/git-rebase-keymap ()
	"My keys for git-rebase mode."
	(interactive)
	(toggle-read-only nil)
	(text-mode)

	; Commented out are my failed attempts at getting git-rebase mode to behave.
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "p" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "r" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "e" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "s" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "f" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "x" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "d" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "u" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "h" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "j" nil)
;	(evil-magit-define-key evil-magit-state 'git-rebase-mode-map "k" nil)

;	(define-key git-rebase-mode-map "p" nil)
;	(define-key git-rebase-mode-map "r" nil)
;	(define-key git-rebase-mode-map "e" nil)
;	(define-key git-rebase-mode-map "s" nil)
;	(define-key git-rebase-mode-map "f" nil)
;	(define-key git-rebase-mode-map "x" nil)
;	(define-key git-rebase-mode-map "d" nil)
;	(define-key git-rebase-mode-map "u" nil)
;	(define-key git-rebase-mode-map "h" nil)
;	(define-key git-rebase-mode-map "j" nil)
;	(define-key git-rebase-mode-map "k" nil)

;	(evil-magit-revert)
)

;(with-eval-after-load "git-rebase"
;	`(progn
;		(l/git-rebase-keymap)
;	)
;)

(provide 'l-git-rebase)

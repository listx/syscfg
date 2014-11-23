; Emacs
(define-key evil-normal-state-map [f1] 'menu-bar-mode) ; toggle the menu bar
(define-key evil-normal-state-map ",w" 'save-buffer) ; save
(define-key evil-normal-state-map ",W" ":w!") ; force save
(define-key evil-normal-state-map ",q" 'vimlike-quit) ; close current elscreen, or current window if only one elscreen
(define-key evil-normal-state-map ",Q" ":q!") ; close current window, *even if modified*
(define-key evil-normal-state-map ",d" 'kill-this-buffer) ; kill current buffer without confirmation
(define-key evil-normal-state-map ",D" 'kill-this-buffer-volatile) ; kill current buffer without confirmation, *even if modified*
(define-key evil-normal-state-map ",x" 'save-buffers-kill-emacs) ; save and quit
(define-key evil-normal-state-map ",u" 'undo-tree-visualize) ; see undo history in tree format (this will be opened in a new split window)
(define-key evil-normal-state-map ",z" 'suspend-emacs)
(define-key evil-insert-state-map [S-insert]
	(lambda () (interactive) (insert (x-selection 'PRIMARY)))) ; paste X primary
(define-key evil-normal-state-map "gw" 'fill-paragraph) ; insert hard line breaks


; navigation
; simulate vim's "nnoremap <space> 10jzz"
(define-key evil-normal-state-map " "
	(lambda ()
		(interactive)
		(next-line 10)
		(evil-scroll-line-to-center nil)
	)
)
; simulate vim's "nnoremap <backspace> 10kzz"
(define-key evil-normal-state-map (kbd "DEL")
	(lambda ()
		(interactive)
		(previous-line 10)
		(evil-scroll-line-to-center nil)
	)
)
(define-key evil-normal-state-map ",h"
	(lambda () (interactive) (split-window-vertically) (balance-windows)))
(define-key evil-normal-state-map ",v"
	(lambda () (interactive) (split-window-horizontally) (balance-windows)))
; move to next window
(define-key evil-normal-state-map (kbd "TAB") 'other-window)
; move to previous window
(define-key evil-normal-state-map [backtab]
	(lambda () (interactive) (other-window -1))
)
; Change K from being mapped to interactive man pages to being used as the
; vanilla comma ',' key's functionality (intra-line backwards search repeat for
; any t, T, f, F searches).
(define-key evil-normal-state-map "K" 'evil-repeat-find-char-reverse)
(define-key evil-visual-state-map "K" 'evil-repeat-find-char-reverse)
; buffer movement
(define-key evil-normal-state-map "H" 'evil-next-buffer)
(define-key evil-normal-state-map "L" 'evil-prev-buffer)
; new buffer
(define-key evil-normal-state-map ",n" 'vimlike-:tabe)
; find file
(define-key evil-normal-state-map ",e" 'ido-find-file)
; set line ending to UNIX
(define-key evil-normal-state-map ",E"
	(lambda () (interactive) (set-buffer-file-coding-system 'utf-8-unix t)))
; replace all /r/n with just /n
; make "kj" behave as ESC key, adapted from http://article.gmane.org/gmane.emacs.vim-emulation/980
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)

; Elscreen
; new vimlike "tab", aka "screen"
(define-key evil-normal-state-map ",N" 'elscreen-create)
; tab navigation
(define-key evil-normal-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-normal-state-map (kbd "C-h") 'elscreen-previous)
(define-key evil-insert-state-map (kbd "C-l") 'elscreen-next)
(define-key evil-insert-state-map (kbd "C-h") 'elscreen-previous)

; Nox integration (comment/uncomment regions)
(define-key evil-visual-state-map ",c"
	(lambda () (interactive) (my-addrem-comment t))) ; add comment
(define-key evil-visual-state-map ",C"
	(lambda () (interactive) (my-addrem-comment nil))) ; remove comment
(define-key evil-normal-state-map ",c"
  (lambda () (interactive) (my-addrem-comment t)))
(define-key evil-normal-state-map ",C"
  (lambda () (interactive) (my-addrem-comment nil)))

; org-mode
(evil-define-key 'normal org-mode-map (kbd "M-o")
	(lambda ()
		(interactive)
		(end-of-line)
		(insert "\n")
		(evil-append nil)
	)
)
(evil-define-key 'normal org-mode-map "O"
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-heading)
		(org-metaright)
		(evil-append nil)
	)
)
(evil-define-key 'normal org-mode-map "o"
	(lambda ()
		(interactive)
		(end-of-line)
		(always-insert-item)
		(evil-append nil)
	)
)

(evil-define-key 'normal org-mode-map "t"
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-todo-heading nil)
		(evil-append nil)
	)
)
(evil-define-key 'normal org-mode-map (kbd "M-t")
	(lambda ()
		(interactive)
		(end-of-line)
		(org-insert-todo-heading nil)
		(org-metaright)
		(evil-append nil)
	)
)
(evil-define-key 'normal org-mode-map (kbd "C-o") 'org-toggle-heading) ; convert a plain list into a heading
(evil-define-key 'normal org-mode-map "T" 'org-todo) ; mark a TODO item as DONE
(evil-define-key 'normal org-mode-map ";a" 'org-agenda) ; access agenda buffer
(evil-define-key 'normal org-mode-map "-" 'org-cycle-list-bullet) ; change bullet style

; allow us to access org-mode keys directly from Evil's Normal mode
; change item type
(evil-define-key 'normal org-mode-map (kbd "M-i") 'org-shiftright)
(evil-define-key 'normal org-mode-map (kbd "M-I") 'org-shiftleft)
; navigate on a per-item basis
(evil-define-key 'normal org-mode-map (kbd "M-p") 'org-shiftup)
(evil-define-key 'normal org-mode-map (kbd "M-n") 'org-shiftdown)
; heading-based navigation
(evil-define-key 'normal org-mode-map (kbd "M-l")
	'org-forward-heading-same-level)
(evil-define-key 'normal org-mode-map (kbd "M-h")
	'org-backward-heading-same-level)
(evil-define-key 'normal org-mode-map (kbd "M-k")
	'outline-previous-visible-heading)
(evil-define-key 'normal org-mode-map (kbd "M-j") 'outline-next-visible-heading)
; move items around, including child nodes
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)

(evil-define-key 'normal org-mode-map (kbd "TAB") 'other-window)
(evil-define-key 'normal org-mode-map [(control tab)] 'org-cycle)

(evil-define-key 'normal org-mode-map (kbd "<f12>") 'org-html-export-to-html)

(evil-define-key 'insert org-mode-map (kbd "M-TAB") 'org-shifttab)

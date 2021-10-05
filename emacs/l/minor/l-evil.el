(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (use-package undo-tree
    :config
    (global-undo-tree-mode))
  (evil-set-undo-system 'undo-tree)
  ; Prefer normal mode for all new buffers.
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-default-state 'normal)
  ; Cursor colors for the various states.
  ; TODO: Make these depend on the actual theme.
  (setq evil-insert-state-cursor '("#000000" box))
  (setq evil-emacs-state-cursor '("#ff0000" box))
  (setq evil-normal-state-cursor '("DodgerBlue1" box))
  (setq evil-visual-state-cursor '("#0000ff" box))
  ; Make the color in set-cursor-color be the default color recognized by Evil
  ; (for the non-customized states, such as visual mode). This is required because
  ; we change the color _after_ enabling evil.
  (setq evil-default-cursor t)
  ; Hide the cursor for inactive windows.
  (setq cursor-in-non-selected-windows nil)

  ; In vanilla Vim, pressing "o" on an indented line will insert leading
  ; indentation, and pressing ESC immediately will cancel the insertion of
  ; that indentation. We follow the spirit of that behavior here by marking,
  ; upon every insertion, whether the insertion was purely whitespace (such as
  ; the newline from "o"). This information is used by
  ; `l/undo-blank-insertion' and `l/open-line'.
  (setq l/before-open-line nil)
  (setq l/nonblank-inserted nil)
  (add-hook 'post-self-insert-hook 'l/check-for-nonblank-insertion)
  ; Aggressively undo any "o" or "O" command if we only enter whitespace.
  (add-hook 'evil-insert-state-exit-hook 'l/undo-blank-insertion)
  (l/define-key-args evil-normal-state-map "o" l/open-line nil)
  (l/define-key-args evil-normal-state-map "O" l/open-line t)

  ; Global Evil toplevel bindings (without a leader-key prefix, as in
  ; l-general.el). There are not that many; most of the more interesting
  ; keybindings are defined as part of major-mode's own keymaps.
  (define-key evil-motion-state-map (kbd "<f1>") 'menu-bar-mode)
  ; Make "kj" behave as ESC key, adapted from
  ; http://article.gmane.org/gmane.emacs.vim-emulation/980.
  (define-key evil-insert-state-map "k" #'l/maybe-exit)

  ; TAB to shift focus to the next window.
  (define-key evil-motion-state-map (kbd "TAB") 'other-window)
  ; S-TAB to shift focus to the previous window.
  (l/define-key-args evil-motion-state-map (kbd "<backtab>") other-window -1)

  ; Use C-j and C-k to shift focus to the next/prev window.
  (define-key evil-motion-state-map (kbd "C-j") 'other-window)
  (l/define-key-args evil-motion-state-map (kbd "C-k") other-window -1)
  (define-key evil-insert-state-map (kbd "C-j") 'other-window)
  (l/define-key-args evil-insert-state-map (kbd "C-k") other-window -1)
  ; Swap current window (and its buffer) with the next window.
  (define-key evil-motion-state-map (kbd "C-S-j") 'window-swap-states)
  (define-key evil-motion-state-map (kbd "C-S-k") '(lambda () (interactive)
    (other-window -1)
    (window-swap-states)
    (other-window -1)))
  (define-key evil-insert-state-map (kbd "C-S-j") 'window-swap-states)
  (define-key evil-insert-state-map (kbd "C-S-k") '(lambda () (interactive)
    (other-window -1)
    (window-swap-states)
    (other-window -1)))

  (l/define-key-args evil-motion-state-map (kbd "SPC") l/scroll-jump 10)
  (l/define-key-args evil-motion-state-map (kbd "DEL") l/scroll-jump -10)
  (l/define-key-args evil-normal-state-map (kbd "DEL") l/scroll-jump -10)
  (define-key evil-motion-state-map "H" 'evil-prev-buffer)
  (define-key evil-motion-state-map "L" 'evil-next-buffer)
  (define-key evil-motion-state-map (kbd "C-l") 'tab-next)
  (define-key evil-motion-state-map (kbd "C-h") 'tab-previous)
  (define-key evil-insert-state-map (kbd "C-l") 'tab-next)
  (define-key evil-insert-state-map (kbd "C-h") 'tab-previous)
  (l/define-key-args evil-motion-state-map (kbd "C-S-l") tab-bar-move-tab 1)
  (l/define-key-args evil-motion-state-map (kbd "C-S-h") tab-bar-move-tab -1)
  (l/define-key-args evil-insert-state-map (kbd "C-S-l") tab-bar-move-tab 1)
  (l/define-key-args evil-insert-state-map (kbd "C-S-h") tab-bar-move-tab -1)
  (define-key evil-normal-state-map (kbd "C-o") 'l/insert-newline-below)
  (define-key evil-normal-state-map (kbd "C-S-o") 'l/insert-newline-above)

  ; Make RET insert indentation after inserting a newline (noticeable when
  ; editing C files).
  (define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
  (define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)
  ; For all minor modes, make backspace behave like backspace in insert mode.
  (define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace)

  ; Paste X primary buffer.
  (l/define-key-args evil-normal-state-map (kbd "C-p") l/paste-X-primary nil)
  (l/define-key-args evil-normal-state-map (kbd "C-S-p") l/paste-X-primary-smart t)
  (l/define-key-args evil-insert-state-map (kbd "S-<insert>") l/paste-X-primary-smart nil)
  ; For Mac OSX laptops without the insert key.
  (l/define-key-args evil-insert-state-map (kbd "C-p") l/paste-X-primary-smart nil)

  ; UIM mode
  (define-key evil-insert-state-map (kbd "C-_") 'l/uim-mode)

  ; Interactive search-and-replace "bindings". These are not real bindings in
  ; the sense that they call functions --- this is a limitation of upstream's
  ; design of query-replace. See
  ; http://www.gnu.org/software/emacs/manual/html_node/elisp/Search-and-Replace.html.
  (define-key query-replace-map [return] 'act)
  (define-key query-replace-map "N" 'backup)
  (define-key query-replace-map "a" 'automatic)
  (define-key query-replace-map "e" 'edit-replacement)

  ; Parens in Vim normal mode bind to prev/forward sentence. We generally
  ; follow the "1-sentence-per-line" rule for long prose documents so these
  ; keybindings are basically useless. Perfect for jumping!
  (define-key evil-normal-state-map "(" 'evil-jump-backward)
  (define-key evil-normal-state-map ")" 'evil-jump-forward)

  ; Change K from being mapped to interactive man pages to being used as the
  ; vanilla comma ',' key's functionality (intra-line backwards search repeat
  ; for any t, T, f, F searches).
  (define-key evil-normal-state-map "K" 'evil-repeat-find-char-reverse)
  (define-key evil-visual-state-map "K" 'evil-repeat-find-char-reverse))

(defun l/open-line (above)
  (interactive)
  (setq l/before-open-line (kakapo-lc))
  (kakapo-open above))

; See
; http://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode.
(evil-define-command l/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let
      (
      ; wait 200 milliseconds
        (evt
          (read-event
            (format "Insert %c to exit insert state" ?j)
            nil
            0.2)))
      (cond
        ((null evt) (message ""))
        ((and (integerp evt) (char-equal evt ?j))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
        (t
          (setq unread-command-events
            (append unread-command-events (list evt))))))))

; Navigation.
(defun l/scroll-jump (cnt)
  "Scroll by CNT lines."
  (interactive)
  (forward-line cnt)
  (evil-scroll-line-to-center nil))

; Add newlines above/below, without going through kakapo-open. We have a hook
; that runs on exit of insert mode, which discards purely whitespace insertions,
; so this way we can insert whitespace (newlines) without worrying about the
; hook.
(defun l/insert-newline-below ()
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (insert "\n")
  (forward-line -1))
(defun l/insert-newline-above ()
  (interactive)
  (beginning-of-line)
  (insert "\n")
  (forward-line -1))

(defun l/check-for-nonblank-insertion ()
  (if (and
      (eq l/nonblank-inserted nil)
      (save-excursion
        (forward-char -1)
        (looking-at "[^[:space:]]")))
    (setq l/nonblank-inserted t)))

; Vim-like visual selection. Make the visual selection update the X primary
; buffer. This function's name clashes with the one from upstream, overwriting
; it.
(defun evil-visual-update-x-selection (&optional buffer)
  "Update the X selection with the current visual region."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (evil-visual-state-p)
          (fboundp 'x-select-text)
          (or (not (boundp 'ns-initialized))
            (with-no-warnings ns-initialized))
          (not (eq evil-visual-selection 'block)))
;      ; Vanilla evil, as of commit
;      ; 1def26dc9c4d084e766364eff2d9f3aa51613cf7.

;          (x-select-text (buffer-substring-no-properties
;                          evil-visual-beginning
;                          evil-visual-end))

;           ; from Hans-Peter Deifel's email on Nov 1 2012, to the
;           ; implementations-list@lists.ourproject.org mailing list
;           ; (http://permalink.gmane.org/gmane.emacs.vim-emulation/1715), which
;           ; makes visual selections from Evil update the X11 primary
;           ; selection; this version does not work with the latest Evil
;           ; 1def26dc9c4d084e766364eff2d9f3aa51613cf7 because of a "X selection
;           ; unavailable for this frame" error.

;      (x-set-selection 'PRIMARY
;        (buffer-substring-no-properties
;          evil-visual-beginning
;          evil-visual-end))
;      (setq x-last-selected-text-primary)

;      ; We explicitly use 'xsel' to update the primary selection; this
;      ; way, we can emulate Vim's behavior in both terminal and GUI emacs.
      (if (l/os "darwin")
        (call-process-region
          evil-visual-beginning
          evil-visual-end
          "pbcopy"
          nil
          0
          nil
          "-pboard" "general"
        )
        (call-process-region
          evil-visual-beginning
          evil-visual-end
          "xsel"
          nil
          0
          nil
          "--primary" "--input")))))))

; When yanking ('y' key) in terminal Emacs, copy into the X clipboard. As
; suggested from http://permalink.gmane.org/gmane.emacs.vim-emulation/2023, the
; blog post at
; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
; touches on this topic; here is a modified version of that blog post:
(unless (display-graphic-p)
  (when (getenv "DISPLAY")
    ; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        (if (l/os "darwin")
          (call-process-region
            evil-visual-beginning
            evil-visual-end
            "pbcopy"
            nil
            0
            nil
            "-pboard" "general")
          (call-process-region
            (point-min)
            (point-max)
            "xsel"
            nil
            0
            nil
            "--clipboard" "--input"))))
    ; Call back for when user pastes
    ; Idea from
    ; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    (defun xsel-paste-function()
      ; Find out what is current selection by xsel. If it is different
      ; from the top of the kill-ring (car kill-ring), then return
      ; it. Else, nil is returned, so whatever is in the top of the
      ; kill-ring will be used.
      (let
        ((xsel-output (shell-command-to-string
          (if (l/os "darwin")
            "pbpaste"
            "xsel --clipboard --output"))))
      (unless (string= (car kill-ring) xsel-output) xsel-output)))
    ; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)))

(defun l/paste-X-primary ()
  (interactive)
  (let
    (
      (xpribuf
        (if (display-graphic-p)
          (if (l/os "darwin")
            ; Mac has no concept of primary vs clipboard
            ; selection. Everything is just "clipboard".
            (x-get-selection 'CLIPBOARD)
            ; For Linux, use primary selection.
            (x-get-selection 'PRIMARY)))))
    (insert xpribuf)
    ; Prevent our 'undo usage of 'o' Insert state exit hook from undoing
    ; this paste.
    (setq l/before-open-line nil)))

(defun l/paste-X-primary-smart (backward)
  (interactive)
  (let
    (
      (old-state (if (evil-insert-state-p) "insert" "normal"))
      (pos (point))
      (line-max (save-excursion (end-of-line) (point)))
      (xpribuf (x-selection 'PRIMARY)))
    ; If what we want to paste has a newline in it, then we should paste it
    ; starting at the beginning of a line, not at point (which could be in
    ; the middle of a line on some text).
    (if (string-match "\n" xpribuf)
      (if backward
        (progn
          (evil-insert nil)
          (beginning-of-line)
          (l/paste-X-primary)
          (goto-char pos)
          (evil-first-non-blank))
        (progn
          (evil-append nil)
          (if (= line-max (point-max))
            (progn
              (end-of-line)
              (insert "\n"))
            (forward-line 1))
          (beginning-of-line)
          (l/paste-X-primary)
          (goto-char pos)
          (forward-line 1)
          (evil-first-non-blank)
          (forward-char 1)))
      (l/paste-X-primary))
    (if (string= old-state "normal")
      (evil-normal-state))
    (message nil)))

; The code here is concerned about interaction with kakapo-mode; but since it
; deals with Vimlike behavior of undoing whitespace-only insertions such as "o
; ESC", we leave it here.
(defun l/undo-blank-insertion ()
  (interactive)
  ; like Vim, remove whitespace if nothing was inserted
  (delete-trailing-whitespace)
  ; Every time we enter insert mode with `kakapo-open', we mark the current
  ; line with `l/before-open-line'. If all we did was just enter a bunch of
  ; whitespace, we keep undoing until we reach a point where `point' is on a
  ; line that matches `l/before-open-line'. The edge case we have to look out
  ; for is if we enter insert mode without `kakapo-open' --- in this case, it
  ; is important to check if `l/before-open-line' is set; if it is set, it
  ; means that we did use `kakapo-open', so it's safe to undergo the undo
  ; chain. Lastly, if all we want to do is enter a blank newline, simply use
  ; `kakapo-open' and then manually redo (because there will be nothing to
  ; 'undo' because of the while-loop of undos below) after exiting insert
  ; state, OR just don't use `kakapo-open' (e.g., ESC -> A -> enter -> ESC).
  (if (and
      (not l/nonblank-inserted)
      l/before-open-line)
    (progn
      (while (not (string= (kakapo-lc) l/before-open-line))
        (undo-tree-undo))
      ; We match Vim's behavior. In Vim, if you press "o" or "O" and
      ; then immediately do ESC, and undo, point is exactly where it
      ; was. For Evil, point is actually "consistent" because it is
      ; placed one character *before* where point was, much like how
      ; repeatedly pressing "i" (for insert mode) and ESC results in
      ; the cursor slowly moving backward one character at a time. To
      ; counter this, we manually move our point over one character.
      (forward-char)))
  (setq l/before-open-line nil)
  (setq l/nonblank-inserted nil))

(provide 'l-evil)

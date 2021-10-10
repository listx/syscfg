;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! iedit
  :when (featurep! :completion vertico)
  :defer t
  :init
  ;; Fix conflict with embark.
  (setq iedit-toggle-key-default nil))

; Enable `CSI u` support. See https://emacs.stackexchange.com/a/59225.
; xterm with the resource ?.VT100.modifyOtherKeys: 1
; GNU Emacs >=24.4 sets xterm in this mode and define
; some of the escape sequences but not all of them.
; xterm with the resource ?.VT100.modifyOtherKeys: 1
; GNU Emacs >=24.4 sets xterm in this mode and define
; some of the escape sequences but not all of them.
(defun character-apply-modifiers (c &rest modifiers)
  "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
  (if (memq 'control modifiers) (setq c (if (or (and (<= ?@ c) (<= c ?_))
                                                (and (<= ?a c) (<= c ?z)))
                                            (logand c ?\x1f)
                                          (logior (lsh 1 26) c))))
  (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
  (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
  (vector c))
(defun l/eval-after-load-xterm ()
  (interactive)
  (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
    (let ((c 32) (uppercase 65))
      ; Create bindings for all ASCII codepoints from 32 (SPACE) to 126 (~).
      ; That is, make Emacs understand what these `CSI u' sequences mean.
      (while (<= c 126)
        (mapc (lambda (x)
                (define-key xterm-function-map (format (car x) c)
                  (apply 'character-apply-modifiers c (cdr x))))
              '(("\e\[%d;3u" meta)
                ("\e\[%d;5u" control)
                ("\e\[%d;6u" control shift)
                ("\e\[%d;7u" control meta)
                ("\e\[%d;8u" control meta shift)))
        (setq c (1+ c)))
      ; Interpret the C-S-<letter> sequences encoded as `CSI u' sequences,
      ; (e.g., "\e\[76;6u" (C-S-L) as C-S-l). This is because in Alacritty we
      ; always use the uppercase ASCII letter for the codepoint between the `['
      ; and `;' delimiters of the sequence. This use of 6u instead of 5u
      ; somewhat deviates from the example for "A" vs "a" as described in
      ; http://www.leonerd.org.uk/hacks/fixterms/, because there they recommend
      ; "\e\[65;5u" (note the 5u instead of the 6u) to encode C-S-A. The reason
      ; we use 6u instead of the recommended 5u is because for some reason we
      ; cannot get 5u to work with tmux. That is, if we pass in "\e\[76;5u" from
      ; Alacritty to tmux, tmux encodes it as C-l instead of C-S-l. So instead
      ; we feed in the 6u variant from Alacritty, which tmux does recognize as
      ; C-S-l. And then we tell tmux to convert all such C-S- sequences back
      ; into a `CSI u' sequence, again using the 6u variant for Emacs to
      ; consume. We could probably use the 5u variant from within Emacs but
      ; using the 6u variant keeps all settings consistent across alacritty,
      ; tmux, and emacs.
      ;
      ; Anyway, now in terminal emacs we can distinguish between C-S-l and C-l.
      ;
      ; The (+ 32 uppercase) expression shifts the uppercase codepoint up by 32,
      ; making it lowercase for Emacs.
      ;
      ; See https://emacs.stackexchange.com/a/59225 for the idea. Note that this
      ; code does not match the answer there because it also depends on how you
      ; set up your Alacritty bindings; if you make Alacritty send lowercase
      ; ASCII letters (a-z) instead of uppercase ones (A-Z) then this hack
      ; probably is not necessary.
      (while (<= uppercase 90)
        (mapc (lambda (x)
                (define-key xterm-function-map (format (car x) uppercase)
                  (apply 'character-apply-modifiers (+ 32 uppercase) (cdr x))))
              '(("\e\[%d;6u" control shift)
                ("\e\[%d;8u" control meta shift)))
        (setq uppercase (1+ uppercase)))

      ; Tab
      ;
      ; The conflicts with TAB and C-i are due to tmux, which has a regression
      ; that treats these keys the same, even if our terminal uses `CSI u' mode
      ; to disambiguate them. See https://github.com/tmux/tmux/issues/2705. If
      ; it did not have that regression we would be able to bind TAB and C-i
      ; freely without restrictions.
      ;
      ; We cannot read M-S-Tab (it gets read as C-M-i).
      ;(define-key xterm-function-map "\e\[9;4u"
      ;  (apply 'character-apply-modifiers 9 '(meta shift)))         ; M-S-TAB
      (define-key xterm-function-map "\e\[9;5u"
        (apply 'character-apply-modifiers 9 '(control)))            ; C-TAB
      (define-key xterm-function-map "\e\[9;6u"
        (apply 'character-apply-modifiers 9 '(control shift)))      ; C-S-TAB
      ; We cannot read C-M-Tab (it gets read as C-M-i).
      ;(define-key xterm-function-map "\e\[9;7u"
      ;  (apply 'character-apply-modifiers 9 '(control meta)))       ; C-M-TAB
      ; We cannot read C-M-S-Tab (it gets read as C-M-S-i).
      ;(define-key xterm-function-map "\e\[9;8u"
      ;  (apply 'character-apply-modifiers 9 '(control meta shift))) ; C-M-S-TAB

      ; Backspace (DEL)
      (define-key xterm-function-map "\e\[127;3u"
        (apply 'character-apply-modifiers 127 '(meta)))               ; M-DEL
      (define-key xterm-function-map "\e\[127;4u"
        (apply 'character-apply-modifiers 127 '(meta shift)))         ; M-S-DEL
      (define-key xterm-function-map "\e\[127;5u"
        (apply 'character-apply-modifiers 127 '(control)))            ; C-DEL
      (define-key xterm-function-map "\e\[127;6u"
        (apply 'character-apply-modifiers 127 '(control shift)))      ; C-S-DEL
      (define-key xterm-function-map "\e\[127;7u"
        (apply 'character-apply-modifiers 127 '(control meta)))       ; C-M-DEL
      (define-key xterm-function-map "\e\[127;8u"
        (apply 'character-apply-modifiers 127 '(control meta shift))) ; C-M-S-DEL

      ; Enter (RET)
      (define-key xterm-function-map "\e\[13;3u"
        (apply 'character-apply-modifiers 13 '(meta)))               ; M-RET
      (define-key xterm-function-map "\e\[13;4u"
        (apply 'character-apply-modifiers 13 '(meta shift)))         ; M-S-RET
      (define-key xterm-function-map "\e\[13;5u"
        (apply 'character-apply-modifiers 13 '(control)))            ; C-RET
      (define-key xterm-function-map "\e\[13;6u"
        (apply 'character-apply-modifiers 13 '(control shift)))      ; C-S-RET
      (define-key xterm-function-map "\e\[13;7u"
        (apply 'character-apply-modifiers 13 '(control meta)))       ; C-M-RET
      (define-key xterm-function-map "\e\[13;8u"
        (apply 'character-apply-modifiers 13 '(control meta shift)))))) ; C-M-S-RET

(eval-after-load "xterm" '(l/eval-after-load-xterm))

(setq user-full-name "Linus Arver"
      user-mail-address "linusarver@gmail.com")

(setq doom-theme 'zenburn)

(map! :after evil-org
      :map evil-org-mode-map
      :mnv "M-k" #'org-backward-element
      :mnv "M-j" #'org-forward-element
      :mnv "M-h" #'org-up-element
      :mnv "M-l" #'org-down-element
      :mnv "M-K" #'org-metaup
      :mnv "M-J" #'org-metadown
      :mnv "M-H" #'org-shiftmetaleft
      :mnv "M-L" #'org-shiftmetaright)

(setq org-directory "~/lo/note/")

(setq display-line-numbers-type nil)

(use-package! evil-escape
  :config
  (setq evil-escape-key-sequence "kj"))
(map! :after evil-org
      :map evil-org-mode-map
      :m "gk" #'evil-previous-visual-line
      :m "gj" #'evil-next-visual-line)

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(map! :n "S" #'evil-snipe-s)
(map! :leader :desc "help" "H" help-map)
(map! :leader
      :desc "notes"
      "N" doom-leader-notes-map)

(map! :m "SPC" (cmd!! #'l/scroll-jump 10)
      :mn "DEL" (cmd!! #'l/scroll-jump -10))
(defun l/scroll-jump (cnt)
  "Scroll by CNT lines."
  (interactive "p")
  (forward-line cnt)
  (evil-scroll-line-to-center nil))
(map! :m "H" #'previous-buffer
      :m "L" #'next-buffer)

(map! :leader
      :desc "split-h" "h" #'split-window-vertically
      :desc "split-v" "v" #'split-window-horizontally)
(map! :after org
      :map org-mode-map
      "|" nil)
(map! :after evil
      :map evil-normal-state-map
      "=" nil
      :map evil-motion-state-map
      "-" #'enlarge-window
      "_" #'shrink-window
      "+" #'balance-windows
      "\\" #'enlarge-window-horizontally
      "|" #'shrink-window-horizontally)
(map! :leader
      :desc "quit/session" "Q" doom-leader-quit/session-map
      :desc "l/quit-buffer" "q" #'l/quit-buffer)
(defun l/quit-buffer ()
  "Tries to escape the current buffer by closing it (or moving to a
non-auxiliary buffer if possible). Calls `l/gc-views' to handle any sort of
window management issues."
  (interactive)
  (let*
    (
      (original-bufname (buffer-name))
      (aux-buffer-rgx "^ *\*.+\*$")
      (is-aux-buffer (l/buffer-looks-like original-bufname '("^ *\*.+\*$")))
      (buffers (mapcar 'buffer-name (buffer-list)))
      (primary-buffers-count
        (length
          (seq-filter
            '(lambda (bufname) (not (string-match "^ *\*.+\*$" bufname)))
            buffers)))
      (primary-buffer-exists (> primary-buffers-count 0))
    )

    ; If we're on a magit-controlled buffer, do what magit expects and simulate
    ; pressing C-c C-c (with-editor-finish).
    (catch 'my-catch
      (progn
        (if (bound-and-true-p with-editor-mode)
          (if (buffer-modified-p)
            ; If there are any unsaved changes, either discard those changes or do
            ; nothing.
            (if (y-or-n-p "l/quit-buffer: Invoke (with-editor-cancel) to cancel the editing of this buffer?")
              (with-editor-cancel t)
              ; Use catch/throw to stop execution.
              (throw 'my-catch (message "l/quit-buffer: Aborting (doing nothing).")))
            (with-editor-finish t)))
        ; Close the current view (or exit the editor entirely), but only if we
        ; originally tried to close a non-"auxiliary" buffer. An "auxiliary"
        ; buffer is any buffer that is created in support of another major
        ; buffer. For example, if we open buffer "A", but then run `M-x
        ; describe-function' so that we're on a "*Help*" buffer, do NOT close
        ; the view (and exit emacs). In other words, such "auxiliary" buffers,
        ; when we want to quit from them, we merely want to just switch over to
        ; a primary (non-auxiliary) buffer.
        ;
        ; If we *only* have auxiliary buffers, then of course just quit.
        (if (and is-aux-buffer primary-buffer-exists)
          ; Cycle through previous buffers until we hit a primary
          ; (non-auxiliary) buffer.
          (progn
            (catch 'buffer-cycle-detected
              (while
                (string-match "^ *\*.+\*$" (buffer-name))
                ; Break loop if somehow our aux-buffer-rgx failed to account for all
                ; hidden/aux buffers and we are just looping over and over among the
                ; same list of actual auxiliary buffers.
                (if (string= original-bufname (buffer-name))
                  (throw 'buffer-cycle-detected
                    (message "l/quit-buffer: Buffer cycle detected among auxiliary buffers; invoking `l/gc-views'."))
                  (previous-buffer))))
              ; If we've broken the loop (due to a cycle), run (l/gc-views) as
              ; it is better than doing nothing.
              (l/gc-views))
          (l/gc-views))))))

; Either close the current window, or if only one windw, use the ":q" Evil
; command; this simulates the ":q" behavior of Vim when used with tabs to
; garbage-collect the current "view".
(defun l/gc-views ()
  "Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab."
  (interactive)
  (let
    ( (one-tab (= 1 (length (tab-bar-tabs))))
      (one-window (one-window-p)))
    (cond
      ; If current tab has split windows in it, close the current live
      ; window.
      ((not one-window) (delete-window) nil)
      ; If there are multiple tabs, close the current one.
      ((not one-tab) (tab-bar-close-tab) nil)
      ; If there is only one tab, just try to quit (calling tab-bar-close-tab
      ; will not work, because if fails if there is only one tab).
      (one-tab
        (progn
          ; When closing the last frame of a graphic client, close everything we
          ; can. This is to catch graphical emacsclients that do not clean up
          ; after themselves.
          (if (display-graphic-p)
            (progn
              ; Minibuffers can create their own frames --- but they can linger
              ; around as an invisible frame even after they are deleted. Delete all
              ; other frames whenever we exit from a single visible daemon frame,
              ; because there is no point in keeping them around. If anything they
              ; can hinder detection of "is there a visible frame?" logic from the
              ; shell.
              (delete-other-frames)
              ; While we're at it, also close all buffers, because it's annoying to
              ; have things like Helm minibuffers and the like sitting around.
              (mapc
                'kill-buffer
                (seq-filter
                  (lambda (bufname)
                    (not (l/buffer-looks-like bufname
                      '(
                      ; Do not delete buffers that may be open which are for git
                      ; rebasing and committing. This is in case these buffers
                      ; are open in other clients which may still be working on
                      ; these buffers.
                      "^COMMIT_EDITMSG"
                      "^git-rebase-todo"
                      ; This catches buffers like 'addp-hunk-edit.diff' which is
                      ; used during surgical edits of what to stage ('e' option
                      ; to the 'git add -p' command).
                      ".*hunk-edit.diff"
                      ; Don't delete system buffers buffers.
                      "^\*Messages\*"))))
                  (mapcar 'buffer-name (buffer-list))))))
          (evil-quit)) nil))))

(defun l/buffer-looks-like (bufname regexes)
  "Return t if the buffer name looks like any of the given regexes."
  (interactive)
  (eval (cons 'or (mapcar
    (lambda (rgx) (string-match rgx bufname)) regexes))))
(map! :after evil-org
      :map evil-org-mode-map
      ;; The org lang module (doom's module) has some arcane bindings which we
      ;; have to undo by pulling some teeth out. This includes undoing the
      ;; CSdown and CSup bindings which silently map to C-S-j and C-S-k,
      ;; respectively.
      :ni "C-S-k" nil
      :ni "C-S-j" nil)
(map! :imnv "C-j" (cmd!! #'other-window 1)
      :imnv "C-k" (cmd!! #'other-window -1)
      :imnv "C-S-j" #'window-swap-states
      :imnv "C-S-k" #'l/swap-window-states)

(defun l/swap-window-states () (interactive)
  (other-window -1)
  (window-swap-states)
  (other-window -1))

(setq tab-bar-show t
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-separator (propertize " " 'font-lock-face '(:background "color-16"))
      tab-bar-tab-name-function #'l/get-tab-name)

; Based on `tab-bar-tab-name-current-with-count', with some tweaks.
(defun l/get-tab-name ()
  "Generate tab name from the buffer of the selected window.
Also add the number of windows in the window configuration."
  (interactive)
  (let ((count (length (window-list-1 nil 'nomini)))
        (name (window-buffer (minibuffer-selected-window))))
    (if (> count 1)
        (format " [%d] %s " (- count 1) name)
        (format " %s " name))))
(map! :mi "C-l" #'tab-next
      :mi "C-h" #'tab-previous
      :mi "C-S-l" (cmd!! #'tab-bar-move-tab 1)
      :mi "C-S-h" (cmd!! #'tab-bar-move-tab -1))
(map! :leader :desc "tab-new" "n" #'tab-new)

(map! :leader :desc "window" "W" evil-window-map)
(map! :leader :desc "save-buffer" "w" #'save-buffer)
(map! :leader :desc "kill-buffer" "d" #'l/kill-this-buffer)
(map! :leader :desc "kill-buffer!" "D" #'l/kill-this-buffer!)
(defun l/kill-this-buffer ()
  "Kill current buffer."
  (interactive)
  (if (bound-and-true-p with-editor-mode)
    (with-editor-cancel t)
    (kill-this-buffer)))

(defun l/kill-this-buffer! ()
  "Kill current buffer even if it is modified."
  (interactive)
  (set-buffer-modified-p nil)
  (l/kill-this-buffer))

(map! :mi "C-o" #'l/insert-newline-below
      :mi "C-S-o" #'l/insert-newline-above)

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
(map! :after flycheck
      :leader :desc "flycheck" "F" flycheck-command-map)
(map! :after flycheck
      :map flycheck-command-map
      "n" #'l/flycheck-next-error
      "N" #'l/flycheck-prev-error)

(defun l/flycheck-next-error ()
  (interactive)
  (flycheck-next-error)
  (evil-scroll-line-to-center nil))
(defun l/flycheck-prev-error ()
  (interactive)
  (flycheck-previous-error)
  (evil-scroll-line-to-center nil))

(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-.*\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'"))

(custom-set-faces!
  '(tab-bar  :background "color-16")
  '(tab-bar-tab  :weight bold :box nil :foreground "color-16" :background "color-51")
  '(tab-bar-tab-inactive  :weight bold :box nil :foreground "color-16" :background "color-38"))

(custom-set-faces!
  ;; Fix ugly colors for diffs. Prevalent because of git comit message buffers
  ;; like COMMIT_EDITMSG.
  '(font-lock-comment-face  :foreground "#9fc59f")
  '(git-commit-summary  :foreground "#fff" :weight bold)
  '(diff-added        :foreground "green" :background "dark green")
  '(diff-removed      :foreground "red" :background "dark red")
  '(diff-context      :foreground "#ffffff")
  '(diff-header       :foreground "yellow" :background "#3f3f3f" :weight bold)
  '(diff-file-header  :foreground "yellow" :background "#3f3f3f" :weight bold)
  '(diff-hunk-header  :foreground "cyan"   :background "#3f3f3f")
  '(git-commit-keyword  :foreground "#dcdccc" :background "#3f3f3f"))
;; Enable soft word-wrap almost everywhere (including elisp).
(+global-word-wrap-mode +1)

; Enable only left-side fringe.
(set-fringe-mode '(10 . 0))

; Disable hl-line mode, because it is extremely slow. We want to use hl-line+
; mode instead, which is much faster because it only highlights the line when
; idle.
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(use-package! hl-line+
  :config
  (custom-set-faces! '(hl-line :background "grey32"))
  ; Highlight the current cursor line; set overlay to a high number to override
  ; other properties (e.g., mmm-default-submode-face).
  (setq hl-line-overlay-priority (/ most-positive-fixnum (expt 2 55)))
  ; Only highlight when idle.
  (toggle-hl-line-when-idle)
  (setq global-hl-line-mode nil)
  (hl-line-when-idle-interval 0.5))

(use-package! vim-empty-lines-mode
  :config
  (add-hook 'org-mode-hook 'vim-empty-lines-mode)
  (add-hook 'prog-mode-hook 'vim-empty-lines-mode)
  (add-hook 'text-mode-hook 'vim-empty-lines-mode)
  (custom-set-faces! '(vim-empty-lines-face :weight bold)))

; Modeline colors.
(custom-set-faces!
 '(mode-line
   :background "color-235"
   :foreground "color-231")
 '(mode-line-inactive
   :weight bold
   :background "color-16"
   :foreground "color-245"))

; Dim buffers in inactive windows to make the current one "pop".
(use-package! auto-dim-other-buffers
 :config
 (auto-dim-other-buffers-mode)
 (custom-set-faces! '(auto-dim-other-buffers-face :foreground "color-250" :background "color-234")))

; Always enable the tab bar, even if there is just one buffer showing (such as
; when we open a single buffer).
(tab-bar-mode)

; Use bright visuals for coloring regions and interactive search hits.
(custom-set-faces!
 '(lazy-highlight  :foreground "pink" :background "dark red" :weight normal)
 '(isearch  :foreground "dark red" :background "pink" :weight bold)
 '(region  :foreground "dark red" :background "pink" :weight bold))

(map! :after (git-gutter magit)
      :map doom-leader-git-map
      ; BUG: For some reason the "hunk" description does not show up in which-key.
      (:prefix-map ("h" . "hunk")
       "n" #'l/git-gutter:next-hunk
       "N" #'l/git-gutter:prev-hunk
       "r" #'git-gutter:revert-hunk
      ; "s" to mean "show hunk"
       "s" #'git-gutter:popup-hunk))

(defun l/git-gutter:next-hunk ()
  (interactive)
  (git-gutter:next-hunk 1)
  (evil-scroll-line-to-center nil))
(defun l/git-gutter:prev-hunk ()
  (interactive)
  (git-gutter:previous-hunk 1)
  (evil-scroll-line-to-center nil))

(use-package! git-gutter
  :after (hl-line+)
  :config
  ; Git diff +/- marks.
  (global-git-gutter-mode +1)
  ; Update git-gutter every time we lose/regain focus of the current window.
  ; This is to catch cases where we are SSH-ed in to a machine and are running
  ; emacs in terminal mode, which doesn't get the same "frame" focus signals as
  ; above because there is literally no frame.
  (defun l/git-gutter-refresh (orig-fun &rest args)
    (prog1
        (apply orig-fun args)
      (git-gutter:update-all-windows)))
  (advice-add 'select-window :around #'l/git-gutter-refresh)
  ; Make git-gutter refresh based on a timer (abuse the fact that
  ; hl-line-highlight-now is called whenever we're idle).
  (advice-add 'hl-line-highlight-now :around #'l/git-gutter-refresh)
  ; Update git-gutter every time we lose/regain focus to the frame. See
  ; https://emacs.stackexchange.com/a/60971/13006.
  (add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (git-gutter:update-all-windows))))
  (custom-set-faces!
  '(git-gutter:modified :foreground "#d0d")
  '(git-gutter:added :foreground "#0d0")
  '(git-gutter:deleted :foreground "#d00"))
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign " ")
  (setq git-gutter:deleted-sign " "))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

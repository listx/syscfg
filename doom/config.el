;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! iedit
  :when (featurep! :completion vertico)
  :defer t
  :init
  ;; Fix conflict with embark.
  (setq iedit-toggle-key-default nil))
(use-package! company
  :config
  (setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
  (add-to-list '+company-backend-alist '(text-mode (:separate company-dabbrev company-yasnippet))))

;; Enable `CSI u` support. See https://emacs.stackexchange.com/a/59225.  xterm
;; with the resource ?.VT100.modifyOtherKeys: 1 GNU Emacs >=24.4 sets xterm in
;; this mode and define some of the escape sequences but not all of them.  xterm
;; with the resource ?.VT100.modifyOtherKeys: 1 GNU Emacs >=24.4 sets xterm in
;; this mode and define some of the escape sequences but not all of them.
(defun l/csi-u-support ()
  (interactive)
  (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
    (let ((c 32))
      ;; Create bindings for all ASCII codepoints from 32 (SPACE) to 126 (~).
      ;; That is, make Emacs understand what these `CSI u' sequences mean.
      (while (<= c 127)
        (mapc (lambda (x)
                (define-key xterm-function-map
                  ;; What the terminal sends.
                  (format (car x) c)
                  ;; The Emacs key event to trigger.
                  (apply 'l/char-mods c (cdr x))))
              '(("\x1b[%d;2u" S)
                ("\x1b[%d;3u" M)
                ("\x1b[%d;4u" M S)
                ("\x1b[%d;5u" C)
                ("\x1b[%d;6u" C S)
                ("\x1b[%d;7u" C M)
                ("\x1b[%d;8u" C M S)))
        (setq c (1+ c)))

      ;; For C-{j-k} (e.g., "\x1b[106;5u" for C-j) and C-S-{j-k} (e.g.,
      ;; "\x1b[106;6u" for C-S-j), we have to bind things a bit differently
      ;; because Emacs's key event recognizes the character "10" as C-j. So If
      ;; we reference bindings with "C-j" elsewhere, such as using doom's `map!'
      ;; macro, Emacs expect a key event with character value 10, and not 105
      ;; ("j" character's ASCII value). We convert 105 to 10 by just masking the
      ;; lower 5 bits. Likewise, because the value itself (10) is already a
      ;; "control" character, there is no need to apply the control character
      ;; modifier itself, which is why they are missing in the list of bindings
      ;; below.
      ;;
      ;; We only bind keys that we use here. The keys that are not bound are
      ;; left alone, to leave them unmapped. This way, l-disambiguation-mode can
      ;; recognize those unbound keys properly.
      (setq special-keys '(?h ?j ?k ?l ?o))
      (while special-keys
        (setq c (car special-keys))
        (mapc (lambda (x)
                (define-key xterm-function-map
                  (format (car x) c)
                  (apply 'l/char-mods (logand c #b11111) (cdr x))))
              '(("\x1b[%d;5u")
                ("\x1b[%d;6u" S)
                ("\x1b[%d;7u" M)
                ("\x1b[%d;8u" M S)))
        (setq special-keys (cdr special-keys)))

      ;; Take care of `CSI u` encoding of special keys. These are:
      ;;
      ;; 9      TAB
      ;; 13     RET (Enter)
      ;; 27     ESC
      ;; 32     SPC
      ;; 64     @
      ;; 91     [
      ;; 127    DEL (Backspace)
      ;;
      ;; We don't bother with codes 32 64 91 127 because they're already taken
      ;; care of in the first loop above for the range 32-127.
      (setq special-keys '(9 13 27))
      (while special-keys
       (setq c (car special-keys))
       (mapc (lambda (x)
              (define-key xterm-function-map
                (format (car x) c)
                (apply 'l/char-mods c (cdr x))))
        '(("\x1b[%d;2u" S)
          ("\x1b[%d;3u" M)
          ("\x1b[%d;4u" M S)
          ("\x1b[%d;5u" C)
          ("\x1b[%d;6u" C S)
          ("\x1b[%d;7u" C M)
          ("\x1b[%d;8u" C M S)))
       (setq special-keys (cdr special-keys))))))

(eval-after-load "xterm" '(l/csi-u-support))
(defun l/disambiguate-problematic-keys ()
  "This doesn't really do anything special other than just create placeholder
bindings for as-yet-unbound keys (determined manually). If we don't do this then
running `describe-keys' on these bindings sometimes gives the wrong answer
because Emacs will equate these keys with other keys (e.g., C-i with C-S-i)."
  (interactive)

  ;; ASCII 9 (<TAB>)
  (l/bind-placeholder '(9 C))      ; C-TAB
  (l/bind-placeholder '(9 C S))    ; C-S-TAB
  (l/bind-placeholder '(9 C M))    ; C-M-TAB
  (l/bind-placeholder '(9 C M S))  ; C-M-S-TAB

  ;; Similar to TAB, don't mess with RET key for now.
  ;; ASCII 13 (Enter, aka <RET>)
  (l/bind-placeholder '(13 S))         ; S-RET
  (l/bind-placeholder '(13 M))         ; M-RET
  (l/bind-placeholder '(13 M S))       ; M-S-RET
  (l/bind-placeholder '(13 C))         ; C-RET
  (l/bind-placeholder '(13 C S))       ; C-S-RET
  (l/bind-placeholder '(13 C M))       ; C-M-RET
  (l/bind-placeholder '(13 C M S))     ; C-M-S-RET

  ;; ASCII 27 (0x1b, <ESC>)
  (l/bind-placeholder '(#x1b S))      ; S-ESC
  (l/bind-placeholder '(#x1b M S))    ; M-S-ESC
  (l/bind-placeholder '(#x1b C))      ; C-ESC
  (l/bind-placeholder '(#x1b C S))    ; C-S-ESC
  (l/bind-placeholder '(#x1b C M))    ; C-M-ESC
  (l/bind-placeholder '(#x1b C M S))  ; C-M-S-ESC

  ;; ASCII 64 ('@')
  (l/bind-placeholder '(64 C))

  ;; ASCII 91 ('[')
  ;; "[" key. Usually conflicts with Escape.
  ;; M-[ is already recognized correctly, so we don't do anything here. (That
  ;; is, there is no need to tweak the "\e[91;3u" binding already taken care
  ;; of with l/eval-after-load-xterm).
  (l/bind-placeholder '(91 M S))    ; M-S-[
  (l/bind-placeholder '(91 C))      ; C-[
  (l/bind-placeholder '(91 C S))    ; C-S-[
  (l/bind-placeholder '(91 C M))    ; C-M-[
  (l/bind-placeholder '(91 C M S))  ; C-M-S-[

  ;; ASCII 105 ('i')
  (l/bind-placeholder '(105 C))      ; C-i
  (l/bind-placeholder '(105 C S))    ; C-S-i
  (l/bind-placeholder '(105 C M))    ; C-M-i
  (l/bind-placeholder '(105 C M S))  ; C-M-S-i

  ;; C-j and C-S-j are already bound for window navigation.
  ;; C-M-j and C-M-S-j are already bound from tmux, so no point in binding them here (we'll never see them).

  ;; ASCII 109 ('m')
  (l/bind-placeholder '(109 C))     ; C-m
  (l/bind-placeholder '(109 C S))   ; C-S-m
  (l/bind-placeholder '(109 C M))   ; C-M-m
  (l/bind-placeholder '(109 C M S)) ; C-M-S-m

  ;; ASCII 127 (Backspace, aka <DEL>)
  (l/bind-placeholder '(127 M))      ; M-DEL
  (l/bind-placeholder '(127 M S))    ; M-S-DEL
  (l/bind-placeholder '(127 C))      ; C-DEL
  (l/bind-placeholder '(127 C S))    ; C-S-DEL
  (l/bind-placeholder '(127 C M))    ; C-M-DEL
  (l/bind-placeholder '(127 C M S)))  ; C-M-S-DEL

(defmacro l/bind-placeholder (binding)
  ; Note: The following are all basically equivalent:
  ;
  ;   (global-set-key (vector (logior (lsh 1 26) 105)) #'foo)
  ;   (global-set-key [#x4000069] #'foo)
  `(define-key l-disambiguation-mode-map
     (apply 'l/char-mods (car ,binding) (cdr ,binding))
     #'(lambda () (interactive)
         (message "[unbound] %s-%s (\x1b[%d;%du)"
                  (l/mods-to-string (cdr ,binding))
                  (single-key-description (car ,binding))
                  (car ,binding)
                  (l/mods-to-int (cdr ,binding))))))

(defun l/mods-to-int (ms)
  (let ((c 0))
   (if (memq 'C ms) (setq c (logior (lsh 1 2) c)))
   (if (memq 'M ms) (setq c (logior (lsh 1 1) c)))
   (if (memq 'S ms) (setq c (logior (lsh 1 0) c)))
   (+ 1 c)))

(defun l/mods-to-string (ms)
  (let ((s ""))
   (if (memq 'C ms) (setq s "C"))
   (if (memq 'M ms) (setq s (concat s (if (not (string= "" s)) "-") "M")))
   (if (memq 'S ms) (setq s (concat s (if (not (string= "" s)) "-") "S")))
   s))

; This is like character-apply-modifiers, but we don't do any special
; behind-the-scenes modification of the character.
(defun l/char-mods (c &rest modifiers)
  "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (C M S).
Return an event vector."
  (if (memq 'C modifiers) (setq c (logior (lsh 1 26) c)))
  (if (memq 'M modifiers) (setq c (logior (lsh 1 27) c)))
  (if (memq 'S modifiers) (setq c (logior (lsh 1 25) c)))
  (vector c))

(defvar l-disambiguation-mode-map (make-keymap) "Keymap for disambiguating keys in terminal Emacs.")
(define-minor-mode l-disambiguation-mode
   "A mode for binding key sequences so that we can see them with `M-x
  describe-key'."
  :global t
  :init-value nil
  :lighter " Disambiguation"
  ;; The keymap.
  :keymap l-disambiguation-mode-map)
(add-hook 'l-disambiguation-mode-on-hook 'l/disambiguate-problematic-keys)

;; Load xterm-specific settings for TERM=alacritty-xtermlike.
(add-to-list 'term-file-aliases '("alacritty-xtermlike" . "xterm-256color"))

(setq user-full-name "Linus Arver"
      user-mail-address "linusarver@gmail.com")

(setq doom-theme 'zenburn)

(map! :after dired
      :map dired-mode-map
      ;; "H" is by default bound to dired-do-hardlink.
      :mnv "H" #'previous-buffer
      ;; "L" is by default bound to dired-do-load.
      :mnv "L" #'next-buffer
      :mnv "h" #'dired-up-directory
      :mnv "l" #'dired-find-file)
(add-hook 'git-commit-setup-hook #'(lambda () (flyspell-mode -1)))
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

(after! org
  (add-to-list 'org-todo-keywords
               '(sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED" "OBSOLETE"))
  (add-hook 'org-mode-hook 'l/org-colors))

;; Dim org-block face (source code blocks) separately, because they are not
;; dimmed by default. Also dim org-hide as well.
(defun l/org-colors ()
  (add-to-list 'face-remapping-alist '(org-hide (:filtered (:window adob--dim t) (:foreground "#1c1c1c")) org-hide))
  (add-to-list 'face-remapping-alist '(org-block (:filtered (:window adob--dim t) (:background "#262626")) org-block)))

(setq org-directory
      (nth 0 (split-string (getenv "L_ORG_AGENDA_DIRS"))))
;; List of directories to use for agenda files. Each directory is searched
;; recursively.
(let*
  ((files (mapcan
           (lambda (dir) (directory-files-recursively dir "\\.org$"))
           (split-string (getenv "L_ORG_AGENDA_DIRS"))))
   (exclude-patterns (split-string (getenv "L_ORG_AGENDA_EXCLUDE_PATTERNS")))
   (reduced
     (seq-reduce
       (lambda (fs exclude-pattern)
         (seq-filter
           (lambda (f)
             (not (string-match-p (regexp-quote exclude-pattern) f)))
           fs))
       exclude-patterns
       files)))
  (setq org-agenda-files reduced))

;; Disable spellcheck.
(remove-hook 'org-mode-hook #'flyspell-mode)
(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      :mnv "H" #'previous-buffer
      :mnv "L" #'next-buffer)

; org-agenda: Add weekly review view.
; https://emacs.stackexchange.com/a/8163/13006
(setq org-agenda-custom-commands
  '(("w" "Weekly review"
    agenda ""
    (
      (org-agenda-span 'week)
      (org-agenda-start-with-log-mode '(closed clock state))
      (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'nottodo 'done))))
    ; Export as HTML.
    ("X" "Export HTML" agenda ""
      ((htmlize-head-tags (concat "    <meta"
        " http-equiv=\"refresh\""
        ; Refresh every 60 seconds.
        " content=\"60\""
        ">\n")))
      ("~/agenda.html"))))
(map! :after org-roam
      :map org-roam-mode-map
      :mnvi "C-k" nil
      :mnvi "C-j" nil)
(setq org-roam-directory (concat org-directory "/note"))

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
;(map! :m (apply 'l/char-mods 32 '(C M S)) (cmd!! #'l/scroll-jump 20))

(defun l/scroll-jump (cnt)
  "Scroll by CNT lines."
  (interactive "p")
  (forward-line cnt)
  (evil-scroll-line-to-center nil))
(map! :m "H" #'previous-buffer
      :m "L" #'next-buffer)
(after! vertico
  (map! :map vertico-map
         "S-DEL" #'l/vertico-directory-up))

;; Like vertico-directory-up, but always delete up to the nearest '/'.
(defun l/vertico-directory-up ()
  "Delete directory before point."
  (interactive)
  (save-excursion
    (goto-char (1- (point)))
    (when (search-backward "/" (minibuffer-prompt-end) t)
      (delete-region (1+ (point)) (point-max))
      t)))

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
      tab-bar-separator (propertize " " 'font-lock-face '(:background "#000000"))
      tab-bar-tab-name-function #'l/get-tab-name)

; Based on `tab-bar-tab-name-current-with-count', with some tweaks.
(defun l/get-tab-name ()
  "Generate tab name from the buffer of the selected window.
Also add the number of windows in the window configuration."
  (interactive)
  (let ((count (length (window-list-1 nil 'nomini)))
        (name (window-buffer (minibuffer-selected-window))))
    (if (> count 1)
        (format " ◩ %d %s " (- count 1) name)
        (format " %s " name))))
(map! :after evil-org
      :map evil-org-mode-map
      :ni "C-S-h" nil
      :ni "C-S-l" nil)
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
;; Use text-mode for scratch buffer.
(setq-default doom-scratch-initial-major-mode 'text-mode)

(custom-set-faces!
  '(vertical-border :background "#2e3330" :foreground "#2e3330")
  '(highlight-numbers-number  :weight bold)
  '(font-lock-builtin-face  :foreground "#ffcfaf")
  ;; Fix ugly colors for diffs. Prevalent because of git comit message buffers
  ;; like COMMIT_EDITMSG.
  '(git-commit-summary  :foreground "#ffffff" :weight bold)
  '(diff-added        :foreground "#00ff00" :weight bold)
  '(diff-removed      :foreground "#ff0000" :weight bold)
  '(diff-context      :foreground "#ffffff")
  '(diff-header       :foreground "#ffff00" :background "#3f3f3f" :weight bold)
  '(diff-file-header  :foreground "#ffff00" :background "#3f3f3f" :weight bold)
  '(diff-hunk-header  :foreground "#00ffff"   :background "#3f3f3f")
  '(git-commit-keyword  :foreground "#dcdccc" :background "#3f3f3f"))

(use-package! rainbow-mode
  :hook (prog-mode text-mode))
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

(defmacro l/custom-set-faces-matching! (regex &rest props)
  "Apply properties in bulk to all faces that match the regex."
  `(custom-set-faces!
    ,@(delq nil
       (mapcar (lambda (f)
                 (let ((s (symbol-name f)))
                   (when (string-match-p regex s)
                     `'(,f ,@props))))
               (face-list)))))

;; Make all doom-modeline-* faces have a uniform foreground, to make them easier
;; to read with our custom mode-line background. This way we don't have to spell
;; otu each font one at a time.
(use-package! doom-modeline
  :config
  (l/custom-set-faces-matching! "doom-modeline-" :foreground "#ccff94"))

; Modeline colors.
(custom-set-faces!
 '(mode-line
   :weight bold
   :background "#3f5f4f"
   :foreground "#ccff94")
 '(mode-line-inactive
   :background "#2e3330"
   :foreground "#86ab8e"))

(custom-set-faces!
  '(tab-bar  :background "#000000")
  '(tab-bar-tab  :inherit mode-line :weight bold :box nil)
  '(tab-bar-tab-inactive :inherit mode-line-inactive :box nil))

; Dim buffers in inactive windows to make the current one "pop".
(use-package! auto-dim-other-buffers
 :config
 (auto-dim-other-buffers-mode)
 (custom-set-faces! '(auto-dim-other-buffers-face :foreground "#bcbcbc" :background "#1c1c1c")))

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
   '(git-gutter:modified :foreground "#ff00ff")
   '(git-gutter:added :foreground "#00ff00")
   '(git-gutter:deleted :foreground "#ff0000"))
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign " ")
  (setq git-gutter:deleted-sign " "))

;; Highlight the entire expression when hovering over matching parentheses.
(use-package! highlight-sexp
  :hook ((prog-mode . highlight-sexp-mode))
  :config
  (custom-set-faces!
    '(hl-sexp-face :extend t))
  (setq hl-sexp-background-color "grey33"))

;; Useful for highlighting single sentences in plain text (because).
(use-package! focus
  :hook ((text-mode . focus-mode))
  :config
  (custom-set-faces!
    '(focus-focused :weight bold :extend t)
    '(focus-unfocused :inherit nil))
  (add-to-list 'focus-mode-to-thing '(python-mode . paragraph)))

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

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! company
  :config
  (setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
  (add-to-list '+company-backend-alist '(text-mode (:separate company-dabbrev company-yasnippet))))

(defun l/copy-to-clipboard (orig-fun string)
  "Copy killed text or region into the system clipboard, by shelling out to a
script which knows what to do depending on the environment."
  (let ((b64 (base64-encode-string (encode-coding-string string 'no-conversion) t)))
   (start-process-shell-command "copy" nil (format "printf %s | ~/syscfg/script/copy-clipboard.sh --base64" b64))
   (funcall orig-fun string)))

(advice-add 'gui-select-text :around #'l/copy-to-clipboard)
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

;; Load xterm-specific settings for TERM=wezterm.
(add-to-list 'term-file-aliases '("wezterm" . "xterm-256color"))

(setq user-full-name "Linus Arver"
      user-mail-address "linusarver@gmail.com")

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
      :map org-read-date-minibuffer-local-map
      "h" (cmd! (org-eval-in-calendar '(calendar-backward-day 1)))
      "l" (cmd! (org-eval-in-calendar '(calendar-forward-day 1)))
      "j" (cmd! (org-eval-in-calendar '(calendar-forward-week 1)))
      "k" (cmd! (org-eval-in-calendar '(calendar-backward-week 1)))
      "0" (cmd! (org-eval-in-calendar '(calendar-beginning-of-week 1)))
      "$" (cmd! (org-eval-in-calendar '(calendar-end-of-week 1)))
      "H" (cmd! (org-eval-in-calendar '(calendar-backward-month 1)))
      "L" (cmd! (org-eval-in-calendar '(calendar-forward-month 1)))
      "J" (cmd! (org-eval-in-calendar '(calendar-forward-month 2)))
      "K" (cmd! (org-eval-in-calendar '(calendar-backward-month 2)))
      :map evil-org-mode-map
      :mnv "M-k" #'org-backward-element
      :mnv "M-j" #'org-forward-element
      :mnv "M-h" #'org-up-element
      :mnv "M-l" #'org-down-element
      :mnv "M-K" #'org-metaup
      :mnv "M-J" #'org-metadown
      :mnv "M-H" #'org-shiftmetaleft
      :mnv "M-L" #'org-shiftmetaright)

(map! :after org
      :map org-mode-map
      :localleader
      (:prefix ("d" . "date/deadline")
         "t" #'l/org-insert-timestamp-inactive)
      (:prefix ("e" . "export")
        :desc "subtree (children only)" "s" (cmd! (l/org-export-as-markdown-to-clipboard nil))
        :desc "subtree (children + parent)" "S" (cmd! (l/org-export-as-markdown-to-clipboard 't))
        "d" #'org-export-dispatch))

(defun l/org-insert-timestamp-inactive ()
  (interactive)
  (org-time-stamp-inactive '(16)))
;; See https://emacs.stackexchange.com/a/22398/13006. Detect poorly-converted
;; links (those that have two or more parentheses, which can happen if we have
;; an elisp link).
;;
;; That is, if we have
;;
;;      [[elisp:(foo)][link-name]]
;;
;; in the raw orgmode text, the default Markdown export converts this to
;;
;;      [link-name]((foo))
;;
;; which is not what we want. So we detect any link that is defined in Markdown
;; with "((..." and if so, scrub the link location with an error message, so
;; that the above becomes
;;
;;      [link-name](MARKDOWN-LINK-EXPORT-ERROR)
;;
;; Note that links written as
;;
;;      [[elisp:foo][link-name]]
;;
;; which is valid for calling `foo` directly, won't be caught by this function
;; because it will get exported as
;;
;;      [link-name](foo)
;;
;; by the Markdown exporter, erasing information that the link was a broken
;; "elisp" type to begin with.
;;
;; In addition, unfortunately it appears that the input `link' can end in a
;; number of space characters. So we have to preserve these extraneous
;; characters as well (hence the second capture group).
(defun l/org-export-md-scrub-invalid-links (link backend info)
  "Scrub invalid Markdown links of the form `[LINK-NAME]((...)' with just LINK-NAME."
  (if (eq backend 'md)
    (replace-regexp-in-string "\\(\\[[^]]*\\]\\)((.+?)\\(\s*\\)$" "\\1(MARKDOWN-LINK-EXPORT-ERROR)\\2" link)
   link))
(after! ox
  (add-to-list 'org-export-filter-link-functions
                 'l/org-export-md-scrub-invalid-links))

(after! org
  (defun l/org-export-as-markdown-to-clipboard (include-parent-heading)
    "Like doom's +org/export-to-clipboard, but (1) always exports to markdown, (2)
  always processes only the current subtree around point, and (3) pipes to a
  hardcoded clipboard script to perform the copy. The unwind-protect stuff was
  copy/pasted from the example given at
  https://www.gnu.org/software/emacs/manual/html_node/elisp/Cleanups.html. It's
  interesting to see that doom has a slightly different version with
  (unwind-protect (with-current-buffer ...) (kill-buffer buffer))."
    (interactive)
    (require 'ox)
    (let* ((org-export-with-toc nil)
           (org-export-show-temporary-export-buffer nil)
           ;; If point is above the topmost heading, then export the whole buffer.
           (export-whole-buffer
            ;; If we don't use this if condition, the (save-excursion ...) will
            ;; always return a truthy value.
            (if (not (save-excursion (condition-case nil (org-back-to-heading) (error nil))))
                t
                nil))
           (async nil)
           (visible-only t)
           (body-only t)
           ; Temporary buffer to hold exported contents.
           (buffer (save-window-excursion
                     (cond (export-whole-buffer (org-export-to-buffer 'md "*Formatted Copy*" async nil visible-only body-only))
                           (include-parent-heading
                              (save-restriction
                                (org-narrow-to-subtree)
                                (org-export-to-buffer 'md "*Formatted Copy*" async nil visible-only body-only)))
                           (t (org-export-to-buffer 'md "*Formatted Copy*" async 't visible-only body-only))))))
      (with-current-buffer buffer
        (unwind-protect
          (let ((bufstr (buffer-string)))
               (if (= 0 (length bufstr))
                   (message "Nothing to copy.")
                   (progn
                     ;; Delete leading newline from org-export-to-buffer.
                     (goto-line 1)
                     (flush-lines "^$")
                     (call-shell-region (point-min) (point-max) "~/syscfg/script/copy-clipboard.sh" nil 0)
                     (message (concat "Exported children of subtree starting with `"
                                      (if (> (length bufstr) 20)
                                          (concat
                                           (string-trim-left
                                              (substring bufstr 0 20))
                                           "...")
                                        bufstr
                                       "' as Markdown into clipboard.")))
                     ;; "Kill" locally ("copy") into emacs. The word "kill" here
                     ;; is unfortunate because it is overloaded with the "kill" in
                     ;; "kill-buffer" below. Anyway we also send the buffer to an
                     ;; external "copy" program.
                     (kill-new (buffer-string)))))
          ;; Always make sure to kill (close) this temporary buffer.
          (kill-buffer buffer)))))
  ; Make calendars in agenda start on Monday.
  (setq calendar-week-start-day 1)
  (setq org-startup-indented t)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "IN-PROGRESS(i)"
           "WAITING(w)"
           "|"
           "DONE(d)"
           "CANCELED(c)"
           "OBSOLETE(o)")
          (sequence
           "ASK(a)"                     ; A question to ask
           "ASKED(e)"                   ; Question was asked, but we're waiting for them to respond
           "|"
           "ANSWERED(r)"))
        org-todo-keyword-faces
        '(("ASK"  . +org-todo-active)
          ("IN-PROGRESS" . +org-todo-active)
          ("WAITING" . +org-todo-onhold)
          ("ASKED" . +org-todo-onhold)
          ("ANSWERED"   . +org-todo-cancel)
          ("CANCELED"   . +org-todo-cancel)
          ("OBSOLETE" . +org-todo-cancel)))
  (add-hook 'org-mode-hook (lambda () (vim-empty-lines-mode -1)))
  (add-hook 'org-mode-hook 'l/org-colors))

;; Dim org-block face (source code blocks) separately, because they are not
;; dimmed by default. Also dim org-hide as well.
(defun l/org-colors ()
  (add-to-list 'face-remapping-alist `(org-hide (:filtered (:window adob--dim t) (:foreground ,l/color-xGrey1)) org-hide))
  (add-to-list 'face-remapping-alist `(org-block (:filtered (:window adob--dim t) (:background ,l/color-xGrey2)) org-block)))

(setq org-directory
      (nth 0 (split-string (getenv "L_ORG_AGENDA_DIRS"))))
;; List of directories to use for agenda files. Each directory is searched
;; recursively.
(defun l/reset-org-agenda-files ()
  (interactive)
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
    (setq org-agenda-files reduced)))
(l/reset-org-agenda-files)

;; Disable spellcheck.
(remove-hook 'org-mode-hook #'flyspell-mode)

;; See https://github.com/org-roam/org-roam/issues/2198#issuecomment-1208072780.
(setq org-fold-core-style "overlays")
(setq org-log-done 'note)
(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      :mnv "C-k" nil
      :mnv "C-j" nil
      :mnv "H" #'previous-buffer
      :mnv "L" #'next-buffer)

; Make a fast shortcut to show the agenda
(map! :leader :desc "org-agenda-list" "A" #'org-agenda-list)

; org-agenda: Add weekly review view.
; https://emacs.stackexchange.com/a/8163/13006
(setq org-agenda-custom-commands
      '(("w" "Weekly review"
         ((agenda ""))
         ((org-agenda-buffer-name "*REVIEW*")
          (org-agenda-span 15)
          (org-agenda-start-day "-15d")
          (org-agenda-start-with-log-mode '(closed clock state))
          (org-agenda-skip-function
           ;; Skip unfinished entries.
           '(org-agenda-skip-entry-if 'nottodo 'done))))
        ("c" "Composite view"
         ;; We only show P0 TODO items if the have been scheduled, and their
         ;; scheduled date is today or in the past. This way we only concern
         ;; ourselves with tasks that we can actually work on.
         ((tags "PRIORITY=\"0\""
                ((org-agenda-skip-function
                  '(or
                    ;; Skip entries if they haven't been scheduled yet.
                    (l/org-agenda-skip-if-scheduled-later)
                    ;; Skip entries if they are DONE (or CANCELED, etc).
                    (org-agenda-skip-entry-if 'todo 'done)))
                 (org-agenda-overriding-header "P0 tasks from today or the past")))
          ;; See 7 days from today. It's like the opposite of "Weekly review".
          (agenda ""
                  ((org-agenda-span 7)
                   (org-agenda-start-day "-0d")))
          ;; List all global TODO items that have not yet been scheduled or
          ;; deadlined.
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (l/org-skip-subtree-if-priority ?0)
                          (org-agenda-skip-if nil '(scheduled deadline)))))))
         ((org-agenda-buffer-name "*QUEUE*")
          (org-agenda-compact-blocks t)))
        ; Export as HTML.
        ("X" "Export HTML" agenda ""
          ((htmlize-head-tags
            (concat
             "    <meta"
             " http-equiv=\"refresh\""
                           ; Refresh every 60 seconds.
             " content=\"60\""
             ">\n")))
          ("~/agenda.html"))))

(defun l/org-agenda (key &optional open-in-new-tab)
  "Open customized org-agenda."
  (interactive)
  (let* ((bufname (cond
                   ((string= "c" key) "*QUEUE*")
                   ((string= "w" key) "*REVIEW*")
                   (t "*UNKNOWN AGENDA TYPE*")))
         (buf (get-buffer bufname)))
    (when open-in-new-tab (tab-bar-new-tab))
    ;; Avoid re-generating the buffer from scratch if we already generated one
    ;; earlier. This makes it fast.
    (if buf
        (switch-to-buffer buf)
        (org-agenda nil key))
    (org-agenda-redo)
    (message (concat
              "Opened agenda view `"
              key
              "' with bufname `"
              bufname
              "' and buffer `"
              (prin1-to-string buf)
              "'."))))

;; Adapted from https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html.
(defun l/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?0, ?1, or ?2."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

;; Adapted from https://emacs.stackexchange.com/a/29838/13006.
(defun l/org-agenda-skip-if-scheduled-later ()
 "If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (scheduled-seconds
            (time-to-seconds
              (org-time-string-to-time
                (org-entry-get nil "SCHEDULED"))))
          (now (time-to-seconds (current-time))))
       (and scheduled-seconds
            (>= scheduled-seconds now)
            subtree-end))))
(defun l/org-roam-goto-day (day)
  "Open a new tab and show the day's journal file."
  (interactive)
  (tab-bar-new-tab)
  (cond
   ((eq day 'yesterday) (org-roam-dailies-goto-tomorrow (- 1)))
   ((eq day 'today) (org-roam-dailies-goto-today))
   (t (org-roam-dailies-goto-tomorrow 1)))
  (goto-char (point-max)))

(defun l/org-roam-open-node (&optional initial-input)
  "Search for org-roam nodes and open in a new tab."
  (interactive)
  (let ((node (org-roam-node-read initial-input)))
    (if node (progn (tab-bar-new-tab) (org-roam-node-open node)))))

(defun l/org-roam-capture (key subdir)
  (interactive)
  (org-roam-capture nil key
                    :filter-fn (lambda (node) (string-equal subdir (org-roam-node-doom-type node)))))

(defun l/rg-search (dir pat &rest args)
  "Use rg-helper.sh to search DIR for pat. See rg-helper.sh for
details."
  (interactive)
  (let ((dir-expanded (expand-file-name dir)))
    (tab-bar-new-tab)
    (consult--grep "rg" (l/rg-search-mk-builder dir-expanded args) dir-expanded pat)))

(defun l/rg-search-mk-builder (dir args)
  "Returns a lambda that can fill in the `pat' variable. The explicit use of
`list' here is required because we want to do 2 levels of substitution. We
substitute in `dir' and `args' at the time we build the list. We then put in a
`backquote' so that the `pat' can be substituted in when the lambda is called by
consult--grep."
  (let ((invocation (append (list "rg-helper.sh" dir "all" ',pat) args)))
    (list 'lambda (list 'pat) (list 'backquote (list ':command invocation)))))

(defun l/rg-search-blocks (dir pat &rest args)
  "Use rg-helper.sh to search DIR for pat in org-mode's code blocks."
  (interactive)
  (tab-bar-new-tab)
  (consult--grep "rg(blocks)" (l/rg-search-blocks-mk-builder dir args) dir pat))

(defun l/rg-search-blocks-mk-builder (dir args)
  "Like l/rg-search-mk-builder, but uses rg-helper.sh's 'regions' mode to search
between begin_WORD ... end_WORD blocks."
  (let ((invocation (append (list "rg-helper.sh" dir "regions"
                             ',(concat "(\\x00)(\\d+):\\s*(?:(?!#\\+(begin|end)_\\w+?)).*?(" pat ")")
                             ',(concat "^\\s*#\\+begin_\\w+\[^\\n\]*$((?!^\\s*#\\+end_\\w+$).)*(" pat ").*?(?!^\\s*#\\+end_\\w+$)")) args)))
    (list 'lambda (list 'pat) (list 'backquote (list ':command invocation)))))
(defun l/org-roam-get-nearby-dailies ()
  "Return a list of absolute filenames of all dailies files from the current
  week."
  (interactive)
  (let ((journal-files (seq-filter
                        (lambda (f) (string-match "journal" f))
                        (org-agenda-files))))
    (seq-filter 'l/select-nearby-dailies journal-files)))

(defun l/select-nearby-dailies (f)
  "Given filename `f', determine if it has a substring that matches a date from
  `l/nearby-dates'."
  (let ((matches (mapcar (lambda (rgx) (string-match rgx f)) (l/nearby-dates))))
    (cl-some (lambda (m) m) matches)))

(defun l/nearby-dates ()
  "Return a list of dates near today."
  (mapcar (lambda (x) (org-read-date nil nil x))
          '("-7d"
            "-6d"
            "-5d"
            "-4d"
            "-3d"
            "-2d"
            "-1d"
            "0d"
            "+1d")))
(map! :after org-roam
      :map org-roam-mode-map
      :mnvi "C-k" nil
      :mnvi "C-j" nil)
(after! org-roam
  (setq
        org-roam-node-display-template
        (format "%s %s ${doom-hierarchy}"
                (propertize "${doom-type:20}" 'face 'font-lock-keyword-face)
                (propertize "${doom-tags:20}" 'face 'org-tag))))
(setq org-roam-directory (concat org-directory "/note/")
      org-roam-dailies-directory "journal/"
      org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<[%Y-%m-%d %a %H:%M]> %?"
         :empty-lines-before 1
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d %a>\n")))
      l/org-roam-default-template "#+title: ${title}\n\n* TODOs\n\n* Notes\n:PROPERTIES:\n:VISIBILITY: children\n:END:\n"
      ;; Would be nice to make point position itself after the same line as the
      ;; "TODO" heading itself. Currently we have to press Backspace twice to
      ;; finish setting up the capture template.
      l/org-roam-default-olp '("TODOs" "TODO")
      org-roam-capture-templates
      `(("r" "reference" plain
         "%?"
         :target (file+head+olp "reference/${slug}.org" ,l/org-roam-default-template ,l/org-roam-default-olp)
         :unnarrowed t)
        ("c" "creche" plain
         "%?"
         :target (file+head+olp "creche/${slug}.org" ,l/org-roam-default-template ,l/org-roam-default-olp)
         :unnarrowed t)
        ("p" "proj-temp" plain
         "%?"
         :target (file+head+olp "proj-temp/${slug}.org" ,l/org-roam-default-template ,l/org-roam-default-olp)
         :unnarrowed t)
        ("P" "proj-perm" plain
         "%?"
         :target (file+head+olp "proj-perm/${slug}.org" ,l/org-roam-default-template ,l/org-roam-default-olp)
         :unnarrowed t)
        ("x" "pub" plain
         "%?"
         :target (file+head+olp "pub/${slug}.org" ,l/org-roam-default-template ,l/org-roam-default-olp)
         :unnarrowed t)))
(map! :after alchemist
      :map alchemist-mode-map
      :mnvi "C-k" nil
      :mnvi "C-j" nil)
(after! sh-script
  (set-formatter! 'shfmt
    '("shfmt"
       "--binary-next-line"
       "--func-next-line"
      ("--indent" "%d" (unless indent-tabs-mode tab-width))
      ("--language-dialect" "%s" (pcase sh-shell (`bash "bash") (`mksh "mksh") (_ "posix"))))))

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
(require 'consult)

(defun l/split-window-vertically ()
  "Split window verically."
  (interactive)
  (split-window-vertically)
  (other-window 1))
(defun l/split-window-horizontally ()
  "Split window horizontally."
  (interactive)
  (split-window-horizontally)
  (other-window 1))
(map! :leader
      :desc "split-h" "h" #'l/split-window-vertically
      :desc "split-v" "v" #'l/split-window-horizontally)
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
      tab-bar-tab-name-function #'l/get-tab-name)

; Based on `tab-bar-tab-name-current-with-count', with some tweaks.
(defun l/get-tab-name ()
  "Generate tab name from the buffer of the selected window.
Also add the number of windows in the window configuration."
  (interactive)
  (let* ((count (length (window-list-1 nil 'nomini)))
         (buffer (window-buffer (minibuffer-selected-window)))
         (stylized-name (l/get-stylized-buffer-name buffer)))
    (if (> count 1)
        (format " ◩ %d %s " (- count 1) stylized-name)
        (format " %s " stylized-name))))

(defun l/get-stylized-buffer-name (buffer)
  "Return a stylized buffer name."
  (interactive)
  (let* ((bufname (buffer-name buffer))
         (bufname-short (string-remove-suffix ".org" bufname))
         (buf-date-match (string-match "^[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}$" bufname-short))
         (buf-is-date (eq 0 buf-date-match)))
    (cond ((string= bufname "dashboard.org") "DASHBOARD")
          (buf-is-date (l/append-relative-date-suffix bufname-short))
          (t bufname-short))))

(defun l/append-relative-date-suffix (date-str)
  ;; We use `org-time-stamp-to-now', but reverse the sign. This follows a simple
  ;; "number line" model where we have the present day at day "0", with old days
  ;; on the left (negative numbers) and future days on the right (positive
  ;; numbers).
  (let* ((day-diff (org-time-stamp-to-now date-str))
         (sign (if (< day-diff 0) "" "+"))
         (suffix (concat " [" sign (number-to-string day-diff) "]")))
   (cond ((= day-diff 0) (concat date-str " [TODAY]"))
         ((= day-diff 1) (concat date-str " [TOMORROW]"))
         ((= day-diff -1) (concat date-str " [YESTERDAY]"))
         (t (concat date-str suffix)))))
(map! :after evil-org
      :map evil-org-mode-map
      :ni "C-S-h" nil
      :ni "C-S-l" nil)
(map! :mi "C-l" #'tab-next
      :mi "C-h" #'tab-previous
      :mi "C-S-l" (cmd!! #'tab-bar-move-tab 1)
      :mi "C-S-h" (cmd!! #'tab-bar-move-tab -1))
(map! :leader :desc "tab-new" "n" (cmd!! #'tab-bar-new-tab 1))

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
  ;; Disable autoformatting of YAML files, because it can result in huge
  ;; indentation (whitespace) changes with no semantic difference.
  (setq lsp-yaml-format-enable nil)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-.*\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'"))
;; Use text-mode for scratch buffer.
(setq-default doom-scratch-initial-major-mode 'text-mode)

; Colors taken from PastelDark.dhall.
(setq l/color-text "#000000")
(setq l/color-cursor "#ffffff")
(setq l/color-background "#343c48")
(setq l/color-foreground "#e5e7ea")
(setq l/color-black "#22222f")
(setq l/color-red "#e49f9f")
(setq l/color-green "#91e380")
(setq l/color-yellow "#eae47c")
(setq l/color-blue "#7cacd3")
(setq l/color-magenta "#df9494")
(setq l/color-cyan "#8cdbd8")
(setq l/color-white "#e5e7ea")
(setq l/color-brightblack "#343c48")
(setq l/color-brightred "#e5bfbf")
(setq l/color-brightgreen "#afe0a1")
(setq l/color-brightyellow "#f2fb9e")
(setq l/color-brightblue "#95add1")
(setq l/color-brightmagenta "#f2b0b0")
(setq l/color-brightcyan "#b4f0f0")
(setq l/color-brightwhite "#ffffff")
(setq l/color-xAvocado "#3f5f4f")
(setq l/color-xBrightOrange "#ffcfaf")
(setq l/color-xDarkGreen "#2e3330")
(setq l/color-xGrey1 "#1c1c1c")
(setq l/color-xGrey2 "#262626")
(setq l/color-xLime "#ccff94")
(setq l/color-xMoss "#86ab8e")
(setq l/color-xUltraBrightGreen "#00ff00")
(setq l/color-xUltraBrightMagenta "#ff00ff")
(setq l/color-xUltraBrightRed "#ff0000")
(defmacro l/custom-set-faces-matching! (regex &rest props)
  "Apply properties in bulk to all faces that match the regex."
  `(custom-set-faces!
    ,@(delq nil
       (mapcar (lambda (f)
                 (let ((s (symbol-name f)))
                   (when (string-match-p regex s)
                     `'(,f ,@props))))
               (face-list)))))

(defun l/reset-faces ()
  (interactive)
  (setq tab-bar-separator (propertize " " 'font-lock-face `(:background ,(doom-darken (doom-color 'bg-alt) 0.2))))
  (custom-set-faces!
    `(vertical-border :background ,(doom-color 'base0) :foreground ,(doom-color 'base0))
    '(highlight-numbers-number  :weight bold)
    `(hl-line :background ,(doom-darken (doom-color 'bg-alt) 0.4))
    '(vim-empty-lines-face :weight bold)

    `(auto-dim-other-buffers-face :foreground ,(doom-color 'base8) :background ,(doom-darken (doom-color 'bg-alt) 0.6))
                                        ; Use bright visuals for coloring regions and interactive search hits.
    '(lazy-highlight  :foreground "pink" :background "dark red" :weight normal)
    '(isearch  :foreground "dark red" :background "pink" :weight bold)
    '(region  :foreground "dark red" :background "pink" :weight bold)

    `(tab-bar :background ,(doom-darken (doom-color 'bg-alt) 0.2))
    `(tab-bar-tab
       :background ,(doom-color 'base8)
       :foreground ,(doom-color 'base0)
       :weight bold
       :box nil)
    `(tab-bar-tab-inactive
       :background ,(doom-color 'base6)
       :foreground ,(doom-color 'base0)
       :box nil)

    `(mode-line
       :weight bold
       :background ,(doom-color 'base8)
       :foreground ,(doom-color 'base0))
    `(mode-line-inactive
       :background ,(doom-color 'base6)
       :foreground ,(doom-color 'base0))

    `(git-gutter:modified :foreground ,l/color-xUltraBrightMagenta)
    `(git-gutter:added :foreground ,l/color-xUltraBrightGreen)
    `(git-gutter:deleted :foreground ,l/color-xUltraBrightRed)
    ;; Fix ugly colors for diffs. Prevalent because of git comit message buffers
    ;; like COMMIT_EDITMSG.
    '(git-commit-summary  :foreground "brightwhite" :weight bold)
    '(diff-added        :foreground "#ccffcc" :background "#335533" :weight bold)
    '(diff-removed      :foreground "#ffcccc" :background "#553333" :weight bold)
    '(diff-context      :foreground "brightwhite")
    '(diff-function     :foreground "brightmagenta")
    '(diff-header       :foreground "#ffff00" :background "#555533" :weight bold)
    '(diff-file-header  :foreground "brightyellow")
    '(diff-hunk-header  :foreground "brightcyan")
    '(git-commit-keyword  :foreground "brightmagenta" :weight bold))

  ;; Make all doom-modeline-* faces have a uniform foreground, to make them easier
  ;; to read with our custom mode-line background. This way we don't have to spell
  ;; out each font one at a time.
  (eval `(l/custom-set-faces-matching! "doom-modeline-" :foreground ,(doom-color 'base0))))

(use-package! rainbow-mode
  :hook (prog-mode text-mode))
(use-package! doom-themes
  :config
  (cond
   ((string= "lo" (daemonp))
    (load-theme 'doom-one t))
   (t
    (load-theme 'doom-zenburn t)))
  (l/reset-faces))
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
  ; Only highlight when idle.
  (toggle-hl-line-when-idle)
  (setq global-hl-line-mode nil)
  (hl-line-when-idle-interval 0.5))

(use-package! vim-empty-lines-mode
  :config
  (add-hook 'prog-mode-hook 'vim-empty-lines-mode)
  (add-hook 'text-mode-hook 'vim-empty-lines-mode))

(use-package! doom-modeline
  :config
  (l/reset-faces))

; Dim buffers in inactive windows to make the current one "pop".
(use-package! auto-dim-other-buffers
 :config
 (auto-dim-other-buffers-mode))

; Always enable the tab bar, even if there is just one buffer showing (such as
; when we open a single buffer).
(tab-bar-mode)

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
      (when (frame-focus-state)
        (git-gutter:update-all-windows))))
  (advice-add 'select-window :around #'l/git-gutter-refresh)
  ; Make git-gutter refresh based on a timer (abuse the fact that
  ; hl-line-highlight-now is called whenever we're idle).
  (advice-add 'hl-line-highlight-now :around #'l/git-gutter-refresh)
  ; Update git-gutter every time we lose/regain focus to the frame. See
  ; https://emacs.stackexchange.com/a/60971/13006.
  (add-function :after after-focus-change-function (lambda () (when (frame-focus-state) (git-gutter:update-all-windows))))
  (setq git-gutter:modified-sign "█")
  (setq git-gutter:added-sign "█")
  (setq git-gutter:deleted-sign "█"))

;; Disable vertical bar cursor shape in terminal emacs.
(setq evil-motion-state-cursor 'box)
(setq evil-visual-state-cursor 'box)
(setq evil-normal-state-cursor 'box)
(setq evil-insert-state-cursor 'box)
(setq evil-emacs-state-cursor  'box)

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

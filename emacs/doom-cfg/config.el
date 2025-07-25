;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-



(defun l/copy-to-clipboard (orig-fun string)
  "Copy killed text or region into the system clipboard, by shelling out to a
script which knows what to do depending on the environment."
  (let ((b64
         (base64-encode-string (encode-coding-string string 'no-conversion) t)))
   (start-process-shell-command
    "copy" nil
    (format "printf %s | ~/syscfg/script/copy-clipboard.sh --base64" b64))
   (funcall orig-fun string)))

(advice-add 'gui-select-text :around #'l/copy-to-clipboard)
;; Enable `CSI u` support. See https://emacs.stackexchange.com/a/59225.
;;
;; xterm with the resource ?.VT100.modifyOtherKeys: 1
;; GNU Emacs >=24.4 sets xterm in this mode and define
;; some of the escape sequences but not all of them.
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
  ;; C-M-j and C-M-S-j are already bound from tmux, so no point in binding them
  ;; here (we'll never see them).

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

(defvar l-disambiguation-mode-map (make-keymap)
  "Keymap for disambiguating keys in terminal Emacs.")
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
      user-mail-address "linus@ucla.edu")

(map! :after dired
      :map dired-mode-map
      ;; "H" is by default bound to dired-do-hardlink.
      :mnv "H" #'previous-buffer
      ;; "L" is by default bound to dired-do-load.
      :mnv "L" #'next-buffer
      :mnv "h" #'dired-up-directory
      :mnv "l" #'dired-find-file)
(map! :after magit
      :map magit-mode-map
      ;; Remap C-{j,k} bindings.
      :mnvi "C-k" nil
      :mnvi "C-j" nil
      :mnvi "M-k" #'magit-section-backward
      :mnvi "M-j" #'magit-section-forward)
(map! :after magit
      :map global-map
      :leader
      :prefix ("g" . "git")
      (:prefix
       ("h" . "hunk")
       (:desc "goto next hunk"     "n" #'l/git-gutter:next-hunk)
       (:desc "goto previous hunk" "N" #'l/git-gutter:prev-hunk)
       (:desc "revert hunk"        "r" #'git-gutter:revert-hunk)
       (:desc "show hunk"          "s" #'git-gutter:popup-hunk))
      (:desc "Update and push"
             "u"
             (cmd! (start-process-shell-command
                    "update-and-push"
                    nil
                    "git add --update && git commit --message update && git push")))
      ;; Unbind the existing key.
      "y" nil
      (:prefix
       ("y" . "copy")
       (:desc "commit desc (Git ML style)" "d" (cmd! (l/copy-git 'commit-desc)))
       (:desc "commit message (raw)"       "m" (cmd! (l/copy-git 'commit-msg)))
       (:desc "commit SHA (raw)"           "s" (cmd! (l/copy-git 'commit-sha)))
       (:desc "file/region (URL)"          "y" (cmd! (l/copy-git 'file-url)))
       (:desc "commit (URL)"               "Y" (cmd! (l/copy-git 'commit-url)))))
(defun l/get-sha ()
  "Get Git SHA underneath point. Checks that the SHA is valid (that
the object exists locally)."
  (interactive)
  (when-let ((regex "\\([a-f0-9]+\\)")
             (sha (save-excursion
                    (skip-chars-backward "a-f0-9")
                    (looking-at regex)
                    (match-string-no-properties 1)))
             (sha-validated (magit-git-string "rev-parse" sha)))
    sha-validated))

(defun l/copy-git (mode)
  "Copy Git revision under point. Use `mode' to determine what to
 copy."
  (interactive)
  (let* ((sha (l/get-sha))
         (copytext
          (pcase mode
            ;; Copy commit SHA (40 chars).
            ('commit-sha sha)
            ;; Copy commit description (Git mailing list style).
            ('commit-desc
             (magit-git-string "show"
                               "--no-patch"
                               "--pretty=reference"
                               sha))
            ;; Copy the commit message. Useful for populating PR
            ;; descriptions.
            ('commit-msg
             (with-temp-buffer
               (magit-git-insert "cat-file" "commit" sha)
               (goto-char (point-min))
               ;; Go down 5 lines to skip the tree, parent,
               ;; author, committer, and blank line just before
               ;; the title.
               (forward-line 5)
               (buffer-substring-no-properties (point) (point-max))))
            ('commit-url (browse-at-remote--commit-url sha))
            ;; Copy a URL to the file (typically a GitHub link to the file).
            ;; If a region is active, highlight that region.
            ('file-url (browse-at-remote-get-url)))))
    (kill-new copytext)
    (message copytext)))
(after! magit
  (setq magit-repository-directories
      `(("~/prog" . 1)
        ("~/syscfg" . 0))))
(after! magit
  (setq git-commit-major-mode #'org-mode))
(defun l/git-commit-setup ()
  (interactive)
  (apheleia-mode -1)
  (envrc-mode -1)
  (flycheck-mode -1)
  (git-gutter-mode -1)
  (org-fancy-priorities-mode -1)
  (org-superstar-mode -1)
  (rainbow-mode -1)
  (smartparens-mode -1)
  (yas-minor-mode -1))
(after! magit
  (add-hook 'git-commit-setup-hook #'l/git-commit-setup))
(defun l/git-gutter:next-hunk ()
  (interactive)
  (git-gutter:next-hunk 1)
  (evil-scroll-line-to-center nil))
(defun l/git-gutter:prev-hunk ()
  (interactive)
  (git-gutter:previous-hunk 1)
  (evil-scroll-line-to-center nil))

(use-package! git-gutter
  :config
  ; Git diff +/- marks.
  (global-git-gutter-mode +1)
  ; Update the git-gutter automatically every second.
  (setq git-gutter:update-interval 1)
  (setq git-gutter:modified-sign "█")
  (setq git-gutter:added-sign "█")
  (setq git-gutter:deleted-sign "█"))
(map! :after evil-org
      :map evil-org-mode-map
      ;; Remove conflicting bindings.
      :i "C-j" nil
      :i "C-k" nil
      :i "C-h" nil
      :i "C-l" nil

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
      :mnv "M-S-k" #'org-metaup
      :mnv "M-S-j" #'org-metadown
      :mnv "M-S-h" #'org-shiftmetaleft
      :mnv "M-S-l" #'org-shiftmetaright
      :mnv "(" #'org-mark-ring-goto
      :i "C-RET" #'l/org-insert-thing)

(defun l/org-insert-thing ()
  "Insert the next thing, depending on context."
  (interactive)
  (cond ((org-in-item-p)    (org-insert-item))
        ((org-at-heading-p) (org-insert-heading))
        ((org-at-table-p)   (org-table-insert-row 1))
        (t                  (insert ?\n))))

(map! :after org
      :map org-mode-map
      :localleader
      (:prefix ("d" . "date/deadline")
         "t" #'l/org-insert-timestamp-inactive)
      (:prefix ("e" . "export")
        :desc "subtree (children only)" "s"
          (cmd! (l/org-export-as-markdown-to-clipboard nil))
        :desc "subtree (children + parent)" "S"
          (cmd! (l/org-export-as-markdown-to-clipboard 't))
        "d" #'org-export-dispatch)
      (:prefix ("g" . "goto")
         "b" #'org-babel-goto-named-src-block)
      (:prefix ("p" . "priority")
         :desc "Set priority to 0"
         "0" (cmd! (org-priority 0))

         :desc "Set priority to 1"
         "1" (cmd! (org-priority 1))

         :desc "Set priority to 2"
         "2" (cmd! (org-priority 2))

         :desc "Set priority to 3"
         "3" (cmd! (org-priority 3))

         :desc "Set priority to 4"
         "4" (cmd! (org-priority 4))))

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
  "Scrub invalid Markdown links of the form `[LINK-NAME]((...)' with just
LINK-NAME."
  (if (eq backend 'md)
    (replace-regexp-in-string
     "\\(\\[[^]]*\\]\\)((.+?)\\(\s*\\)$"
     "\\1(MARKDOWN-LINK-EXPORT-ERROR)\\2"
     link)
   link))
(after! ox
  (add-to-list 'org-export-filter-link-functions
                 'l/org-export-md-scrub-invalid-links))

(after! org
  (add-hook 'org-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode 1)))
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
           (org-export-with-smart-quotes nil)
           (org-export-with-special-strings nil)
           (org-export-with-fixed-width t)
           ;; If point is above the topmost heading, then export the whole buffer.
           (export-whole-buffer
            ;; If we don't use this if condition, the (save-excursion ...) will
            ;; always return a truthy value.
            (if (not (save-excursion
                       (condition-case nil (org-back-to-heading) (error nil))))
                t
                nil))
           (async nil)
           (visible-only nil)
           (body-only t)
           ; Temporary buffer to hold exported contents.
           (buffer (save-window-excursion
                     (cond (export-whole-buffer
                            (org-export-to-buffer
                                'md "*Formatted Copy*" async nil
                                visible-only body-only))
                           (include-parent-heading
                              (save-restriction
                                (org-narrow-to-subtree)
                                (org-export-to-buffer
                                    'md "*Formatted Copy*" async nil
                                    visible-only body-only)))
                           (t (org-export-to-buffer
                                  'md "*Formatted Copy*" async 't
                                  visible-only body-only))))))
      (with-current-buffer buffer
        (unwind-protect
          (let ((bufstr (buffer-string)))
               (if (= 0 (length bufstr))
                   (message "Nothing to copy.")
                   (progn
                     ;; Delete leading newline from org-export-to-buffer.
                     (goto-line 1)
                     (evil-yank
                      (point-min)
                      (point-max))
                     (message (concat
                               "Exported children of subtree starting with `"
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
           ; A question to ask
           "ASK(a)"
           ; Question was asked, but we're waiting for them to respond
           "ASKED(e)"
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
  ; When editing text near hidden text (e.g., the "..." ellipses after folded
  ; headings), expand it so that we are forced to only edit text around hidden
  ; text when it is un-hidden.
  (setq org-catch-invisible-edits 'show-and-error)
  ; Never make trees' trailing empty lines visible from collapsed view.
  (setq org-cycle-separator-lines 0)
  ; Introduce unordered bulleted list hierarchy. We flip-flop between "-" and
  ; "+" as we continue to nest. This helps keep track of nesting.
  (setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "-")))
  ; Enable habits (see https://orgmode.org/manual/Tracking-your-habits.html).
  (add-to-list 'org-modules 'org-habit t)
  ; Show daily habits in the agenda even if they have already been completed for
  ; today. This is useful for the consistency graph being displayed even for
  ; completed items.
  (setq org-habit-show-all-today t)
  ; Disable doom's habit graph resizing code, because it right-aligns the
  ; consistency graph. This makes the graph's rows hard to line up with the text
  ; on the left describing the actual habits (on widescreen monitors, the
  ; detriment to usabilitiy is especially pronounced).
  (defun +org-habit-resize-graph-h nil)
  ; Set the absolute starting point for the consistency graph. Our habit
  ; descriptions are short enough that this works fine. The effect is that the
  ; graph is now left-aligned, closer to the habit descriptions. This improves
  ; readability.
  (setq org-habit-graph-column 41)
  ; Show the past 28 days of history.
  (setq org-habit-preceding-days 28)
  ; Set 4AM as the true "ending time" of a day, and make it so that any task
  ; completed between 12AM and 4AM are recorded as 23:59 of the previous day.
  (setq org-extend-today-until 4
        org-use-effective-time t)
  (add-hook 'org-mode-hook #'(lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (use-package! org-appear
    :config
    ;; Hide emphasis markers (e.g., *foo*, /foo/, =foo=).
    (setq org-hide-emphasis-markers t)
    ;; Toggle emphasis markers.
    (setq org-appear-autoemphasis t)

    ;; Toggle links (relies on org-link-descriptive).
    (setq org-appear-autolinks t)

    ;; Trigger the unhiding of things based on whether we enter or leave insert
    ;; mode in evil-mode.
    (setq org-appear-trigger 'manual)
    (add-hook 'org-mode-hook (lambda ()
                               (add-hook 'evil-insert-state-entry-hook
                                         #'org-appear-manual-start
                                         nil
                                         t)
                               (add-hook 'evil-insert-state-exit-hook
                                         #'org-appear-manual-stop
                                         nil
                                         t))))
  ;; Turn on dynamic headline numbering (org-num-mode) because it helps us
  ;; understand roughly where we are in the headline hierarchy.
  (setq org-startup-numerated t)
  (after! org-superstar
    ;; Custom bullets for heading bullets. We use the same symbol across all
    ;; levels (similar to default Org behavior of using '*' across all levels).
    (setq org-superstar-headline-bullets-list '(#x25A0))

    ;; Hide leading stars entirely. This way headings are never indented. We
    ;; already get automatic numbering which tells us how deeply nested we are
    ;; anyway with `org-num-mode' above, so we don't really lose any contextual
    ;; information by doing this.
    (setq org-superstar-remove-leading-stars t)

    ;; Custom bullets for plain lists. Unlike headings, the customization here is
    ;; not about nesting levels at all. Instead it is just a direct 1:1
    ;; replacement of which other character to use for the usual characters "-+*"
    ;; that Org cycles when calling `org-cycle-list-bullet' on a plain list item.
    (setq org-superstar-prettify-item-bullets t)
    (setq org-superstar-item-bullet-alist
          '((?- . #x25CF)    ;; ● BLACK CIRCLE
            (?+ . #x21AA)    ;; ↪ RIGHT ARROW WITH HOOK
            (?* . #x2738)))) ;; ✸ HEAVY EIGHT POINTED RECTILINEAR BLACK STAR
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (after! (org org-fancy-priorities)
    (setq org-priority-highest 0
          org-priority-default 2
          org-priority-lowest 4)
    (setq org-fancy-priorities-list '(
                                      (?0 . "[P0]")
                                      (?1 . "[P1]")
                                      (?2 . "[P2]")
                                      (?3 . "[P3]")
                                      (?4 . "[P4]"))

          org-priority-faces '((?0 :foreground "#f00")
                               (?1 :foreground "#ff0")
                               (?2 :foreground "#0f0")
                               (?3 :foreground "#0ff")
                               (?4 :foreground "#ccc"))))

  (add-hook 'org-mode-hook 'org-fancy-priorities-mode)
  (add-hook 'org-babel-post-tangle-hook (lambda ()
                                          (delete-trailing-whitespace)
                                          (save-buffer)))
  (defun l/org-log-note-buffer-empty-p ()
    "Is current buffer empty except for the boilerplate template at the top?"
    (eq (point-max) 85))

  (defun l/org-store-log-note (orig-fun)
    (let ((org-note-abort (l/org-log-note-buffer-empty-p)))
      (apply orig-fun nil)))

  (advice-add 'org-store-log-note :around #'l/org-store-log-note)
  (setq org-id-link-to-org-use-id t)
  (add-hook 'org-mode-hook (lambda () (modify-syntax-entry ?= ".")))
  (add-hook 'org-mode-hook (lambda () (org-indent-mode -1)))
  (add-hook 'org-mode-hook 'l/org-colors))

;; Dim org-block face (source code blocks) separately, because they are not
;; dimmed by default. Also dim org-hide as well.
(defun l/org-colors ()
  (add-to-list 'face-remapping-alist
               `(org-hide (:filtered
                           (:window adob--dim t)
                           (:foreground ,l/color-xGrey1)) org-hide))
  (add-to-list 'face-remapping-alist
               `(org-block (:filtered
                            (:window adob--dim t)
                            (:background ,l/color-xGrey2)) org-block)))

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

(setq org-log-done 'note)
(setq org-log-redeadline 'note)
(setq org-log-reschedule 'note)
(use-package! org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))
(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      :mnv "SPC" nil
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
           ;; Skip entries that haven't been marked with any of "DONE" keywords.
           '(org-agenda-skip-entry-if 'nottodo 'done))))
        ("c" "Composite view"
         ;; We only show P0 TODO items if the have been scheduled, and their
         ;; scheduled date is today or in the past. This way we only concern
         ;; ourselves with tasks that we can actually work on.
         ((tags
           "URGENCY>=\"0\""
           ((org-agenda-skip-function
             '(or
               ;; Skip entries if they haven't been scheduled yet.
               (l/org-agenda-skip-if-scheduled-later)
               ;; Skip entries if they are DONE (or CANCELED, etc).
               (org-agenda-skip-entry-if 'todo 'done)))
            (org-agenda-overriding-header
             "Prioritized tasks from today or the past")))
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
          (org-agenda-compact-blocks t)))))

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

;; Adapted from
;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html.
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
(defun l/org-roam-open-node (&optional initial-input)
  "Search for org-roam nodes and open in a new tab."
  (interactive)
  (let ((node (org-roam-node-read initial-input)))
    (if node (progn (tab-bar-new-tab) (org-roam-node-open node)))))

(defun l/org-roam-capture (key subdir)
  (interactive)
  (org-roam-capture
   nil key
   :filter-fn (lambda (node)
                (string-equal subdir (org-roam-node-doom-type node)))))

(defun l/rg-search (dir pat &rest args)
  "Use rg-helper.sh to search DIR for pat. See rg-helper.sh for
details."
  (interactive)
  (let ((dir-expanded (expand-file-name dir)))
    (tab-bar-new-tab)
    (consult--grep
     ;; Prompt
     "rg"
     ;; Make-builder
     #'consult--ripgrep-make-builder
     ;; Dir
     dir-expanded
     ;; Initial input
     pat)))
(map! :after org-roam
      :map org-roam-mode-map
      :mnvi "C-k" nil
      :mnvi "C-j" nil)
(after! org-roam
  (setq
        org-roam-node-display-template
        (format "%s %s ${doom-hierarchy}"
                (propertize "${doom-type:10}" 'face 'font-lock-keyword-face)
                (propertize "${doom-tags:50}" 'face 'org-tag))))
(setq org-roam-directory (concat org-directory "/note/")
      l/org-roam-default-template
        (concat "#+title: ${title}\n"
                "#+filetags: UNTAGGED\n"
                "\n"
                "* FOO")
      l/org-roam-zk-template
        (concat "#+title: ${title}\n"
                "#+filetags: UNTAGGED\n\n")
      l/org-roam-default-olp '("FOO")
      org-roam-capture-templates
      `(("r" "raw" plain
         "%?"
         :target (file+head+olp "raw/${slug}.org"
                                ,l/org-roam-default-template
                                ,l/org-roam-default-olp)
         :unnarrowed t)

        ("p" "personal" plain
         "%?"
         :target (file+head+olp "personal/${slug}.org"
                                ,l/org-roam-default-template
                                ,l/org-roam-default-olp)
         :unnarrowed t)

        ("z" "zk" plain
         "%?"
         :target (file+head "zk/${slug}-%<%Y%m%d%H%M%S>.org"
                                ,l/org-roam-zk-template)
         :unnarrowed t)

        ("Z" "zk-join" plain
         "%?"
         :target (file+head+olp "zk-join/${slug}.org"
                                ,l/org-roam-default-template
                                ,l/org-roam-default-olp)
         :unnarrowed t)))

(use-package org-fc
  :after org

  :custom
  (org-fc-directories '("~/lo/note"))

  :config
  (require 'org-fc-keymap-hint)

  :init
  ;; Set keys that were overridden by evil-mode.

  ;; Keys while viewing a prompt.
  (evil-define-minor-mode-key 'normal 'org-fc-review-flip-mode
    (kbd "RET") 'org-fc-review-flip
    (kbd "n") 'org-fc-review-flip
    (kbd "p") 'org-fc-review-edit
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit)
  ;; Keys while evaluating the result.
  (evil-define-minor-mode-key 'normal 'org-fc-review-rate-mode
    (kbd "a") 'org-fc-review-rate-again
    (kbd "h") 'org-fc-review-rate-hard
    (kbd "g") 'org-fc-review-rate-good
    (kbd "e") 'org-fc-review-rate-easy
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit)
  ;; Keys while in the dashboard.
  (evil-define-key 'normal org-fc-dashboard-mode-map
    (kbd "q") 'kill-current-buffer
    (kbd "r") 'org-fc-dashboard-review)

  ;; Keys to invoke org-fc.
  (map! :leader
       (:prefix ("r" . "Flashcards")
        :desc "Dashboard"     "R" #'org-fc-dashboard
        :desc "Review"        "r" #'org-fc-review
        (:prefix ("n" . "New Flashcard")
         :desc "Normal"        "i" #'org-fc-type-normal-init
         :desc "Normal"        "n" #'org-fc-type-normal-init
         :desc "Cloze"         "c" #'org-fc-type-cloze-init
         :desc "Double"        "d" #'org-fc-type-double-init
         :desc "Text-Input"    "t" #'org-fc-type-text-input-init))))
(use-package! hyperbole
  :init
  (hyperbole-mode 1)
  :config
  (let ((l/jira-base-url (getenv "L_JIRA_BASE_URL")))
    (when l/jira-base-url
      ;; Define action for button.
      (defun l/browse-jira-ticket (ticket)
        "Open ticket in JIRA."
        (let ((url (concat l/jira-base-url ticket)))
          (browse-url-default-browser url)))
      ;; Define text pattern for button.
      (defib l/open-jira-ticket-at-point ()
        "Get the Jira ticket identifier at point and load ticket in browser."
        (when-let ((regex "\\([A-Z]+-[0-9]+\\)")
                   (ticket (save-excursion
                             (skip-chars-backward "A-Z0-9-")
                             (looking-at regex)
                             (match-string-no-properties 1))))
          (ibut:label-set ticket
                          (match-beginning 1)
                          (match-end 1))
          (hact 'l/browse-jira-ticket ticket)))))
  )
(map! :after alchemist
      :map alchemist-mode-map
      :mnvi "C-k" nil
      :mnvi "C-j" nil)
(map! :after cider
      :map cider-repl-mode-map
      ; Use M-{k,j} instead of M-{p,n} for cycling through history.
      :mnvi "M-k" #'cider-repl-previous-input
      :mnvi "M-j" #'cider-repl-next-input

      ; Disable some conflicting keybindings in =cider-stacktrace-mode=, which
      ; pops up if we hit an exception inside a CIDER session.
      :map cider-stacktrace-mode-map
      :mnvi "C-k" nil
      :mnvi "C-j" nil)
(add-hook 'clojure-mode-hook 'l/customize-clojure-mode)
(defun l/customize-clojure-mode ()
  (interactive)
  (auto-fill-mode 1)
  (setq cider-preferred-build-tool 'clojure-cli))
(add-hook 'c-mode-hook 'l/customize-c-mode)
(defun l/customize-c-mode ()
  (interactive)
  (setq c-default-style "linux"
        c-basic-offset 8
        tab-width 8))
(map! :after ccls
      :map (c-mode-map c++-mode-map)
      :mnvi "C-h" nil
      :mnvi "C-l" nil
      :mnvi "C-k" nil
      :mnvi "C-j" nil)
(defun l/set-tab-width-to-8 ()
  (interactive)
  (setq tab-width 8)
  (setq c-basic-offset 8)
  (setq sh-basic-offset 8))
(add-hook 'makefile-mode-hook #'l/set-tab-width-to-8)
(add-hook 'makefile-automake-mode-hook #'l/set-tab-width-to-8)
(add-hook 'makefile-gmake-mode-hook #'l/set-tab-width-to-8)
(add-hook 'makefile-bsdmake-mode-hook #'l/set-tab-width-to-8)
(defvar l/c-like-modes '(c-mode))
(defvar l/banned-auto-format-dirs '("prog/git"))

(defun l/auto-format-buffer-p ()
  (interactive)
  (and (or (not (member major-mode l/c-like-modes))
           (locate-dominating-file default-directory ".clang-format"))
       (buffer-file-name)
       (save-match-data
         (let ((dir (file-name-directory (buffer-file-name))))
           (not (cl-some (lambda (regexp) (string-match regexp dir))
                    l/banned-auto-format-dirs))))))

(defun l/after-change-major-mode ()
  (progn
    (apheleia-mode (if (l/auto-format-buffer-p) 1 -1))))

(add-hook! 'after-change-major-mode-hook 'l/after-change-major-mode)
(after! sh-script
  (set-formatter! 'shfmt
    '("shfmt"
       "--binary-next-line"
       "--func-next-line"
      (format "--indent=%d" (if indent-tabs-mode
                                0
                              2))
      (format "--language-dialect=%s"
       (pcase sh-shell (`bash "bash") (`mksh "mksh") (_ "posix"))))))
(add-hook 'sh-mode-hook #'l/set-tab-width-to-8)
(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (display-fill-column-indicator-mode 1)))

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
  (if (> 0 cnt)
      (progn (evil-scroll-line-up (abs cnt))
             (evil-previous-line (abs cnt)))
      (progn (evil-scroll-line-down cnt)
             (evil-next-line cnt))))
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

(map! :after evil-common
      :map evil-normal-state-map
      "C-n" #'+workspace:switch-next)

(defun l/split-window-vertically ()
  "Split window verically."
  (interactive)
  (split-window-vertically)
  (balance-windows)
  (other-window 1))
(defun l/split-window-horizontally ()
  "Split window horizontally."
  (interactive)
  (split-window-horizontally)
  (balance-windows)
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
  (let* ((original-bufname (buffer-name))
         (aux-buffer-rgx "^ *\*.+\*$")
         (is-aux-buffer (l/buffer-looks-like original-bufname '("^ *\*.+\*$")))
         (buffers (mapcar 'buffer-name (buffer-list)))
         (primary-buffers-count
           (length
             (seq-filter
               '(lambda (bufname) (not (string-match "^ *\*.+\*$" bufname)))
               buffers)))
         (primary-buffer-exists (> primary-buffers-count 0)))

    ; If we're on a magit-controlled buffer, do what magit expects and simulate
    ; pressing C-c C-c (with-editor-finish).
    (catch 'my-catch
      (progn
        (if (bound-and-true-p with-editor-mode)
          (if (buffer-modified-p)
            ; If there are any unsaved changes, either discard those changes or
            ; do nothing.
            (if
              (y-or-n-p
               (concat "l/quit-buffer: Invoke (with-editor-cancel) "
                       "to cancel the editing of this buffer?"))
              (with-editor-cancel t)
              ; Use catch/throw to stop execution.
              (throw 'my-catch
                     (message "l/quit-buffer: Aborting (doing nothing).")))
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
                ; Break loop if somehow our aux-buffer-rgx failed to account for
                ; all hidden/aux buffers and we are just looping over and over
                ; among the same list of actual auxiliary buffers.
                (if (string= original-bufname (buffer-name))
                  (throw 'buffer-cycle-detected
                    (message
                     (concat "l/quit-buffer: Buffer cycle detected among "
                             "auxiliary buffers; invoking `l/gc-views'.")))
                  (previous-buffer))))
            ; If we've broken the loop (due to a cycle), run (l/gc-views) as
            ; it is better than doing nothing.
            (l/gc-views)
            (balance-windows))
          (l/gc-views)
          (balance-windows))))))

; Either close the current window, or if only one windw, use the ":q" Evil
; command; this simulates the ":q" behavior of Vim when used with tabs to
; garbage-collect the current "view".
(defun l/gc-views ()
  "Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab."
  (interactive)
  (let ((one-tab (= 1 (length (tab-bar-tabs))))
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
              ; around as an invisible frame even after they are deleted. Delete
              ; all other frames whenever we exit from a single visible daemon
              ; frame, because there is no point in keeping them around. If
              ; anything they can hinder detection of "is there a visible
              ; frame?" logic from the shell.
              (delete-other-frames)
              ; While we're at it, also close all buffers, because it's annoying
              ; to have things like Helm minibuffers and the like sitting
              ; around.
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
      :imnv "C-j" (cmd!! #'other-window 1)
      :imnv "C-k" (cmd!! #'other-window -1)
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
         (buf-date-match
          (string-match
           "^[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}$"
           bufname-short))
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

(use-package! notmuch
  :config
  (add-hook 'notmuch-message-mode-hook 'l/customize-notmuch-message-mode)
  (defun l/customize-notmuch-message-mode ()
    (interactive)
    (flycheck-mode -1)
    (git-gutter-mode -1)
    (smartparens-mode -1))
  (map! :after notmuch
        :map notmuch-show-mode-map
        :mnv "C-k" nil
        :mnv "C-j" nil
        :mnv "H" #'previous-buffer)
  (map! :after notmuch
        :map notmuch-tree-mode-map
        :mnv "C-k" nil
        :mnv "C-j" nil)
  (map! :after notmuch
        :map notmuch-show-mode-map
        ;; Swap "cr" and "cR". `notmuch-show-reply' is "reply all" and is the more
        ;; common one we use in mailing list discussions (you would almost never
        ;; only reply to the sender only, which is what
        ;; `notmuch-show-reply-sender' does), so give it the simpler "cr" binding.
        :mnv "cr" #'notmuch-show-reply
        :mnv "cR" #'notmuch-show-reply-sender)
  (setq notmuch-saved-searches
        '((:name "inbox"
           :query "tag:inbox"
           :count-query "tag:inbox AND tag:unread"
           :key "i")

          (:name "git-me"
           :query "tag:git and \"Linus Arver\""
           :count-query "tag:git AND tag:unread"
           :key "g")

          (:name "git-cook"
           :query "tag:git and \"Cooking\""
           :count-query "tag:git AND tag:unread and Cooking"
           :key "G")

          (:name "sent"
           :query "tag:sent"
           :key "s")))
  (defun l/+notmuch-get-sync-command (orig-fun) "~/syscfg/script/mail-sync.sh")
  (advice-add '+notmuch-get-sync-command :around #'l/+notmuch-get-sync-command)
  (setq sendmail-program "gmi")
  (setq message-sendmail-extra-arguments
        '("send" "--quiet" "-t" "-C" "~/mail/linusarver@gmail.com"))
  (defun notmuch-mua-reply-guess-sender (orig-fun query-string &optional sender
                                                  reply-all duplicate)
    (let ((sender (or sender
                      "Linus Arver <linus@ucla.edu>")))
      (funcall orig-fun query-string sender reply-all duplicate)))
  (advice-add 'notmuch-mua-reply :around 'notmuch-mua-reply-guess-sender)
  (after! (notmuch magit)
    (add-hook 'notmuch-show-hook 'l/set-current-magit-directory))
  (defun l/set-current-magit-directory ()
    (interactive)
    (let ((tags (notmuch-show-get-tags)))
      (cond
        ((member "git" tags) (setq-local default-directory "~/prog/git"))))))

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
  ;; Disable some cosmetics because of an annoying "Error processing message
  ;; (args-out-of-range ..." error that happens every time we eval a buffer.
  ;; See
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3586#issuecomment-1166620517.
  (setq lsp-enable-symbol-highlighting nil)
  ;; Disable autoformatting of YAML files, because it can result in huge
  ;; indentation (whitespace) changes with no semantic difference.
  (setq lsp-yaml-format-enable nil)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-.*\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'"))
(map! :after evil
      :leader
      :mnv "c," #'evilnc-comment-or-uncomment-lines)
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
  (setq tab-bar-separator
        (propertize " "
                    'font-lock-face
                    `(:background ,(doom-darken (doom-color 'bg-alt) 0.2))))
  (custom-set-faces!
    `(vertical-border
      :background ,(doom-color 'base0) :foreground ,(doom-color 'base0))
    '(highlight-numbers-number  :weight bold)
    `(hl-line :background ,(doom-darken (doom-color 'bg-alt) 0.4))
    '(vim-empty-lines-face :weight bold)

    `(auto-dim-other-buffers-face
      :background ,(doom-darken (doom-color 'bg-alt) 0.6))
    '(org-headline-done        :foreground "#aaaaaa" :weight bold)

    ; Use bright visuals for coloring regions and interactive search hits.
    '(lazy-highlight  :foreground "pink" :background "dark red" :weight normal)
    '(isearch  :foreground "dark red" :background "pink" :weight bold)
    '(region  :foreground "dark red" :background "pink" :weight bold)

    ; vertico
    `(vertico-multiline       :foreground ,l/color-foreground)
    `(vertico-group-title     :foreground ,l/color-xBrightOrange)
    `(vertico-group-separator :foreground ,l/color-xBrightOrange
                              :strike-through t)

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

    ; LSP-related faces.
    `(lsp-lens-face      :foreground  ,(doom-lighten (doom-color 'grey) 0.3))
    `(lsp-details-face   :foreground  ,(doom-lighten (doom-color 'grey) 0.3))
    `(lsp-signature-face :foreground  ,(doom-lighten (doom-color 'grey) 0.3))

    `(mode-line
       :weight bold
       :background ,(doom-color 'base8)
       :foreground ,(doom-color 'base0))
    `(mode-line-inactive
       :background ,(doom-color 'base6)
       :foreground ,(doom-color 'base0))

    `(org-roam-header-line
       :background ,(doom-color 'base7)
       :foreground ,(doom-color 'base0)
       :weight bold)

    `(notmuch-message-summary-face :foreground ,l/color-foreground)
    `(notmuch-search-count :foreground ,l/color-foreground)
    `(notmuch-tree-no-match-subject-face :foreground ,l/color-foreground)
    `(notmuch-wash-cited-text :foreground ,l/color-foreground)

    `(git-gutter:modified :foreground ,l/color-xUltraBrightMagenta)
    `(git-gutter:added :foreground ,l/color-xUltraBrightGreen)
    `(git-gutter:deleted :foreground ,l/color-xUltraBrightRed)
    ;; Fix ugly colors for diffs. Prevalent because of git comit message buffers
    ;; like COMMIT_EDITMSG.
    '(git-commit-summary  :foreground "brightwhite" :weight bold)
    '(diff-added        :foreground "#ccffcc" :background "#335533"
                        :weight bold)
    '(diff-removed      :foreground "#ffcccc" :background "#553333"
                        :weight bold)
    '(diff-context      :foreground "brightwhite")
    '(diff-function     :foreground "brightmagenta")
    '(diff-header       :foreground "#ffff00" :background "#555533"
                        :weight bold)
    '(diff-file-header  :foreground "brightyellow")
    '(diff-hunk-header  :foreground "brightcyan")
    '(git-commit-keyword  :foreground "brightmagenta" :weight bold))

  ;; Make all doom-modeline-* faces have a uniform foreground, to make them
  ;; easier to read with our custom mode-line background. This way we don't have
  ;; to spell out each font one at a time.
  (eval `(l/custom-set-faces-matching! "doom-modeline-"
                                       :foreground ,(doom-color 'base0))))

(use-package! rainbow-mode
  :hook (prog-mode text-mode))
;; Disable rainbow-mode (because "#def" in "#define" gets interpreted as a hex
;; color.)
(add-hook 'c-mode-hook (lambda () (rainbow-turn-off)))
(use-package! doom-themes
  :config
  (advice-add 'doom-init-theme-h :after #'l/reset-faces)
  (cond
   ((string= "l" (daemonp))
    (load-theme 'doom-one t))
   (t
    (load-theme 'doom-zenburn t))))
;; Enable soft word-wrap almost everywhere (including elisp).
(+global-word-wrap-mode +1)

; Enable only left-side fringe.
(set-fringe-mode '(10 . 0))

; Disable hl-line mode, because it can be surprisingly disorienting. Besides, we
; can always use "v" or "V" to get a visual queue easily enough.
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(use-package! vim-empty-lines-mode
  :config
  (add-hook 'org-mode-hook 'vim-empty-lines-mode)
  (add-hook 'prog-mode-hook 'vim-empty-lines-mode)
  (add-hook 'text-mode-hook 'vim-empty-lines-mode))

(use-package! doom-modeline
  :config
  ;; If the window width is 100 or less, start truncating certain things (e.g.,
  ;; overly long file path names for Java/Clojure codebases).
  (setq doom-modeline-window-width-limit 100))

; Dim buffers in inactive windows to make the current one "pop".
(use-package! auto-dim-other-buffers
 :config
 (auto-dim-other-buffers-mode))

; Always enable the tab bar, even if there is just one buffer showing (such as
; when we open a single buffer).
(tab-bar-mode)

; Enable the mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; Disable vertical bar cursor shape in terminal emacs.
(setq evil-motion-state-cursor 'box)
(setq evil-visual-state-cursor 'box)
(setq evil-normal-state-cursor 'box)
(setq evil-insert-state-cursor 'box)
(setq evil-emacs-state-cursor  'box)
;; Broken. See README.org for discussion.
;; (tty--set-output-buffer-size (* 128 1024))
(setq
 spell-fu-directory "~/syscfg/emacs/spell-fu"
 ispell-library-directory "~/syscfg/emacs/spell-fu"
 ispell-dictionary "en"
 ispell-personal-dictionary "~/syscfg/emacs/spell-fu/custom-dict.txt")
;; Extra faces we want to avoid spellchecking for, grouped by major mode.
(setq l/spell-excluded-faces-alist
  '(;; This mode is empty, but it's good to have it still to make it easier to
    ;; see the shape of the data.
    (latex-mode
     . ())
    (org-mode
     . (
        ;; Disable spellchecking for text inside tables.
        org-table))))

(after! spell-fu
  (dolist (major-mode '(latex-mode org-mode))
    (dolist (face (alist-get major-mode l/spell-excluded-faces-alist))
      (cl-pushnew face (alist-get major-mode +spell-excluded-faces-alist)))))

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

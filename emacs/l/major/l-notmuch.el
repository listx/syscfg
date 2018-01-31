(use-package notmuch
  :defer t
  :config

  ; Bindings.
  (evil-define-key 'normal notmuch-hello-mode-map
    "h" 'widget-backward
    "j" 'widget-forward
    "k" 'widget-backward
    "l" 'widget-forward
    (kbd "<tab>") 'other-window
    [escape] 'evil-force-normal-state
    "H" 'evil-next-buffer
    "L" 'evil-prev-buffer
    "e" 'notmuch-jump-search
    "/" 'notmuch-search)

  ; Use consistent keybindings across multiple notmuch modes.
  (l/bind-keys
    'normal
    '(
      notmuch-show-mode-map
      notmuch-tree-mode-map)
    `(
      (:key "a" :func (lambda () (interactive) (l/toggle-tag-list '("-inbox" "+archived"))))
      (:key "d" :func (lambda () (interactive) (l/toggle-tag-list '("+trash"))))
      (:key "f" :func (lambda () (interactive) (l/toggle-tag-list '("+flagged"))))
      (:key "u" :func (lambda () (interactive) (l/toggle-tag-list '("+unread"))))
      (:key "s" :func (lambda () (interactive) (l/toggle-tag-list '("+spam"))))
      (:key ,(kbd "<down>") :func (lambda () (interactive) (l/show-message t)))
      (:key ,(kbd "<up>") :func (lambda () (interactive) (l/show-message nil)))
      (:key ,(kbd "S-<down>") :func (lambda () (interactive) (l/show-thread t)))
      (:key ,(kbd "S-<up>") :func (lambda () (interactive) (l/show-thread nil)))
    ))

  (l/bind-keys
    'normal
    '(
      notmuch-hello-mode-map
      notmuch-show-mode-map
      notmuch-tree-mode-map
      notmuch-search-mode-map)
    '(
      (:key "q" :func notmuch-bury-or-kill-this-buffer)
      (:key "=" :func notmuch-refresh-this-buffer)
    ))

  (evil-define-key 'normal notmuch-tree-mode-map "A" (lambda () (interactive) (l/toggle-tag-list '("-inbox" "+archived") t)))
  (evil-define-key 'normal notmuch-tree-mode-map "D" (lambda () (interactive) (l/toggle-tag-list '("+trash") t)))
  (evil-define-key 'normal notmuch-tree-mode-map "F" (lambda () (interactive) (l/toggle-tag-list '("+flagged") t)))
  (evil-define-key 'normal notmuch-tree-mode-map "U" (lambda () (interactive) (l/toggle-tag-list '("+unread") t)))

  (evil-define-key 'normal notmuch-tree-mode-map "J" 'notmuch-tree-next-thread)
  (evil-define-key 'normal notmuch-tree-mode-map "K" 'notmuch-tree-prev-thread)

  (evil-define-key 'normal notmuch-show-mode-map "o" 'hydra-notmuch-show/body)
  (evil-define-key 'normal notmuch-show-mode-map "r" 'notmuch-show-reply)
  (evil-define-key 'normal notmuch-show-mode-map "R" 'notmuch-show-reply-sender)

  (evil-define-key 'normal notmuch-search-mode-map (kbd "RET") 'notmuch-search-show-thread)
  (evil-define-key 'normal notmuch-tree-mode-map   (kbd "RET") 'l/open-message)

  ; Refresh notmuch-hello buffer upon regaining focus. We only do it in
  ; notmuch-hello-mode, because in notmuch-tree-mode, calling this hook results
  ; in reordering the search results (perhaps an upstream bug). Note that
  ; visiting the notmuch-hello-mode buffer (e.g., from exiting
  ; notmuch-tree-mode) already calls notmuch-refresh-this-buffer; the added
  ; benefit here is that we call this function when re-visiting an out-of-focus
  ; Emacs frame that is already showing notmuch-hello-mode.
  (add-hook 'focus-in-hook (lambda () (interactive) (when (string= major-mode "notmuch-hello-mode") (notmuch-refresh-this-buffer))))

  ; Change how the notmuch-hello page looks. Inspired by
  ; http://www.holgerschurig.de/en/emacs-notmuch-hello/.
  (setq notmuch-saved-searches
      '((:key "i" :name "inbox" :query "tag:main and tag:inbox and (not (tag:ml or tag:nl or tag:archived))")
        (:key "u" :name "unread" :query "tag:main and tag:unread")
        (:key "f" :name "flagged" :query "tag:main and tag:flagged")
        (:key "s" :name "sent" :query "falder:main/sent")
        (:key "z" :name "zsh-users" :query "tag:main and tag:zsh-users")
        (:key "n" :name "notmuch" :query "tag:main and tag:notmuch")
        (         :name "woodworking" :query "tag:main and from:admin@paulsellers.com")
        (:key "p" :name "purchases" :query "tag:main and tag:purchases")
        (:key "S" :name "social" :query "tag:main and tag:social")
        (:key "F" :name "finance" :query "tag:main and tag:finance")
        (         :name "archived" :query "tag:main and tag:archived")
        (:key "d" :name "trash" :query "folder:main/trash")
        (         :name "spam" :query "folder:main/spam")
        ))

  (setq notmuch-hello-sections '(
    l/notmuch-hello-insert-searches
    l/notmuch-hello-insert-recent-searches))

  (defface l/notmuch-hello-header-face
    '((t :weight bold))
    "Font for the header in `l/notmuch-hello-insert-searches`."
    :group 'notmuch-faces)

  ; Authentication for sending emails.
  (setq
    auth-sources
      '((:source "~/secure/authinfo.gpg"))
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-smtp-service 587)
  (setq mail-user-agent 'message-user-agent)
  (setq user-mail-address "linusarver@gmail.com"
        user-full-name "Linus Arver")
  ; Set from-address to either `mail-envelope-from' or `user-mail-address' as
  ; fallback.
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)
  (setq message-sendmail-envelope-from 'header)
  ; Add Cc and Bcc headers to the message buffer.
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  ; Save message drafts to ~/tmp if we save the buffer during message
  ; composition.
  (setq message-auto-save-directory "~/tmp")
  ; Change the directory to store the sent mail.
  (setq message-directory "~/mail/")
  ; Kill buffer after sending mail.
  (setq message-kill-buffer-on-exit t)
  ; Setup message signature.
  (add-hook 'message-signature-setup-hook 'l/message-signature-setup)

  ; Add a thousandth separator for message counts.
  (setq notmuch-hello-thousands-separator ",")
  ; Display newest email up top.
  (setq notmuch-search-oldest-first nil))

(defun l/bind-keys (mode keymaps plists)
  "Call l/bind-key en masse for multiple keys."
  (mapc (lambda (plist) (l/bind-key mode keymaps plist)) plists))

(defun l/bind-key (mode keymaps plist)
  "Bind the same key to multiple keymaps."
  (let
    (
      (key (plist-get plist :key))
      (func (plist-get plist :func)))
    (mapc (lambda (keymap) (evil-define-key mode keymap key func)) keymaps)))

(defun l/notmuch-hello-insert-searches ()
  "Insert the saved-searches section."
  (widget-insert (propertize "New     Total      Key  List\n" 'face 'l/notmuch-hello-header-face))
  (mapc (lambda (elem)
          (when elem
            (let* ((q_tot (plist-get elem :query))
                    (q_new (concat q_tot " AND tag:unread"))
                    (n_tot (l/count-query q_tot))
                    (n_new (l/count-query q_new)))
              (l/notmuch-hello-query-insert n_new q_new elem)
              (l/notmuch-hello-query-insert n_tot q_tot elem)
              (widget-insert "   ")
              (widget-insert
                ; Only insert a hotkey if there is one.
                (if (plist-member elem :key)
                  (plist-get elem :key)
                  " "))
              (widget-insert "    ")
              (widget-insert (plist-get elem :name))
              (widget-insert "\n")
            ))
          )
        notmuch-saved-searches))

(defun l/count-query (query)
  (with-temp-buffer
    (insert query "\n")
    (unless (= (call-process-region (point-min) (point-max) notmuch-command
                                    t t nil "count" "--batch") 0)
      (notmuch-logged-error "notmuch count --batch failed"
"Please check that the notmuch CLI is new enough to support `count
--batch'. In general we recommend running matching versions of
the CLI and emacs interface."))

    (goto-char (point-min))
    (let ((n (read (current-buffer))))
      (if (= n 0)
          nil
        (notmuch-hello-nice-number n)))))

(defun l/notmuch-hello-query-insert (cnt query elem)
  (if cnt
      (let* ((str (format "%s" cnt))
              (widget-push-button-prefix "")
              (widget-push-button-suffix "")
              (oldest-first (case (plist-get elem :sort-order)
                              (newest-first nil)
                              (oldest-first t)
                              (otherwise notmuch-search-oldest-first))))
        (widget-create 'push-button
                        :notify #'notmuch-hello-widget-search
                        :notmuch-search-terms query
                        :notmuch-search-oldest-first oldest-first
                        :notmuch-search-type 'tree
                        str)
        (widget-insert (make-string (- 8 (length str)) ? )))
    (widget-insert "        ")))

(defun l/notmuch-hello-insert-recent-searches ()
  "Insert recent searches."
  (when notmuch-search-history
    (widget-insert "Recent searches:")
    (widget-insert "\n\n")
    (let ((start (point)))
      (loop for i from 1 to notmuch-hello-recent-searches-max
        for search in notmuch-search-history do
        (let ((widget-symbol (intern (format "notmuch-hello-search-%d" i))))
          (set widget-symbol
           (widget-create 'editable-field
                  ;; Don't let the search boxes be
                  ;; less than 8 characters wide.
                  :size (max 8
                         (- (window-width)
                        ;; Leave some space
                        ;; at the start and
                        ;; end of the
                        ;; boxes.
                        (* 2 notmuch-hello-indent)
                        ;; 1 for the space
                        ;; before the `[del]'
                        ;; button. 5 for the
                        ;; `[del]' button.
                        1 5))
                  :action (lambda (widget &rest ignore)
                        (notmuch-hello-search (widget-value widget)))
                  search))
          (widget-insert " ")
          (widget-create 'push-button
                 :notify (lambda (widget &rest ignore)
                       (when (y-or-n-p "Are you sure you want to delete this search? ")
                     (notmuch-hello-delete-search-from-history widget)))
                 :notmuch-saved-search-widget widget-symbol
                 "del"))
        (widget-insert "\n"))
      (indent-rigidly start (point) notmuch-hello-indent))
    nil))

(defun l/strip-tags (tag-list)
  "Given a list of tags with + or - prefixes, return only those ones with +, but
without the leading +."
  (let*
    (
      (tag-is-plus (lambda (tag) (string-prefix-p "+" tag)))
      (tag-list-plus-only (seq-filter tag-is-plus tag-list))
      (tag-list-stripped (mapcar (lambda (str) (seq-drop str 1)) tag-list-plus-only))
    )
    tag-list-stripped))

(defun l/is-tag-member (tag-list)
  "Given a tag-list, determine if any of the tags with a leading '+' are a
member of (notmuch-show-get-tags)."
  (let*
    (
      (tag-list-stripped (l/strip-tags tag-list))
      (tag-list-checked
        (mapcar
          (lambda (tag) (member tag (notmuch-show-get-tags)))
          tag-list-stripped))
    )
    ; We reduce the list of booleans in tag-list-checked to see if any of them
    ; are `t'. This would be as trivial as
    ;
    ;   (apply 'or tag-list-checked)
    ;
    ; if `or' was a function, but sadly it is not (it is a macro).
    ; Surprisingly, there is no easy way to reduce a list of booleans in native
    ; Emacs Lisp. We have to borrow "some" from cl-extra.el.
    ;
    ; References:
    ; https://stackoverflow.com/questions/5902847/how-do-i-apply-or-to-a-list-in-elisp
    ; http://www.faqs.org/faqs/lisp-faq/part3/section-3.html.
    (some 'identity tag-list-checked)))

(defun l/toggle-tag-list (tag-list-orig &optional thread)
  "Toggle tags. This is like `notmuch-tree-archive-message', but is generalized
across any list of tags (and is not tied to `notmuch-archive-tags')."
  (let
    (
      (f
        (if (string= major-mode "notmuch-tree-mode")
          (if thread
            'notmuch-tree-tag-thread
            'notmuch-tree-tag)
          'notmuch-show-tag))
      (tag-list         (notmuch-tag-change-list tag-list-orig nil))
      (tag-list-negated (notmuch-tag-change-list tag-list-orig t))
      )
    (funcall f (if (l/is-tag-member tag-list-orig) tag-list-negated tag-list))))

(defun l/open-message ()
  "Open the message under the cursor. Unlike vanilla Notmuch, where RET just
shows the message buffer in a separate window but retains focus in the
notmuch-tree buffer, we switch focus with `other-window'. This way, when we move
up/down with j/k in Normal state, we can scroll through the message buffer. This
behavior mimics the bindings in tig, where pressing RET shows the commit message
and switches focus to that message buffer).

The related functions `l/show-message' and `l/show-thread', also make use of
`other-window'. The use of `other-window' will surely be buggy if we have more
than one window from a notmuch-tree-mode buffer. For now, we are too lazy to fix
this issue."
  (interactive)
  (notmuch-tree-show-message-in)
  (other-window 1))

(defun l/show-message (next)
  "Call appropriate function to see the next/prev message, depending on which
mode is active."
  (interactive)
  (let
    ((f
        (if next 'notmuch-tree-next-message 'notmuch-tree-prev-message)))
    (if (string= major-mode "notmuch-tree-mode")
      (funcall f)
      (progn
        (other-window 1)
        (funcall f)
        (other-window 1)))))

(defun l/show-thread (next)
  "Call appropriate function to see the next/prev thread, depending on which
mode is active."
  (interactive)
  (let
    ((f
        (if next 'notmuch-tree-next-thread 'notmuch-tree-prev-thread)))
    (if (string= major-mode "notmuch-tree-mode")
      (progn
        (funcall f)
        (notmuch-tree-show-message-in))
      (progn
        (other-window 1)
        (funcall f)
        (notmuch-tree-show-message-in)
        (other-window 1)))))

(defun l/message-signature-setup ()
  "Add signature."
  ; We add our own custom bit of text, outside of the default framework provided
  ; by `message-signature', because we don't want to add the "-- " prefix to our
  ; signature. See
  ; https://emacs.stackexchange.com/questions/28100/message-insert-signature-do-not-add-the-prefix.
  ; NOTE: This code is actually half-baked, because we don't know how to create
  ; replies such that the message-cite-reply-position variable actually varies
  ; over different types of messages being replied to. Still, it's a start.
  (let
    ( (sig-ml "\n\n-- \nBest,\nLinus\n")
      (sig-other "\n\nLinus"))
    (save-excursion
      (if (eq message-cite-reply-position 'above)
        (progn
          (message-goto-body)
          (insert sig-other))
        (progn
          (message-goto-signature)
          (insert sig-ml))))))

(defun l/message-signature ()
  "Email signature. In the future, this should depend on the `From:' header of
the message."
  (concat "Best regards,\n" "Linus"))

(defhydra hydra-notmuch-show (:foreign-keys warn)
  "notmuch-show"
  ("d" (lambda () (interactive) (l/toggle-tag-list '("+trash"))) "(un)delete")
  ("f" (lambda () (interactive) (l/toggle-tag-list '("+flagged"))) "(un)flag")
  ("q" nil "exit" :exit t))

(provide 'l-notmuch)

(use-package notmuch
  :defer t
  :config

  ; It's easier to make notmuch-hello-mode's Emacs state behave more sanely than
  ; to make it use Normal state.
  (evil-set-initial-state 'notmuch-hello-mode 'emacs)

  ; Bindings.
  (evil-define-key 'emacs notmuch-hello-mode-map "h" 'widget-backward)
  (evil-define-key 'emacs notmuch-hello-mode-map "j" 'widget-forward)
  (evil-define-key 'emacs notmuch-hello-mode-map "k" 'widget-backward)
  (evil-define-key 'emacs notmuch-hello-mode-map "l" 'widget-forward)
  (evil-define-key 'emacs notmuch-hello-mode-map (kbd "<tab>") 'other-window)
  (evil-define-key 'emacs notmuch-hello-mode-map [escape] 'evil-force-normal-state)
  (evil-define-key 'emacs notmuch-hello-mode-map "H" 'evil-next-buffer)
  (evil-define-key 'emacs notmuch-hello-mode-map "L" 'evil-prev-buffer)

  (evil-define-key 'emacs notmuch-hello-mode-map "e" 'notmuch-jump-search)
  (evil-define-key 'emacs notmuch-hello-mode-map "g" 'notmuch-poll-and-refresh-this-buffer)
  (evil-define-key 'emacs notmuch-hello-mode-map "/" 'notmuch-search)

  (evil-define-key 'normal notmuch-show-mode-map "d" (lambda () (interactive) (l/toggle-tag "trash")))
  (evil-define-key 'normal notmuch-tree-mode-map "d" (lambda () (interactive) (l/toggle-tag "trash")))
  (evil-define-key 'normal notmuch-show-mode-map "f" (lambda () (interactive) (l/toggle-tag "flagged")))
  (evil-define-key 'normal notmuch-tree-mode-map "f" (lambda () (interactive) (l/toggle-tag "flagged")))
  (evil-define-key 'normal notmuch-show-mode-map "u" (lambda () (interactive) (l/toggle-tag "unread")))
  (evil-define-key 'normal notmuch-tree-mode-map "u" (lambda () (interactive) (l/toggle-tag "unread")))
  (evil-define-key 'normal notmuch-show-mode-map "s" (lambda () (interactive) (l/toggle-tag "spam")))
  (evil-define-key 'normal notmuch-tree-mode-map "s" (lambda () (interactive) (l/toggle-tag "spam")))

  (evil-define-key 'normal notmuch-tree-mode-map "D" (lambda () (interactive) (l/toggle-tag "trash" t)))
  (evil-define-key 'normal notmuch-tree-mode-map "F" (lambda () (interactive) (l/toggle-tag "flagged" t)))
  (evil-define-key 'normal notmuch-tree-mode-map "U" (lambda () (interactive) (l/toggle-tag "unread" t)))

  (evil-define-key 'normal notmuch-show-mode-map "o" 'hydra-notmuch-show/body)
  (evil-define-key 'normal notmuch-show-mode-map "r" 'notmuch-show-reply)
  (evil-define-key 'normal notmuch-show-mode-map "R" 'notmuch-show-reply-sender)

  (evil-define-key 'normal notmuch-search-mode-map "q" 'notmuch-bury-or-kill-this-buffer)
  (evil-define-key 'normal notmuch-show-mode-map "q" 'notmuch-bury-or-kill-this-buffer)
  (evil-define-key 'normal notmuch-tree-mode-map "q" 'notmuch-bury-or-kill-this-buffer)

  (evil-define-key 'normal notmuch-search-mode-map "g" 'notmuch-refresh-this-buffer)
  (evil-define-key 'normal notmuch-tree-mode-map "g" 'notmuch-refresh-this-buffer)

  (evil-define-key 'normal notmuch-tree-mode-map (kbd "j") 'notmuch-tree-next-message)
  (evil-define-key 'normal notmuch-tree-mode-map (kbd "k") 'notmuch-tree-prev-message)
  (evil-define-key 'normal notmuch-tree-mode-map (kbd ")") 'notmuch-tree-next-thread)
  (evil-define-key 'normal notmuch-tree-mode-map (kbd "(") 'notmuch-tree-prev-thread)

  (evil-define-key 'normal notmuch-search-mode-map (kbd "RET") 'notmuch-search-show-thread)
  (evil-define-key 'normal notmuch-tree-mode-map   (kbd "RET") 'notmuch-tree-show-message)

  ; Change how the notmuch-hello page looks. Inspired by
  ; http://www.holgerschurig.de/en/emacs-notmuch-hello/.
  (setq notmuch-saved-searches
      '((:key "i" :name "inbox" :query "tag:inbox")
        (:key "u" :name "unread" :query "tag:unread")
        (:key "f" :name "flagged" :query "tag:flagged")
        (:key "s" :name "sent" :query "tag:sent")
        (:key "z" :name "zsh-users" :query "tag:zsh-users")
        (:key "n" :name "notmuch" :query "tag:notmuch")
        (:key "w" :name "woodworking" :query "tag:woodworking")
        (:key "p" :name "purchases" :query "tag:purchases")
        (:key "S" :name "social" :query "tag:social")
        (:key "F" :name "finance" :query "tag:finance")
        (:key "d" :name "trash" :query "tag:trash")
        (         :name "spam" :query "folder:spam")
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

  ; Add a thousandth separator for message counts.
  (setq notmuch-hello-thousands-separator ",")
  ; Display newest email up top.
  (setq notmuch-search-oldest-first nil))

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

(defun l/toggle-tag (tag &optional thread)
  "toggle a tag for message"
  (let
    ((f
      (if (string= major-mode "notmuch-tree-mode")
        (if thread
          'notmuch-tree-tag-thread
          'notmuch-tree-tag)
        'notmuch-show-tag)))
    (if (member tag (notmuch-show-get-tags))
      (funcall f `( ,(concat "-" tag) ))
      (funcall f `( ,(concat "+" tag) )))))

(defhydra hydra-notmuch-show (:foreign-keys warn)
  "notmuch-show"
  ("d" (lambda () (interactive) (l/toggle-tag "trash")) "(un)delete")
  ("f" (lambda () (interactive) (l/toggle-tag "flagged")) "(un)flag")
  ("q" nil "exit" :exit t))

(provide 'l-notmuch)

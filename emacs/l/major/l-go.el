(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook 'l/go-setup))

(defun l/go-setup ()
  (advice-add 'evil-fill :around #'l/fill-region)
  ; FlyCheck will automatically pick up the "golint" tool if it is installed.
  (flycheck-mode)
  ; Run gofmt upon save.
  (add-hook 'before-save-hook #'gofmt-before-save))

(defun l/fill-region (orig-fun &rest args)
  ;(message "evil-fill called with args %S" args)
  (let*
    (
      (use-fill-paragraph (equal major-mode 'go-mode))
      (res
        (if use-fill-paragraph
          (fill-paragraph)
          (apply orig-fun args))))
    ;(message
    ;  (concat
    ;    (if use-fill-paragraph "fill-paragrpah" "evil-fill")
    ;    " returned %S") res)
    res))

(provide 'l-go)

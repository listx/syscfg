(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook 'l/go-setup))

(defun l/go-setup ()
  ; FlyCheck will automatically pick up the "golint" tool if it is installed.
  (flycheck-mode)
  ; Run gofmt upon save.
  (add-hook 'before-save-hook #'gofmt-before-save))

(provide 'l-go)

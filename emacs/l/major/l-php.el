(use-package php-mode
  :config
  ; PHP-mode: disable default keybindings.
  (with-eval-after-load "php-mode"
    (define-key php-mode-map (kbd "<tab>") 'nil)))

(provide 'l-php)

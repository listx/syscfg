(use-package hiwin
  :config
  ; Darken inactive windows.
  (hiwin-activate)
  (set-face-background 'hiwin-face
    (if (display-graphic-p) "#ded6c5" "gray16")))

(provide 'l-hiwin)

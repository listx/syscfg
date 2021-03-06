; When https://github.com/bazelbuild/emacs-bazel-mode is ready, deprecate this.
(use-package bazel-mode
  :mode (
    (".*/BUILD\\'" . bazel-mode)
    (".*/BUILD\\.bazel\\'" . bazel-mode)
    (".*/WORKSPACE\\'" . bazel-mode))
  :config
  (add-hook 'bazel-mode-hook 'l/bazel-setup)
  (add-to-list 'auto-mode-alist '(".*/BUILD\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '(".*/BUILD\\.bazel\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '(".*/WORKSPACE\\'" . bazel-mode)))

(defun l/bazel-setup ()
  (modify-syntax-entry ?- "w"))

(provide 'l-bazel)

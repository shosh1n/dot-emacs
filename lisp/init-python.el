;;; init-python.el  --- Working with python -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :defer t
  :straight (:build t)
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  :config
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  :general (hc/leader-major
             :keymaps 'python-mode-map
             :packages 'lsp-mode
             "l" '(:keymap lsp-command-map :which-key "lsp"))
  )

(provide 'init-python)

;;; init-python.el ends here

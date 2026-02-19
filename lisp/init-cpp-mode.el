;; init-cpp-mode.el  --- Working with cpp-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package cc-mode
  :straight (:type built-in)
  :defer t
  :init
  (put 'c-c++-backend 'safe-local-variable 'symbolp)
  :config
  (require 'compile)
  :general
  (hc/leader-major
   :keymaps 'c++-mode-map
   "l" '(:keymap lsp-command-map :which-key "lsp")
   :packages 'lsp-mode
   "r" '("c0DE: find references" . lsp-find-references)
   "f" '("c0DE: flycheck" . flycheck-list-errors)
   "m" '("c0DE: ui-menu" . lsp-ui-imenu)
   "c" '("c0DE: auto-correct some erros" . lsp-execute-code-action)
   "R" '("c0DE: RENAME" . lsp-rename)
   "Q" '("c0DE: workspace-shutdown" . lsp-workspace-shutdown)
   "q" '("c0DE: workspace-start" . lsp-workspace-restart)
   "o" '("c0DE: find references " . 'lsp-find-references)
   "H" '("c0DE: show call hierarchy " . lsp-treemacs-call-hierarchy)
   )
  )

(use-package cmake-mode
  :defer t
  :straight (:build t))
(use-package cmake-font-lock
  :defer t
  :after cmake-mode
  :straight (:build t))

(use-package modern-cpp-font-lock
  :straight (:build t)
  :defer t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package clang-format+
  :straight (:build t)
  :defer t
  :init
  (add-hook 'c-mode-common-hook #'clang-format+-mode))
(provide 'init-cpp-mode)

;;; init-cpp-mode.el ends here

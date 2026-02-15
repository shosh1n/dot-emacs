;;; init-rusitc.el  --- Working with rustic -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package rustic
  :straight t
  :config
  (setq eldoc-echo-area-prefer-doc-buffer nil)
;;  (setq lsp-eldoc-hook nil)
;;  (setq lsp-enable-symbol-highlighting nil)
;;  (setq lsp-signature-auto-activate nil)
  (setq rustic-format-on-save t)

  :general
  (hc/leader 'override
    :infix "m"
    ""  '(nil :which-key "rustic (for now - later mode dependant")
    "m" '("c0DE: ui-menu" . lsp-ui-imenu)
    "r" '("c0DE: find references" . lsp-find-references)
    "f" '("c0DE: flycheck" . flycheck-list-errors)
    "c" '("c0DE: auto-correct some erros" . lsp-execute-code-action)
    "R" '("c0DE: RENAME" . lsp-rename)
    "Q" '("c0DE: workspace-shutdown" . lsp-workspace-shutdown)
    "q" '("c0DE: workspace-start" . lsp-workspace-restart)
    "S" '("c0DE: status of rust analyzer" . lsp-rust-analyzer-status)
    )

  )
(provide 'init-rustic)
;;; init-rustic ends here

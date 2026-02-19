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
  (hc/leader-major
    :keymaps 'rustic-mode-map
    :packages 'rustic
    "b"  '(:ignore t :which-key "build")
    "bb" #'rustic-cargo-build
    "bB" #'rustic-cargo-bench
    "bc" #'rustic-cargo-check
    "bC" #'rustic-cargo-clippy
    "bd" #'rustic-cargo-doc
    "bf" #'rustic-cargo-fmt
    "bn" #'rustic-cargo-new
    "bo" #'rustic-cargo-outdated
    "br" #'rustic-cargo-run
    "t"  '(:ignore t :which-key "cargo test")
    "ta" #'rustic-cargo-test
    "ta" #'rustic-cargo-test
   )
  )
(provide 'init-rustic)
;;; init-rustic ends here

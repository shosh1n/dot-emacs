;;; init-rust-mode.el  --- Working with rust-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :straight t
  :init
  (setq lsp-rust-analyzer-store-path "/home/shoshin/.cargo/bin/rust-analyzer")
  ;;(setq rust-mode-treesitter-derive t)
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (let ((dot-cargo-bin (expand-file-name "~/.cargo/bin/")))
    (setq rust-rustfmt-bin (concat dot-cargo-bin "rustfmt")
          rust-cargo-bin (concat dot-cargo-bin "cargo")
          rust-format-on-save t)))

(provide 'init-rust-mode)

;;; init-rust-mode.el ends here

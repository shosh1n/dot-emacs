;; init-nix-mode.el  --- Working with nix-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nix-mode
  :defer t
  :straight (:build t)
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  )
(provide 'init-nix-mode)

;;; init-nix-mode.el ends here

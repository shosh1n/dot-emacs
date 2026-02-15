;; init-smartparens.el  --- Working with smartparens -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package smartparens
  :defer t
  :straight (:build t)
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

(provide 'init-smartparens)

;;; init-smartparens.el ends here

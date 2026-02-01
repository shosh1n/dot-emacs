;;; init-glsl-mode.el  --- Working with glsl -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package glsl-mode
  :straight (:build t)
  :config
  (eval-after-load "glsl-mode"
    '(dolist (pattern '("\\.fs\\'" "\\.vs\\'"))
       (add-to-list 'auto-mode-alist (cons pattern 'glsl-mode))))
  )

(provide 'init-glsl-mode)

;;; init-glsl-mode.el ends here

;;; init-yasnippet.el  --- Working with yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package yasnippet
  :defer t
  :straight (:build t)
  :init
  (yas-global-mode)
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet
  :straight (:build t))



(use-package yatemplate
  :defer t
  :after yasnippet
  :straight (:build t))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here

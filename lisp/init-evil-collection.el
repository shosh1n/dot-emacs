;;; init-evil-collection.el  --- Working with evil-collection -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init '(calendar dired calc ediff dashboard bookmark magit))
  )

(provide 'init-evil-collection)

;;; init-evil-collection.el ends here

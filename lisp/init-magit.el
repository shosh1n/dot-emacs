;;; init-magit.el  --- Working with magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :straight (:build t)
  :defer t
  :init
  (setq forge-add-default-bindings nil)
  )

(provide 'init-magit)

;;; init-magit.el ends here

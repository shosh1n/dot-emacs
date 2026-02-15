;;; init-magit.el  --- Working with magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :straight (:build t)
  :defer t
  :init
  (setq forge-add-default-bindings nil)
  :general
  (hc/leader
   :infix "g"
   ""  '(nil :which-key "... magic/git!")
   "g" '("menu " . magit)
   )
  )

(provide 'init-magit)

;;; init-magit.el ends here

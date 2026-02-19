;; init-treemacs.el  --- Working with treemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treemacs
  :defer t
  :straight (:build t)
  :init
  (hc/leader
  :infix "t"
  "" '(nil :which-key "treemacs")
  "o" '("treemacs window " . treemacs-select-window)
  )
  )
(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(provide 'init-treemacs)

;;; init-treemacs.el ends here

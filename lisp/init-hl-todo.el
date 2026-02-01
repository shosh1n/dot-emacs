;;; init-hl-todo.el  --- Working with hl-todo -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; TODO
(use-package hl-todo
  :straight t
  :hook
  ((prog-mode . hl-todo-mode)
   (yaml-mode . hl-todo-mode))
  :custom
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")))
  (with-eval-after-load 'magit
    (add-hook 'magit-log-wash-summary-hook
              #'hl-todo-search-and-highlight t)
    (add-hook 'magit-revision-wash-message-hook
              #'hl-todo-search-and-highlight t))
  )
(provide 'init-hl-todo)

;;; init-hl-todo.el ends here

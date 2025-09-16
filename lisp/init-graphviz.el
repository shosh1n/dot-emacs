;;; init-graphviz.el  --- Working with graphviz -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package graphviz-dot-mode
  :straight t
  :config
  (setq graphviz-dot-indent-width 4)
  :hook
  (graphviz-dot-mode . fycheck-mode))


(provide 'init-graphviz)

;;; init-graphviz.el ends here

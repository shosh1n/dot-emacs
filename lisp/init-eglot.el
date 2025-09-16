;;; init-eglot.el  --- Working with eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :straight t
  (eglot
   :type built-in)
  :config
  (add-to-list 'eglot-server-programs
   '(rust-mode . ("rust-analyzer" :initializationOptions
                 (:procMacro (:enable t)
                  :cargo (:buildScripts (:enable t)
                                        :features "all")))))
   (add-to-list 'eglot-server-programs
     '(python-mode . ("/home/shoshin/miniconda3/bin/basedpyright-langserver" "--stdio"))))

(provide 'init-eglot)
;;; init-eglot.el ends here

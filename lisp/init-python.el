;;; init-python.el  --- Working with python -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :defer t
  :straight (:build t)
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("[./]flake8\\'" . conf-mode)
         ("/Pipfile\\'"   . conf-mode))
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-local-vars-hook #'lsp)
  :config
  (when (and (executable-find "python3.14")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  :general
  (hc/leader-major
    :keymaps 'python-mode-map
    "l" '(:keymap lsp-command-map :which-key "lsp")
    :packages 'lsp-mode
    "r" '("c0DE: find references" . lsp-find-references)
    "f" '("c0DE: flycheck" . flycheck-list-errors)
    "m" '("c0DE: ui-menu" . lsp-ui-imenu)
    "c" '("c0DE: auto-correct some erros" . lsp-execute-code-action)
    "R" '("c0DE: RENAME" . lsp-rename)
    "Q" '("c0DE: workspace-shutdown" . lsp-workspace-shutdown)
    "q" '("c0DE: workspace-start" . lsp-workspace-restart)
    "o" '("c0DE: find references " . 'lsp-find-references)
    "H" '("c0DE: show call hierarchy " . lsp-treemacs-call-hierarchy)
    )
  )

(use-package pytest
  :defet t
  :straight (:build t)
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytests-pdb-all
             pytest-last-failed
             pytest-pdb-last-failed
             pytest-module
             pytest-pdb-module)
  :config
  (add-to-list 'pytest-project-root-files "setup.cfg")
  (hc/leader-major
   :keymaps 'python-mode-map
   :infix "t"
   :packages 'pytest
   ""  '(:ignore t :which-key "test")
   "a" #'python-pytest
   "f" #'python-pytest-file-dwim
   "F" #'python-pytest-file
   "t" #'python-pytest-function-dwim
   "T" #'python-pytest-function
   "r" #'python-pytest-repeat
   "p" #'python-pytest-dispatch)
  )

(use-package poetry
  :defer t
  :straight (:build t)
  :commands (poetry-venv-toggle
             poetry-tracking-mode)
  :config
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode)
  )

(use-package pip-requirements
  :defer t
  :straight (:build t))

(use-package pippel
  :defer t
  :straight (:build t)
  :general
  (hc/leader-major
   :keymaps 'python-mode-map
   :packages 'pippel
   "P" #'pippel-list-packages))

(use-package pipenv
  :defer t
  :straight (:build t)
  :commands (pipenv-activate
             pipenv-deactivate
             pipenv-shell
             pipenv-open
             pipenv-install
             pipenv-uninstall)
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  :general
  (hc/leader-major
   :keymaps 'python-mode-map
   :packages 'pipenv
   :infix "e"
   ""  '(:ignore t :which-key "pipenv")
   "a" #'pipenv-activate
   "d" #'pipenv-deactivate
   "i" #'pipenv-install
   "l" #'pipenv-lock
   "o" #'pipenv-open
   "r" #'pipenv-run
   "s" #'pipenv-shell
   "u" #'pipenv-uninstall))
(use-package pyenv
  :defer t
  :straight (:build t)
  :config
  (add-hook 'python-mode-hook #'pyenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyenv-virtual-env-name (" venv:" pyenv-virtual-env-name " "))
               'append))

(use-package pyenv-mode
  :defer t
  :after python
  :straight (:build t)
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions)
  :general
  (hc/leader-major
    :packages 'pyenv-mode
    :keymaps 'python-mode-map
    :infix "v"
    "u" #'pyenv-mode-unset
    "s" #'pyenv-mode-set))

(use-package pyimport
  :defer t
  :straight (:build t)
  :general
  (hc/leader-major
    :packages 'pyimport
    :keymaps 'python-mode-map
    :infix "i"
    ""  '(:ignore t :which-key "imports")
    "i" #'pyimport-insert-missing
    "r" #'pyimport-remove-unused))

(use-package py-isort
  :defer t
  :straight (:build t)
  :general
  (hc/leader-major
   :keymaps 'python-mode-map
   :packages 'py-isort
   :infix "i"
   ""  '(:ignore t :which-key "imports")
   "s" #'py-isort-buffer
   "R" #'py-isort-region))

(use-package counsel-pydoc
  :defer t
  :straight (:build t))

(use-package cython-mode
  :defer t
  :straight (:build t)
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s")
  :general
  (hc/leader-major
   :keymaps 'cython-mode-map
   :packages 'cython-mode
   :infix "c"
   ""  '(:ignore t :which-key "cython")
   "c" #'cython-compile))

(use-package flycheck-cython
  :defer t
  :straight (:build t)
  :after cython-mode)

(use-package blacken
  :defer t
  :straight (:build t)
  :init
  (add-hook 'python-mode-hook #'blacken-mode))

(use-package lsp-pyright
  :after lsp-mode
  :straight (:build t)
  :custom (lsp-pyright-langserver-command "basedpyright")
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(provide 'init-python)

;;; init-python.el ends here

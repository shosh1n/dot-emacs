;;; init-ltex.el  --- Working with ltex -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-ltex
  :straight t
  :after lsp-mode
  :init
  (setq lsp-ltex-version "16.0.0")
  :hook ((org-mode . lsp)
         (text-mode  . lsp)
         (markdown-mode . lsp)
         (LaTeX-mode . lsp))
  :config
  (setq lsp-ltex-language "de-DE")
  (setq lsp-ltex-mother-tongue "de-DE")
  (setq lsp-ltex-file-extension-whitelist '("org" "md" "tex"))
  (setq lsp-ltex-disabled-rules '(["MORFOLOGIK_RULE_DE"]))
  (setq lsp-log-io t)
  )

(defun my/lsp-ltex-set-documentKind ()
  (cond
   ((derived-mode-p 'org-mode)
    (setq-local lsp-ltex-documentKinds ["plaintext"]))
   ((derived-mode-p 'markdown-mode)
    (setq-local lsp-ltex-documentKinds ["markdown"]))
   ((derived-mode-p 'LaTeX-mode)
    (setq-local lsp-ltex-documentKinds ["latex"]))
   (t
    (setq-local lsp-ltex-documentKinds ["plaintext"]))))

(add-hook 'lsp-mode-hook #'my/lsp-ltex-set-documentKind)

(provide 'init-ltex)

;;; init-ltex.el ends here

;;; init-lsp.el  --- Working with lsp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun lsp--string-vector-p (canidate)
  "Return non-nil if CANIDATE is a vector and every element of it is a string"
  (and
   (vectorp canidate)
   (seq-every-p #'stringp canidate)
   ))

(define-widget 'lsp-string-vector 'lazy
  "A vector of zero or more elements, every element of which is a string, that adheres to a JSON array."
  :offset 4
  :tag "Vector"
  :type '(restricted-sexp
          :match-alternatives (lsp--string-vector-p)))

(defcustom lsp-clangd-executeable ["clangd-18"
                                      "clangd-17"
                                      "clangd-16"
                                      "clangd-15"
                                      "clangd-14"
                                      "clangd-13"
                                      "clangd"]
  "List of executeable names to search for when to run clangd. use `executeable-fund."
  :risky t
  :type 'lsp--string-vector)

(defcustom lsp-clangd-args '("-j=4"
                             "--background-index"
                             "--log=error"
                             "--clang-tidy")
  "Extra arugments for the clangd executeable."
  :risky t
  :type '(repeat string)
  )

(use-package lsp-mode
  :straight t
  :init
  (setq-default lsp-clients-clangd-executable
        (seq-find #'executable-find lsp-clangd-executeable))
  ;; set prefix for lsp-command-keymap
  :hook ((python-mode . lsp)
         (org-mode . lsp)
         (text-mode . lsp)
         (markdown-mode . lsp)
         (LaTeX-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (c++-mode . lsp)
         (c-mode . lsp)
         (objc-mode . lsp)
         )
  :custom
  (lsp-clients-clangd-args lsp-clangd-args)
  (setq lsp-cmake-server-command "/home/shoshin/miniconda3/bin/cmake-language-server /home/shoshin/.local/bin/cmake-language-server")
  )


(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
  )
(use-package lsp-treemacs
  :defer t
  :straight (:build t))


(provide 'init-lsp)
;;; init-lsp.el ends here

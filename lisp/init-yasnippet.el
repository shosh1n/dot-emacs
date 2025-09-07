;;; init-yasnippet.el  --- Working with yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Yasnippet settings
  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!


(use-package yasnippet
  :defer t
  :straight (:build t)
  :init
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
         ;;(post-self-insert . my/yas-try-expandning-auto-snippets)) ;; <- TODO: ltu
  :config
  (yas-global-mode 1)
  ;;(cl-pushnew '(yasnippet backquote-change)      ;; <- TODO: ltu
  ;;                        warning-supress-types
  ;;                        :test #'equal)
  (setq yas-triggers-in-field t)
  ;;(defun my/yas-try-expanding-auto-snippets ()
  ;;  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
  ;;    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
  ;;      (yas-expand))))
  )

(use-package yasnippet-snippets
  :defer t
  :after yasnippet
  :straight (:build t))

(use-package yatemplate
  :defer t
  :after yasnippet
  :straight (:build t))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here

;;; init-cape.el --- Description -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cape
  :straight t
  :hook ((emacs-lisp-mode . kb/cape-capf-setup-elisp)
         (c-mode-hook . kb/lsp-corfu-capf)
         (c++-mode-hook . my/lsp-corfu-capf)
         (cmake-mode . my/lsp-corfu-capf)
         )
  :custom
  (cape-dabbrev-min-length 5)
  :init
  (defun kb/cape-capf-ignore-keywords-elisp(cand)
    ;; taken from: https://kristofferbalintona.me/posts/202203130102/
    (or (not (keywordp cand))
        (eq (char-after (car completion-in-region--data)) ?:)))
  (defun kb/cape-capf-setup-elisp ()
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-properties
                  #'elisp-completion-at-point
                  :predicate #'kb/cape-capf-ignore-keywords-elisp)
                 #'cape-elisp-symbol
                 #'cape-file)))

    ;;(require 'company-yasnippet)
    ;;(add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet)))

(defun my/lsp-corfu-capf ()
  (setq-local completion-at-point-functions
              (list #'lsp-completion-at-point
                    #'cape-dabbrev)))

)

(provide 'init-cape)
;;; init-cape.el ends here

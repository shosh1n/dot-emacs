;; init-company.el  --- Working with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package company
  :defer t
  :straight (:build t)
  :custom
  (company-idle-delay 0.5) ;; how long to wait before pop-up
  :general
   (:keymaps 'company-active-map
   "<right>" 'company-select-next
   "<left>"  'company-select-previous)
   (:keymaps 'company-mode-map
             "TAB" 'tab-indent-or-complete)
   :config
   (defun company-yasnippet-or-completion ()
   (interactive)
   (or (do-yas-expand)
       (company-complete-common)))
   (defun check-expansion ()
     (save-excursion
       (if (looking-at "//>") t
         (backward-char 1)
         (if (looking-at "\\.") t
           (backward-char 1)
           (if (looking-at "::") t nil)
           )
         )
       )
     )
   (defun do-yas-expand()
     (let (minibufferp)
       (minibuffer-complete)
       (if (or (not yas/minor-mode)
               (null (do-yas-expand)))
           (if (check-expansion)
               (company-complete-common)
             (indent-for-tab-command)
             )
         )
       )
     )
)


(provide 'init-company)

;;; init-company.el ends here

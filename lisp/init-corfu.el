;;; init-corfu.el  --- Working with corfu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-cycle nil)
  (corfu-quit-at-boundary nil)
  ;;(corfu-quit-no-match 'separator)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-min-width 80)
  (corfu-max-width 80)
  (corfu-scroll-margin 4)
  (corfu-preview-current 1)
  (corfu-preselect 'prompt)
  ;;(corfu-on-exact match nil)
  :hook
  ((prog-mode . corfu-mode)
   (shell-mode . corfu-mode)
   (eshell-mode . corfu-mode)
   (python-mode . corfu-mode))
  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-p" #'corfu-previous
            "C-n" #'corfu-next
            "<return>" #'corfu-insert
            "M-d" #'corfu-show-documentation
            "M-l" #'corfu-show-location)
  :config
  (setq completion-styles '(orderless basic flex))

  )

(provide 'init-corfu)

;;; init-corfu.el ends here

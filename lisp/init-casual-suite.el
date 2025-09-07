;;; init-casual-suite.el --- Description -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package casual-suite
  :straight (:build t)
  :after (general evil)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "s") #'casual-dired--sort-by-kind)
  :general
  (hc/leader
    :keymaps 'dired-mode-map
    "c" #'casual-dired-tmenu)
  )
(provide 'init-casual-suite)
;;; init-casual-suite.el ends here

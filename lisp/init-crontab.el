;;; init-crontab.el  --- Working with crontab -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package crontab-mode
  :straight t
  (add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?'\\"))
(provide 'init-crontab)

;;; init-crontab.el ends here

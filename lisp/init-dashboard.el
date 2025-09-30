;;; init-dashboard.el  --- Working with dashboard -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package dashboard
  :straight t
  :demand t
  :after
  (org org-agenda general)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 3)
                          (projects . 3)
                          (agenda . 10)
                          (registers . 5)))
  (setq dashboard-startup-banner "~/.config/doom/samus.png")
  (setq dashboard-banner-logo-title "Hi!"
        dashboard-show-shortcuts t
        dashboard-set-navigator t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-week-agenda t
        dashboard-filter-agenda-entry
        'dashboard-no-filter-agenda )
  (setq dashboard-match-agenda-entry "TODO=\"TODO\"|TODO=\"IN-PROGRESS\"")
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (add-hook 'org-finalize-agenda-hook #'dashboard-refresh-buffer)
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  )

(provide 'init-dashboard)

;;; init-dashboard.el ends here

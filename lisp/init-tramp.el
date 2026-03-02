;;; init-tramp.el  --- Working with crontab -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package tramp
  :straight (tramp :type built-in :build t)
  :config
  (add-to-list 'tramp-remote-process-environment
             (format "DISPLAY=%s" (getenv "DISPLAY")))
  (custom-set-variables
   '(tramp-default-method "rsync")
   '(tramp-default-host "172.24.129.14"))
  (setopt tramp-backup-directory-alist nil
          backup-inhibited t
          auto-save-default nil
          tramp-keep-connection t
          tramp-use-ssh-controlmaster-options nil
          remote-file-name-inhibit-cache nil
          tramp-completion-reread-directory-timeout nil
          )
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  ;;(connection-local-set-profiles
  ;; '(:application tramp :protocol "rsync")
  ;; 'remote-direct-async-process)
  ;; (connection-local-set-profiles
  ;;  '(:application tramp :protocol "ssh")
  ;;  'remote-direct-async-process)
  (add-to-list 'tramp-connection-properties (list "/ssh:" "direct-async" t)
               )



  (require 'tramp-sh)
  (setq tramp-remote-path (append tramp-remote-path
                                  '("/home/hermanns/.local/bin/" "/home/hermanns/mambaforge/bin")))
)

(provide 'init-tramp)
;;; init-tramp.el ends here

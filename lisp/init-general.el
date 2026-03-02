;;; init-general .el  --- Working with crontab -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/orga ()
  (interactive)
  (find-file-existing "~/Documents/Applications/")
  )
(defun si/open-bob-file()
  (interactive)
  (find-file (concat "/ssh:procs@172.16.8.201:/opt/sielaff" (read-string "Enter file path: ")))
  (message "connecting to Burgoberbach ...!"))
(defun si/open-lab193-file()
  (interactive)
  (find-file (concat "/ssh:procs@172.24.3.193:/opt/sielaff" (read-string "Enter file path: ")))
  (message "connecting to Burgoberbach ...!"))

(defun si/open-remote-file()
  "open a remote file using tramp."
  (interactive)
  (find-file (concat "/ssh:hermanns@gpu:/home/hermanns" (read-string "Enter file path: ")))
  (message "connecting to gpu-server ...!"))

(defun hc/open-work-folder()
  (interactive)
  (dired "~/src/__workbench/__scratch")
  (message  "%s to working directory!"
            (nth (random 7) '(meandering walking hiking going pathing going Si-lining))))

(defun hc/open-doc()
  (interactive)
  (dired "~/Dokumente/documentation/Bildverarbeiter/Organisatorisches.org")
  (message  "%s open si-doc!"))

(use-package general
  :straight (general :type git :host github :repo "noctuid/general.el")
  :demand t
  :config
 (general-create-definer hc/undefine
    :keymaps 'override
    :states '(normal emacs))
  (general-create-definer hc/leader
    ;; :prefix hc-key
    ;; it's my leader key which should be available everywhere!
    :prefix "SPC"
    :states '(normal motion)
    :keymaps 'override
    )
  (general-create-definer hc/leader-major
    :prefix "s-SPC"
    :states '(normal insert visual emacs)
    :keymaps 'override
    )
  ;;(hc/leader-major
  ;;  "." '("into eldoc" . eldoc-doc-buffer)
  ;;  )
    (hc/leader
   :infix "f"
   ""  '(nil :which-key "file operations")
   "f" '("find a file" . find-file)
   "r" '("open recent files" . recentf-open-files)
   "d" '("📁 dired" . dired-jump)
   "p" '("my config.el" . goto-user-config)
   )
   (hc/leader
   :infix "SPC"
   "" '(nil :which-key "emacs stuff")
   "r" '("reload emacs-configuration" . reload-emacs-config)
   )
   (hc/leader
   :infix "s"
   "" '(nil :which-key "buffer operations")
   "s" '("goto scratch buffer" . scratch-buffer)
   "a" '("erease current buffer" . erase-buffer)
   "e" '("goto scratch buffer" . eval-buffer)
   )
   (hc/leader
   :infix "h"
   "" '(nil :which-key "helpful stuff")
   "v" '("describe variable!" . describe-variable)
   "f" '("describe function!" . describe-function)
   "k" '("describe key!" . describe-key)
   "m" '("describe mode!" . describe-mode)
   "c" '("describe command!" . describe-command)
   )
   (hc/leader
    :infix "a"
    ""  '(nil :which-key "orga operational!")
    "a" '("open job-application base " . my/orga)
    )

   (general-define-key
    :states 'normal
    :keymaps 'dired-mode-map
    "<return>" 'dired-find-file
    "^" 'dired-up-directory
    "m" 'diredp-mark
    "u" 'diredp-unmark-region-files
    "d" 'diredp-flag-region-files-for-deletion
    "x" 'dired-do-delete
    "+" 'dired-create-directory
    "c" 'diredp-do-copy-recursive
    "f" 'diredp-create-file-here
    "U" 'diredp-unmark-all-files-recursive
    "M" 'diredp-mark-files-tagged-not-all
    "(" 'dired-hide-details-mode
     )
   (general-define-key
    :states '(normal insert visual emacs motion)
    :keymaps '(override minibuffer-local-map)
    "C-S-v" 'consult-yank-from-kill-ring
    )
   (hc/leader
    :infix "l"
     "w" 'hc/open-work-folder
     "l" 'si/open-lab193-file
     "g" 'si/open-remote-file
     "o" 'si/open-bob-file
     "s" 'hc/open-doc)





      ;;(general-define-key
   ;; :states '(insert)
   ;; "C-k" #'lsp-ui-doc-focus-frame)
   ;;(general-define-key
   ;; :states '(normal insert)
   ;; :keymaps '(override minibuffer-local-map insert-mode-map)
   ;; "M-p" 'completion-at-point
   ;; )
;;(defun open-remote-file()
;;  "open a remote file using tramp."
;;  (interactive)
;;  (find-file (concat "/ssh:sanakan@172.104.241.26:/" (read-string "Enter file path: ")))
;;  (message "connecting to your server ...!"))
;;
;;   (evil-global-set-key 'normal (kbd "SPC o w") 'hc/open-work-folder)
;;
   )


(provide 'init-general)
;;; init-general .el ends here

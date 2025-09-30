;;; init-dired.el  --- Working with dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;NOTE: dired-mark-suffix
;; dired-extension
;;F find marked


(use-package dired
  :straight (:type built-in)
  :after(evil)
  :demand t
  :config
  (load (expand-file-name (concat user-emacs-directory "elisp/dired+/dired+")))
  (require 'dired-x)
  (require 'dired+)
  (setq dired-auto-revert-buffer 1
        dired-dwim-target t
        dired-listing-switches "-ahl --group-directories-first")
  ;; dired settings
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-omit-mode 1)))
  ;;(add-hook 'dired-mode-hook
          ;;(lambda ()
          ;;  (evil-define-key 'normal dired-mode-map
          ;;    "q" #'diredp-quit-window-kill
          ;;    "s" #'diredp-sort-arbitrary-command))
          ;;)

  )

(use-package nerd-icons-dired
  :after (dired)
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :after (dired)
  :straight t
  :hook
  (dired-mode . diredfl-global-mode))



                                        ;(general-define-key
; :keymaps '(
; "M-K" '(lambda () (interactive) (helm-select-nth-action 4)))

;(make-sparse-keymap "the string")

;(string . real-binding)
;(itemstring help . real-binding)
;(defvar menu-bar-replace-menu (make-sparse-keymap "Replace"))
;(define-key menu-bar-replace-menu [tags-repl-continue]
;  '(menu-item "Continue Replace" multifile-continue
;              :help "Continue last tags replace operation"))
;(define-key menu-bar-replace-menu [tags-repl]
;  '(menu-item "Replace in tagged files" tags-query-replace
;              :help "Interactively replace a regexp in all tagged files"))
;(define-key menu-bar-replace-menu [separator-replace-tags]
;  '(menu-item "--"))
;(define-key menu-bar-edit-menu [replace]
;  (list 'menu-item "Replace" menu-bar-replace-menu))
;;; …
;(define-key global-map [C-c-C-s]
;   menu-bar-replace-menu)

(provide 'init-dired)
;;; init-dired.el ends here

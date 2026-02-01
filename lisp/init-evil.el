;;; init-evil.el --- Working with evil -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :straight (evil :type git :host github :repo "emacs-evil/evil")
  :init
  (setq evil-want-integration t) ;; optional sincce it's already set to t by default
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :custom
  (evil-insert-state-cursor '(bar "Green"))
  (evil-visual-state-cursor '(box "White"))
  (evil-normal-state-cursor '(box "Pink"))
  (evil-want-minibuffer t)
  :hook (after-init . evil-mode)
  :config
   (evil-mode 1)
   (evil-global-set-key 'motion "j" 'evil-next-visual-line)
   (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (setq evil-normal-state-modes
        (append evil-emacs-state-modes
        evil-insert-state-modes
        evil-normal-state-modes
        evil-motion-state-modes))
  ;:general
  ;(general-define-key
  ;  :states 'normal
  ; "M-n" 'evil-next-mark
    ;"þ" 'evil-previous-mark
    ;"µ" 'evil-set-marker
  ;  )
)

(provide 'init-evil)
;;; init-evil.el ends here

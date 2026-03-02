;; init-winpulse.el  --- Working with winpulse -*- lexical-binding: t -*-
;;; Commentary: flashed active selected window
;;; Code:

(use-package winpulse
  :straight t
  :straight (winpulse :type git :host github :repo "https://github.com/xenodium/winpulse")
  :config
  (winpulse-mode +1))
(provide 'init-winpulse)

;;; init-winpulse.el ends here

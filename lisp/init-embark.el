;;; init-embark.el  --- Working with embark -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package embark
  ;;:after consult
  :straight (:build t)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)


  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :general
  (hc/leader
    :states 'normal
    :infix "e"
    "" '(nil :which-key "embark..")
    "a" '("embark-act" . embark-act)
    "A" '("embark-act-all" . embark-act-all)
    "c" '("embark-collect" . embark-collect)
    "d" '("embark do what I mean" . embark-dwim)
    "e" '("embark export" . embark-export)
    )
  )

(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  ;;:after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
;;; init-embark.el ends here

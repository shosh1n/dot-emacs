;;; init-denote.el  --- Working with denote -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package denote
  :straight t
  :after (dired)
  :hook (dired-mode . denote-dired-mode)
  :general
  (:states 'normal
           (hc/leader
             :infix "d"
             "" '(nil :which-key "denote ..")
             "SPC" '("denote" . denote)
             "r" '("rename-file" . denote-rename-file)
             "l" '("link file" . denote-link)
             "b" '("back links" . denote-backlinks)
             "d" '("denote dired" . denote-dired)
             "s" '("denote grep" . denote-grep)
             )
           )
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes"))
  (setq denote-known-keywords '("idea" "emacs" "politics" "economics" "game" "cpp"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (denote-rename-buffer-mode 1))

(provide 'init-denote)

;;; init-denote.el ends here

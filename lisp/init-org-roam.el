;;; init-org-roam.el  --- Working with org-roam -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-roam
  :straight
  ;;(:host github
  ;; :repo "org-roam/org-roam"
  ;; :files (:defaults "extensions/*"))

  :bind (
     ("C-c n l" . org-roam-buffer-toggle)
     ("C-c n f" . org-roam-node-find)
     ("C-c n r" . org-roam-node-insert)
     ("C-c n c" . org-roam-capture)
     ;;:map org-mode-map
     ;;("C-M-i" . completion-at-point)
     ;;:map org-roam-dailies-map
     ;;("Y" . org-roam-dailies-capture-yesterday)
     ;;("T" . org-roam-dailies-capture-tomorrow))
     ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;;(require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
  :custom
  (org-roam-directory (file-truename "~/.org/"))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  )

(use-package org-roam-ui
  :straight t
  ;;(org-roam
  ;; :type git
  ;; :host github
  ;; :repo "org-roam/org-roam-ui"
  ;; :branch "main"
  ;; :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
    org-roam-ui-follow t
    org-roam-ui-update-on-save t
    org-roam-ui-open-on-start t))

(use-package org-roam-bibtex
  :straight t
  ;;(org-roam
  ;; :type git
  ;; :host github
  ;; :repo "org-roam/org-roam-bibtex"
  ;; :files (".el" "out"))
  :after org-roam
  :config
  (setq bibtext-completion-library-path '("~/.org/arxiv.bib")))

(defun my/CreateBookEntry (dir)
  "Create a directory DIR if it does not exist and add a .dir-locals.el file."
  (interactive "FEnter directory name: ")  ;; Allow entering a new directory name

  ;; Ensure the main directory exists
  (unless (file-exists-p dir)
    (make-directory dir t)
    (message "Directory created: %s" dir))

  (let* ((absolute-dir (expand-file-name dir))  ;; Convert to absolute path
         (notes-dir (expand-file-name "notes" absolute-dir))  ;; Define 'notes' directory inside 'dir'
         (img-dir (expand-file-name "img" notes-dir))  ;; Define 'img' directory inside 'notes'
         (dir-locals-file (expand-file-name ".dir-locals.el" absolute-dir)))  ;; Define .dir-locals.el path

    ;; Create 'notes' directory if it doesn't exist
    (unless (file-exists-p notes-dir)
      (make-directory notes-dir t)
      (message "Created 'notes' directory in: %s" notes-dir))

    ;; Create 'img' directory inside 'notes' if it doesn't exist
    (unless (file-exists-p img-dir)
      (make-directory img-dir t)
      (message "Created 'img' directory in: %s" img-dir))

    ;; Create .dir-locals.el inside the main directory
    (unless (file-exists-p dir-locals-file)
      (with-temp-file dir-locals-file
        (insert "((nil . (\n")
        (insert (format "  (eval . (setq-local org-roam-directory %S))\n" notes-dir))
        (insert (format "  (eval . (setq-local org-roam-db-location %S))\n"
                        (expand-file-name "org-roam.db" notes-dir)))
        ;; Toggle org-roam-ui-mode
        (insert "  (eval . (unless (bound-and-true-p org-roam-ui-mode)\n")
        (insert "            (org-roam-ui-mode 1)))\n")
        (insert ")))\n"))  ;; Properly close the list
      (message ".dir-locals.el created in: %s" dir-locals-file))))

(provide 'init-org-roam)

;;; init-org-roam.el ends here

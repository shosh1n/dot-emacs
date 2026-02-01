;;; init-project.el  --- Working with project -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package project
  :straight (:build t)
  :general
  (hc/leader
    :states 'normal
    :infix "p"
    ""  '(nil :which-key "pROJECts!")
    "p" '("switch project" . project-switch-project)
    "b" '("switch project buffer" . project-switch-to-buffer)
    "d" '("project file browser" . project-dired)
    "SPC" '("project find file" . project-find-file)
    "g" '("project file browser" . project-vc-dir)
    "D" '("DELETE PROJECT FORM LIST" . project-forget-project)
    "K" '("KILLALL PROJECT BUFFERS" . project-kill-buffers)
    ;; consult stuff
    ;;""
    )


  )
(provide 'init-project)

;;; init-project.el ends here

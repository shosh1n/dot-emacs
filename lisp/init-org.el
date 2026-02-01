;;; init-org.el  --- Working with org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package org-modern
  :straight t
  :after org
  :config
  (global-org-modern-mode 1)
  )

;;(use-package org-modern-indent
;;  :straight (:package org-modern-indent
;;                      :type built-in
;;                      :local-repo "org-modern-indent"
;;                      :files ("org-modern-indent.el")
;;                      )
;;  :load-path "~/.emacs.d/elisp/org-modern-indent"
;;  :after (org org-modern)
;;  :config
;;  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
;;  )
(use-package engrave-faces
  :straight t
  )


(use-package org
  :straight t
  :after general ;;(engrave-faces general)
  ;;:hook ((org-mode . visual-line-mode)
         ;;)
  ;;:bind
  ;;(:map orgtbl-mode-map
  ;;      ("<tab>" . lazytab-org-table-next-field-maybe)
  ;;      ("TAB" . lazytab-org-table-next-field-maybe))
  ;;:init
  ;;(auto-fill-mode -1)
  ;;(org-num-mode -1)
  ;;(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  ;;(add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
  ;;  ;; Tabular environments using cdlatex
  ;;(add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
  ;;                                     "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
  ;;                                     lazytab-position-cursor-and-edit
  ;;                                     nil nil t))
  ;;(add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
  ;;                                     "\\begin{bmatrix} ? \\end{bmatrix}"
  ;;                                     lazytab-position-cursor-and-edit
  ;;                                     nil nil t))
  ;;(add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
  ;;                                     "\\begin{pmatrix} ? \\end{pmatrix}"
  ;;                                     lazytab-position-cursor-and-edit
  ;;                                     nil nil t))
  ;;(add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
  ;;                                      "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
  ;;                                     lazytab-position-cursor-and-edit
  ;;                                     nil t nil))
  :config
  (setq org-latex-compiler "xelatex"
        org-latex-packages-alist '(("" "amsmath" t))
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-skip-deadline-if-done t
        org-num-mode nil
        org-directory "~/org"
        org-agenda-files (list "~/Dropbox/orgzly"))
  (add-hook 'org-agenda-mode-hook
          (lambda ()
            (evil-define-key 'normal org-agenda-mode-map
              (kbd "q") #'org-agenda-quit)))
  (add-to-list  'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
  (setq org-capture-templates
        '(
          ;; general selection
          ("g" "General To-Do"
           entry (file+headline "~/Dropbox/orgzly/todos.org" "General Tasks")
           "* TODO [#%^{Priority|A|B|C}] %^{Title} :%^{Tag|private|work|activity}:%^g\n:Created: %U\nSCHEDULED:%^{Scheduled to begin}t \n%?"
           :empty-lines 0)
          ("c" "Code To-Do"
          entry (file+headline "~/Dropbox/orgzly/code.org" "Code Related Tasks")
          "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: ")
          ("m" "Meeting"
          entry (file+datetree "~/Dropbox/orgzly/meetings.org")
          "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
          :tree-type week
          :clock-in t
          :clock-resume t
          :empty-lines 0)
        ))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "BLOCKED(@b!)" "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)")
          ))
  (setq org-todo-keyword-faces
        '(("TODO"        . (:foreground "GoldenRod"  :weight bold ))
          ("IN-PROGRESS" . (:foreground "DeepPink"  :weight bold ))
          ("BLOCKED"     . (:foreground "Cyan"      :weight bold ))
          ("DONE"        . (:foreground "LimeGreen" :weight bold ))
          ("WAIT"        . (:foreground "DarkOrange" :weight bold ))
          ("WONT-DO"     . (:foreground "Cyan"       :weight bold ))))
  (setq org-tag-alist '(
                        ;; my selections
                        (:startgroup)
                        ("private" . ?p)
                        ("work" . ?w)
                        ("activity" . ?a)
                        (:endgroup)
                        ;; ticket types
                        (:startgroup)
                        ("@bug" . ?b)
                        ("@feature" . ?f)
                        ("@spike" . ?k)
                        ;; Ticket flags
                        ("@smooth" . ?s)
                        ("@urgent" . ?u)
                        ("@laidback" . ?l)
                        (:endgroup)
                        ;; derived
                        (:startgroup)
                        ("review" . ?R)
                        ("plan" . ?P)
                        ("idea" . ?I)
                        (:endgroup)
                        ;; Code TODO tags
                        (:startgroup)
                        ("sketch" . ?q)
                        ("rewrite" . ?r)
                        ("architect" . ?A)
                        ("streamline" . ?S)
                        (:endgroup)
                        ;; Special tags
                        (:startgroup)
                        ("critical" . ?X)
                        ("obstacle" . ?O)
                        ("note" . ?N)
                        (:endgroup)
                        ;; Meetings tags
                        (:startgroup)
                        ("HR" . ?h)
                        ("general" . ?g)
                        ("meeting" . ?m)
                        ("misc" . ?z)
                        (:endgroup)
                        ;; Work Log Tags
                        ("accomplishment" . ?y)
                        ;;(:endgroup . nil)
                        ))
  :general
  (hc/leader
    :states 'normal
    :infix "o"
    ""  '(nil :which-key "org-mode")
    "a" '("agenda" . org-agenda)
    "l" '("org-store a link" . org-store-link)
    "c" '("capture some tasks..." . org-capture))
  (general-define-key
   :states 'normal
   :keymaps 'org-capture-mode-map
   "RET" "C-c C-c")



;;  (setq org-agenda-category-icon-alist
;;      `(;;("work"       ,(list (all-the-icons-material "work" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
;;        ;;("activity"   ,(list (all-the-icons-faicon "bicycle" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
;;        ;;("meeting"    ,(list (all-the-icons-material "event" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
;;        ;;("@research"  ,(list (all-the-icons-material "science" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
;;        ("general"    ,(list (nerd-icons-mdicon "nf-md-tank" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
;;        ))

  )


(require 'org-tempo)

;;(use-package valign
;;  :straight (:package valign
;;                      :type built-in
;;                      :local-repo "valign"
;;                      :files ("valign.el")
;;                      )
;;  :load-path "~/.emacs.d/elisp/valign"
;;  :after (org)
;;  :config
;;  (add-hook 'org-mode-hook #'valign-mode)
;;  )

(use-package org-contrib
  :after (org)
  :straight t
  :init
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
  )
;;(use-package oauth2
;;  :straight t
;; )
;;(use-package org-gcal
;;  :straight t
;;  :after (org)
;;  :init
;;  :config
;;  (require 'plstore)
;;  (add-to-list 'plstore-encrypt-to '("488B0DAA0B420C3CEDC30F8308ADA8CDF127AC03"))
;;  (setq org-gcal-fetch-file-alist '(("hermannschris@googlemail.com" . "~/Dropbox/cal/dates.org")))
;;  (setq epg-pinentry-mode 'loopback)
;;  )
(use-package org-gcal
  :after org
  :demand t   ;; ensure it's loaded immediately, not deferred
  :config
  (require 'plstore)
  (setq auth-sources '("~/.emacs.d/.authinfo.gpg"))
  (setq epg-pinentry-mode 'loopback)
 (add-to-list 'plstore-encrypt-to '("488B0DAA0B420C3CEDC30F8308ADA8CDF127AC03"))
  (setq org-gcal-fetch-file-alist
        '(("hermannschris@googlemail.com" . "~/Dropbox/cal/dates.org")))
  )

(provide 'init-org)
;;; init-org.el ends here

;;; init-org-agenda.el  --- Working with agenda -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package org-super-agenda
              :straight t
              ;;:after (org)
              :config
              (with-eval-after-load 'org-agenda
                (define-key org-agenda-mode-map (kbd "q") #'org-agenda-quit))
              (setq org-agenda-custom-commands
                    '(("o" "Overwatch"
                       ((agenda ""
                                ((org-agenda-span 7)
                                 (org-agenda-remove-tags t)
                                 ;;(org-agenda-skip-scheduled-if-deadline-is-shown t)
                                 (org-agenda-skip-timestamp-if-deadline-is-shown t)
                                 (org-agenda-skip-deadline-if-done t)))
                        (tags-todo "+private"
                                 (
                                  (org-agenda-prefix-format "%t %s")
                                  (org-agenda-overriding-header "PRIVATE CURRENT")
                                  (org-super-agenda-groups
                                   '(
                                     (:name "Personal"
                                            :tag "CRITICAL"
                                            :order 0
                                      )
                                     (:name "Currently Working"
                                            :todo "IN-PROGRESS"
                                            :order 1
                                            )
                                     (:name "Scheduled"
                                            :todo "PLANNING"
                                            :tag "obstacle"
                                            :order 2
                                            )
                                     (:name "On the way"
                                            :todo "IDEA"
                                            :tag "evolving"
                                            :order 3
                                            )
                                     (:name "General Backlog"
                                            :and (:todo "TODO" :priority "B")
                                            :order 4
                                      )
                                     )
                                   )
                                  )
                                 )
                        (tags-todo "+work"
                                 (
                                  ;; Remove tags to make the view cleaner
                                  (org-agenda-remove-tags nil)
                                  (org-agenda-prefix-format " %t %s")
                                  (org-agenda-overriding-header "WORK CURRENT")

                                  ;; Define the super agenda groups (sorts by oder)
                                  (org-super-agenda-groups
                                   '(
                                     ;; Filter where tag is CRITICAL
                                     (:name "Critical Tasks"
                                            :tag "CRITICAL"
                                            :order 0
                                            )
                                     ;; Filter where TODO is IN-PROGRESS
                                     (:name "Currently Working"
                                            :todo "IN-PROGRESS"
                                            :order 1
                                            )
                                     ;; Filter where TODO state is BLOCKED  - or obstacle!
                                     (:name "Problems & Blockers"
                                            :todo "BLOCKED"
                                            :tag "obstacle"
                                            :order 2
                                            )
                                     ;; Filter where tag is @write_future_ticker
                                     (:name "Scheduled and Planning"
                                            :tag "@scheduled"
                                            :order 4
                                            )
                                     ;; Filter where tag is @research
                                     (:name "Research Required"
                                            :tag "@research"
                                            :order 5
                                            )
                                     ;; Filter where tag is meeting and priority A  (only want TODOs from meetings)
                                     (:name "Meeting Action Items"
                                            :and (:tag "meeting" :priority "A")
                                            :order 6
                                            )
                                     ;; Filter where state is TODO and the priority is A and the tag is not meeting
                                     (:name "Other Important Items"
                                            :and (:todo "TODO" :priority "A" :not (:tag "meeting" )
                                            ))
                                     ;; Filter where the priority is C or less (supports future lower priorities)
                                     (:name "Non Critical"
                                            :priority <= "C"
                                            :order 7
                                            )
                                     ;; Filter where TODO state is VERIFYING
                                     (:name "Currently Being Verified"
                                            :todo "VERIFYING"
                                            :order 8
                                            )
                                     )
                                   )
                                  )
                                  )

                        (tags-todo "+activity"
                                 (
                                  (org-agenda-remove-tags nil)
                                  (org-agenda-prefix-format "%t %s")
                                  (org-agenda-overriding-header "ACTIVITY CURRENT")
                                  (org-super-agenda-groups
                                   '(
                                     (:name "On the Top"
                                            :tag "CRITICAL"
                                            :order 0
                                      )
                                     (:name "Currently Processed"
                                            :todo "IN-PROGRESS"
                                            :order 1
                                            )
                                     (:name "On Schedule"
                                            :todo "PLANNING"
                                            :tag "obstacle"
                                            :order 2
                                            )
                                     (:name "Meeting Action Items"
                                            :and (:tag "meeting" :priority "A")
                                            :order 6
                                            )
                                     (:name "On the way"
                                            :todo "IDEA"
                                            :tag "evolving"
                                            :order 3
                                            )
                                     (:name "General Backlog"
                                            :and (:todo "TODO" :priority ("B" "A"))
                                            :order 4
                                      )
                                     )
                                   )
                                  )
                                 )
                        ))
                      ))
  :general
  (hc/leader
    :states 'normal
    :infix "o"
    ""  '(nil :which-key "org-mode")
    "a" '("agenda" . org-agenda)
    "l" '("org-store a link" . org-store-link)
    "c" '("capture some tasks..." . org-capture))

)

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here

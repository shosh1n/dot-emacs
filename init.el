;;; init.el --- Summary ;;my-emacs -*- lexical-binding: t -*-
;;; Commentary:
;;;; Code:
(setq user-emacs-directory "~/.emacs.d/")
(setq user-full-name "Christoph-Alexander Hermanns"
      user-real-login-name "Christoph-Alexander Hermanns"
      user-login-name "shoshin"
      user-mail-address "christoph-alexander.hermanns@proton.me"
 )
(setq warning-minimum-level :emergency)
(setq gc-cons-threshold 100000000)
(setq package-enable-at-startup nil)
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq site-run-file nil)
(menu-bar-mode -1)
(defvar better-gc-cons-threshold 134217728)
  (add-hook 'emacs-startup-hook
        (lambda ()
          (setq gc-cons-threshold better-gc-cons-threshold)
          (setq file-name-handler-alist file-name-handler-alist-original)
          (makunbound 'file-name-handler-alist-original)))

  (add-hook 'emacs-startup-hook
        (lambda ()
          (if (boundp 'after-focus-change-function)
          (add-function :after after-focus-change-function
                (lambda ()
                  (unless (frame-focus-state)
                    (garbage-collect))))
        (add-hook 'after-focus-change-function 'garbage-collect))
          (defun gc-minibuffer-setup-hook ()
        (setq gc-cons-threshold (* better-gc-cons-threshold 2)))
          (defun gc-minibuffer-exit-hook ()
        (garbage-collect)
        (setq gc-cons-threshold better-gc-cons-threshold))
          (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
          (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

(defun my/minibuffer-quit-clean ()
  "Trim minibuffer input and quit minibuffer."
  (interactive)
  (when (minibufferp)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (insert (string-trim (minibuffer-contents)))))
  (abort-recursive-edit))

(define-key minibuffer-local-map (kbd "C-g") #'my/minibuffer-quit-clean)
(add-hook 'before-save-hook #'whitespace-cleanup)
(global-subword-mode 1)
(setq scroll-conservatively 1000)
(setq-default indent-tabs-mode nil)
(defun add-hooks-to-modes (modes hook-fn)
  (dolist (mode modes)
    (add-hook (intern (format "%s-hook" mode)) hook-fn)))
(add-hooks-to-modes '(prog-mode latex-mode) #'display-line-numbers-mode)
(add-hooks-to-modes '(prog-mode latex-mode) #'hs-minor-mode)


(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
    (add-to-list 'load-path base))
    (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
             (not (equal f ".."))
             (not (equal f ".")))
        (unless (member base load-path)
          (add-to-list 'load-path name)))))))

  (update-to-load-path "~/dot-emacs/lisp")
  (update-to-load-path (expand-file-name "elisp" user-emacs-directory))

  (defconst *sys/linux*
    (eq system-type 'gnu/linux)
    "Are we running on a GNU/LINUX system?")
(global-auto-revert-mode t)
(setq epg-gpg-program
      (executable-find "gpg"))

  (defconst python-p
    (or (executable-find "python3")
    (and (executable-find "python")
         (> (length (shell-command-to-string "python --version | grep 'Python 3'")(message "FOUND PYtON")) 0(message "NO PYtON ._.") )))
    "Do we have python3?")

  (defconst pip-p
    (or (executable-find "pip3")
    (and (executable-find "pip")
         (> (length (shell-command-to-string "pip --version | grep 'python 3'")) 0)))
    "Do we have pip3?")

  (defconst clangd-p
    (or (executable-find "clangd") ;; usually
    (executable-find "/usr/local/opt/llvm/bin/clangd"))
    "Doe we have clangd?")

  (defconst eaf-env-p
    (and (display-graphic-p) python-p pip-p)
    "Do we have EAF environment setup?")

  (defvar bootstrap-version)
  (let ((bootstrap-file
     (expand-file-name
      "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
    (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (setq straight-use-package-by-default t)
  (setq package-check-signature nil)

  (straight-use-package 'use-package)
  (eval-and-compile
    (setq use-package-verbose t
      use-package-expand-minimally t
      use-package-compute-statistics t
      use-package-enable-imenu-support t))

  (eval-when-compile
   (require 'use-package)
   (require 'bind-key))

(setq custom-safe-themes t)

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-time-analogue-clock t)
  )
(use-package nerd-icons
  :straight t
)

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (setq evil-insert-state-cursor '(bar "Green"))
  (setq evil-visual-state-cursor '(box "White"))
  (setq evil-normal-state-cursor '(box "Pink")
  ))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-default-face ((t (:inherit 'unspecified))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(which-key-description-face ((t (:foreground "red"))))
 '(which-key-key-face ((t (:foreground "#8be9fd" :weight bold))))
 '(which-key-separator-face ((t (:foreground "#50fa7b")))))




(use-package exec-path-from-shell
  :straight t
    (exec-path-path-from-shell
      :type git
      :host github
      :repo "purcell/exec-path-from-shell"
    )
)

(require 'exec-path-from-shell)
(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG"))
  (add-to-list 'exec-path-from-shell-variables var))

(use-package which-key
    :straight t
    :diminish
    :init
    :custom
    (which-key-add-column-padding 2)
    (which-key-allow-multiple-replacements t)
    (which-key-idle-delay 0.01)
    (which-key-idle-secondary-delay 0.05)
    (which-key-min-display-lines 6)
    (which-key-mode t)
    (which-key-side-window-slot -10)
    (which-key-show-early-on-C-h nil)
    (which-key-show-prefix 'echo)
    (which-key-popup-type 'minibuffer)
    :config
    (which-key-mode)
    )


(require 'init-tramp)
(require 'init-vterm)

(defun reload-emacs-config ()
  (interactive) (load-file user-init-file))

(defun goto-user-config ()
  (interactive)(find-file user-init-file))



(straight-use-package '(evil :host github :repo "emacs-evil/evil"))
(use-package evil
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
(require 'init-dired)
(require 'init-general)

(require 'init-vertico)



(use-package key-chord
  :straight t
  :config
  (key-chord-mode 1)
)

(use-package mpv
  :straight t
  :config
  (setq mpv-default-options '("--ytdl-format=best")))



(use-package undo-fu
  :straight t)
(use-package undo-fu-session
  :straight t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
(undo-fu-session-global-mode)
(use-package vundo
  :straight t)

;; appearances
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(setq default-frame-alist '((undecorated . t)
                (drag-internal-border . t)
                (internal-border-width . 10)
                (height . 55)
                (width . 174)
                (left . 613)
                (top . 391)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-vars . nil)
                (tool-bar-lines .0)))

(defvar my/default-font-size 120)
(defvar my/default-variable-font-size 160)
(set-face-attribute 'default nil :font "JetBrains Mono" :height my/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height my/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height my/default-variable-font-size :weight 'regular)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)



(require 'time)
(defun show-time ()
  "Enable display-time-mode. For learning purpose."
  (setq display-time-format "%Y-%m-%d %H:%M")
  (display-time-mode 1))
(show-time)
(setq visible-bell t)
(setq x-stretch-cursor t)
(add-to-list 'default-frame-alist '(alpha-background . 0.98))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
(setq dashboard-startup-banner "~/.config/doom/samus.png")

(require 'init-dabbrev)
(require 'init-lsp)
(require 'init-consult)
(require 'init-corfu)
(require 'init-cape)
(require 'init-ltex)

(use-package cmake-mode
  :defer t
  :straight (:build t))
(use-package cmake-font-lock
  :defer t
  :after cmake-mode
  :straight (:build t))



(use-package modern-cpp-font-lock
  :straight (:build t)
  :defer t
  :hook (c++-mode . modern-c++-font-lock-mode))


;; show functionalities



(use-package vertico-prescient
  :straight t
)


(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;;(use-package orderless
;;  :straight t
;;  :custom
;;  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
;;  (completion-styles '(orderless basic))
;;  (completion-category-defaults nil)
;;  (completion-category-overrides '((file (styles partial-completion)))))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((lsp-capf (styles orderless))
     (file (styles partial-completion)))))

;;; reduce shown functionalities
(use-package diminish
  :straight t)

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
          ("C-c l" . marginalia-cycle)) ;; this need to be somwhere else
  :init
  (marginalia-mode))

(add-hook 'prog-mode-hook  #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))


(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)

  (custom-set-faces
   '(evil-goggles-default-face ((t (:inherit 'unspecified))))
   '(evil-goggles-delete-face ((t (:inherit 'shadow))))
   '(evil-goggles-paste-face ((t (:inherit 'lazy-highlight))))
   '(evil-goggles-yank-face ((t (:inherit 'isearch-fail))))))

(use-package engrave-faces
  :straight t
  )

(use-package oauth2
  :straight t
  )
(use-package org-modern
  :straight t
  :after org
  :config
  (global-org-modern-mode -1)
  )

(use-package org-modern-indent
  :straight (:package org-modern-indent
                      :type built-in
                      :local-repo "org-modern-indent"
                      :files ("org-modern-indent.el")
                      )
  :load-path "~/.emacs.d/elisp/org-modern-indent"
  :after (org org-modern)
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
  )

(use-package valign
  :straight (:package valign
                      :type built-in
                      :local-repo "valign"
                      :files ("valign.el")
                      )
  :load-path "~/.emacs.d/elisp/valign"
  :after (org)
  :config
  (add-hook 'org-mode-hook #'valign-mode)
)

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))


(use-package org
  :straight t
  :after (engrave-faces cdlatex general)
  :hook ((org-mode . visual-line-mode)
         )
  :bind
  (:map orgtbl-mode-map
        ("<tab>" . lazytab-org-table-next-field-maybe)
        ("TAB" . lazytab-org-table-next-field-maybe))
  :init
  (auto-fill-mode -1)
  (org-num-mode -1)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
    ;; Tabular environments using cdlatex
  (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
                                       "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
                                       "\\begin{bmatrix} ? \\end{bmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
                                       "\\begin{pmatrix} ? \\end{pmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
                                        "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                                       lazytab-position-cursor-and-edit
                                       nil t nil))
  :config
  (setq org-latex-compiler "xelatex")
  (setq org-latex-packages-alist '(("" "amsmath" t)))
  (load (expand-file-name (concat user-emacs-directory "elisp/my-latex/note-latex")))
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-skip-deadline-if-done t)
  (setq org-num-mode nil)
  (setq org-directory "~/org")
  ;;(defvar my-latex-font-size "12pt" "Default font size for LaTeX exports.")
  (setq org-latex-compiler "xelatex")
  (setq org-agenda-files (list "~/Dropbox/orgzly"))
  (add-to-list  'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
  (setq org-capture-templates
        '(
          ("g" "General To-Do"
           entry (file+headline "~/org/agenda/todos.org" "General Tasks")
           "* TODO [#%^{Priority|A|B|C}] %^{Title} :%^{Tag|private|work|activity}:%^g\n:Created: %U\nSCHEDULED:%^{Scheduled to begin}t \n%?"
           :empty-lines 0)
        ("c" "Code To-Do"
          entry (file+headline "~/org/todos.org" "Code Related Tasks")
          "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: ")
          ("m" "Meeting"
          entry (file+datetree "~/org/agenda/meetings.org")
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
        '(
          ("TODO" . (:foreground "GoldenRod" :weight bold))
          ("IN-PROGRESS"  (:foreground "DeepPink" :weight bold))
          ("BLOCKED" . (:foreground "Cyan" :weight bold))
          ("DONE" . (:foreground "LimeGreen" :weight bold))
          ("OBE" . (:foreground "DarkOrange" :weight bold))
          ("WONT-DO" . (:foreground "Cyan" :weight bold))
          )
        )
  (setq org-tag-alist '(
                        ;; my selections
                        (:startgroup . nil)
                        ("private" 󰄛 . ?p)
                        ("work"  . ?w)
                        ("activity"  . ?a)
                        (:endgroup . nil)

                        ;; ticket types
                        (:startgroup . nil)
                        ("@bug"  . ?b)
                        ("@feature" 󰡱 . ?u)
                        ("@spike" 󰐰. ?k)
                        (:endgroup . nil)

                        ;; Ticket flags
                        ("@scheduled" . ?s)
                        ("@emergency" 󱪄  . ?e)
                        ("@research"  . ?r)

                        ;; Meeting types
                        (:startgroup . nil)
                        ( 󰴺 "general"  . ?g)
                        ("big_sprint_review" . ?r)
                        ("normal_sprint_review" 󰔚 . ?n)
                        ("ai" 󱜚 . ?a)
                        (:endgroup . nil)

                        ;; Code TODO tags
                        ("QA" 󰺴. ?q)
                        ("foundation" . ?f)
                        ("broken_code" . ?c)
                        ("performance" . ?f)

                        ;; Special tags
                        ("CRITICAL" . ?x)
                        ("obstacle" . ?o)

                        ;; Meetings tags
                        ("HR" . ?h)
                        ("general" . ?l)
                        ("meeting" . ?m)
                        ("misc" . ?z)

                        ;; Work Log Tags
                        ("accomplishment" . ?y)
                        ))
  (setq org-agenda-category-icon-alist
      `(;;("work"       ,(list (all-the-icons-material "work" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
        ;;("activity"   ,(list (all-the-icons-faicon "bicycle" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
        ;;("meeting"    ,(list (all-the-icons-material "event" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
        ;;("@research"  ,(list (all-the-icons-material "science" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
        ("general"    ,(list (nerd-icons-mdicon "nf-md-tank" :height 0.9 :v-adjust -0.1)) nil nil :ascent center)
        ))

  )

(use-package org-super-agenda
              :straight t
              :after (org)
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

(use-package org-gcal
  :straight t
  :after org
  :init
  (setq org-gcal-fetch-file-alist '(("hermannschris@googlemail.com" . "~/org/dates.org")))
  :config
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  )

(use-package org-contrib
  :after (org)
  :straight t
  :init
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package avy
  :straight t
  :config
  )

(use-package ispell
  :if (executable-find "aspell")
  :straight (:type built-in)
  :config
  )

(use-package org-ref
  :straight t
  (org-ref
   :type git
   :host github
   :repo "jkitchin/org-ref"
   :files ("*.el"))
  :after org
  :bind (bibtex-mode-map (kbd "c-c-b") 'org-ref-bibtex-hydra/body)
  :config
  (setq bibtex-autokey-year-length 4
    bibtex-autokey-name-year-separator "-"
    bibtex-autokey-year-title-separator "-"
    bibtex-autokey-titleword-separator "-"
    bibtex-autokey-titlewords 2
    bibtex-autokey-titlewords-stretch 1
    bibtex-autokey-titleword-length 5)
  )


(use-package smartparens
  :straight t
  (smart-parens
   :type git
   :host github
   :repo "Fuco1/smartparens"
   :files (".el")))

(use-package flycheck
  :straight t (:build t)
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 2.0)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 1))

(setq display-buffer-alist
  '(
    ;;(BUFFER-MATCHER
    ;; LIST-OF-DISPLAY-FUNCTIONS <- the one that matches will be used and terminate the sequence
    ;; &optional PARAMETERS)        be mindful of order...
    ("\\*Occur\\*"
      ;; 'list-of-functions'
      (display-buffer-reuse-window ;; here I might want to 're-use' the window
       display-buffer-below-selected) ;; if '*occur*' is not there place it below the SEL window!
      ;; Next-Up are 'parameters' to the called functions...
      (window-height . fit-window-to-buffer) ;; or no. of lines to stretch...
      ;;(dedicated . t) ;; dedicate buffer to its window
      (body-function . (lambda(window) (select-window window)))
    )
  )
)

;; programming
;; indentation
(setq indent-tabs-mode nil
      tab-width 4)
(add-hook 'c-mode-hook
      (lambda () (setq-local indent-tabs-mode t)))
;;(add-hook 'lisp-mode-hook
;;	  (lambda () (setq-local indent-tabs-mode t)))



;;(use-package tree-sitter
;;  :config
;;  (require 'tree-sitter-langs)
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;  (define-key python-mode-map (kbd "SPC") nil))
;; clangd

;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
;; fields
;;(use-package cdlatex
;;  :hook ((cdlatex-tab . yas-expand)
;;         (cdlatex-tab . cdlatex-in-yas-field))
;;  :config
;;  (use-package yasnippet
;;    :bind (:map yas-keymap
;;           ("<tab>" . yas-next-field-or-cdlatex)
;;           ("TAB" . yas-next-field-or-cdlatex))
;;    :config
;;    (defun cdlatex-in-yas-field ()
;;      ;; Check if we're at the end of the Yas field
;;      (when-let* ((_ (overlayp yas--active-field-overlay))
;;                  (end (overlay-end yas--active-field-overlay)))
;;        (if (>= (point) end)
;;            ;; Call yas-next-field if cdlatex can't expand here
;;            (let ((s (thing-at-point 'sexp)))
;;              (unless (and s (assoc (substring-no-properties s)
;;                                    cdlatex-command-alist-comb))
;;                (yas-next-field-or-maybe-expand)
;;                t))
;;          ;; otherwise expand and jump to the correct location
;;          (let (cdlatex-tab-hook minp)
;;            (setq minp
;;                  (min (save-excursion (cdlatex-tab)
;;                                       (point))
;;                       (overlay-end yas--active-field-overlay)))
;;            (goto-char minp) t))))
;;
;;    (defun yas-next-field-or-cdlatex nil
;;      (interactive)
;;      "Jump to the next Yas field correctly with cdlatex active."
;;      (if
;;          (or (bound-and-true-p cdlatex-mode)
;;              (bound-and-true-p org-cdlatex-mode))
;;          (cdlatex-tab)
;;        (yas-next-field-or-maybe-expand)))))

;; Array/tabular input with org-tables and cdlatex

(require 'init-auctex)
(require 'init-denote)
(require 'init-graphviz)
(require 'init-yasnippet)
(require 'init-casual-suite)
(require 'init-org-roam)
;;(require 'init-crontab)
;;(require 'init-sly)
;;(custom-set-variable
;; (setq TeX-engine "xetex")
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init fie should contain only one such instance.
;; If there is more than one, they won't work right.
;;(setq lsp-completion-enable-additional-text-edit nil)
;;)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode)))
 '(tramp-default-host "172.104.241.26")
 '(tramp-default-method "rsync")
 '(tramp-default-user "sanakan")
 '(yas-snippet-dirs
   '("/home/shoshin/.emacs.d/straight/build/yasnippet-snippets/snippets")))

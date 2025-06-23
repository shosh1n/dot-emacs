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
 '(which-key-key-face ((t (:foreground "#8be9fd" :weight bold))))
 '(which-key-separator-face ((t (:foreground "#50fa7b"))))
 '(which-key-description-face ((t (:foreground "red")))))



(use-package general
  :straight t
  (general
   :type git
   :host github
   :repo "noctuid/general.el"
   )
  :config
  (general-create-definer hc/leader
    ;; :prefix hc-key
    ;; it's my leader key which should be available everywhere!
    :prefix "SPC"
    :states '(normal motion)
    :keymaps 'override
    )
  (general-create-definer hc/leader-major
    :prefix "S-SPC"
    :states '(normal insert visual emacs)
    :keymaps 'override
    )
  (hc/leader
   :infix "f"
   ""  '(nil :which-key "file operations")
   "f" '("find a file" . find-file)
   "r" '("open recent files" . recentf-open-files)
   "d" '("📁 dired" . dired-jump)
   "p" '("my config.el" . goto-user-config)
   )

   (hc/leader
   :infix "SPC"
   "" '(nil :which-key "emacs stuff")
   "r" '("reload emacs-configuration" . reload-emacs-config)
   )
   (hc/leader
   :infix "s"
   "" '(nil :which-key "buffer operations")
   "s" '("goto scratch buffer" . scratch-buffer)
   "a" '("erease current buffer" . erase-buffer)
   "e" '("goto scratch buffer" . eval-buffer)
   )
   (hc/leader
   :infix "h"
   "" '(nil :which-key "helpful stuff")
   "v" '("describe variable!" . describe-variable)
   "f" '("describe function!" . describe-function)
   "k" '("describe key!" . describe-key)
   "m" '("describe mode!" . describe-mode)
   "c" '("describe command!" . describe-command)
   )

   (general-define-key
    :states 'normal
    :keymaps 'dired-mode-map
    "<return>" 'dired-find-file
    "^" 'dired-up-directory
    )
   (general-define-key
    :states '(normal insert visual emacs motion)
    :keymaps '(override minibuffer-local-map)
    "C-S-v" 'consult-yank-from-kill-ring
    )
   ;;(general-define-key
   ;; :states '(normal insert)
   ;; :keymaps '(override minibuffer-local-map insert-mode-map)
   ;; "M-p" 'completion-at-point
   ;; )
   )

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

(defun reload-emacs-config ()
  (interactive) (load-file user-init-file))

(defun goto-user-config ()
  (interactive)(find-file user-init-file))

(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))


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


(use-package key-chord
  :straight t
  :config
  (key-chord-mode 1)
  ;(key-chord-define evil-insert-state-map "ok" 'keyboard-quit)
  ;(key-chord-define evil-normal-state-map "ok" 'keyboard-quit)
  ;(key-chord-define evil-normal-state-map "ok" 'abort-recursive-edit)
  ;(key-chord-define evil-insert-state-map "ok" 'abort-recursive-edit)
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

;   se  ac age timu-rouge-theme
;;  :straight t
;;  :config
;;  (load-theme 'timu-rouge t)
;;)
;;(use-package tokyo-theme
;;  :straight
;;  (:host github
;;   :repo "rawleyfowler/tokyo-theme.el"
;;   :files ("*.el")
;;   )
;;  :init
;;  (let ((theme-dir (straight--build-dir "tokyo-theme")))
;;    (unless (member theme-dir custom-theme-load-path)
;;                    (add-to-list 'custom-theme-load-path theme-dir)))
;;  :config
;;  (load-theme 'tokyo t))

(require 'time)
(defun show-time ()
  "Enable display-time-mode. For learning purpose."
  (setq display-time-format "%Y-%m-%d %H:%M")
  (display-time-mode 1))
(show-time)
(setq visible-bell t)
(setq x-stretch-cursor t)
(add-to-list 'default-frame-alist '(alpha-background . 0.98))

;;(use-package projectile
;;  :straight t
;;  :init
;;  (projectile-mode +1)
;;  :config
;;  :general
;;  (hc/leader
;;   :states '(normal motion)
;;   :keymaps 'projectile-mode-map
;;   "p" '(:keymap projectile-command-map :which-key "projectile"))
;;  )



(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
(setq dashboard-startup-banner "~/.config/doom/samus.png")
;;(use-package evil-collection
;;  :after (evil)
;;  :straight (evil-collection :type git
;;               :host github
;;               :repo "emacs-evil/evil-collection"))

(use-package prescient
  :straight t
  )



(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-cycle nil)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-auto-prefix 2)
  (corfu-min-width 80)
  (corfu-max-width 80)
  (corfu-scroll-margin 4)
  ;;(corfu-preview-current nil)
  (corfu-preselect 'prompt)
  ;;(corfu-on-exact match nil)
  :hook
  ((prog-mode . corfu-mode)
   (shell-mode . corfu-mode)
   (eshell-mode . corfu-mode)
   (python-mode . corfu-mode))
  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-p" #'corfu-previous
            "C-n" #'corfu-next
            "<return>" #'corfu-insert
            "M-d" #'corfu-show-documentation
            "M-l" #'corfu-show-location)
  )

(use-package cape
  :straight t
  :general
  (hc/leader
    :infix "a"
    "" '(nil :which-key "completions")
    "f" '("file completions" . cape-file)))


;; show functionalities
(use-package vertico
  :straight t
  :custom
  (vertico-count 7)
  :init
  (vertico-mode))
(use-package consult
  :straight t

  :custom
  (consult-preview-key nil)
  :general
  (:states 'normal
           (hc/leader
             :infix "b"
             ""  '(nil :which-key "buffer management")
             "b" '("Show Consult Buffers" . consult-buffer)
             "p" '("goto previous buffer". previous-buffer)
             "n" '("goto next buffer"  . next-buffer))
           )
  )

(use-package vertico-prescient
  :straight t
)


(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :straight t
  :custom
  ;;(orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

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
  (global-org-modern-mode 1)
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

(use-package org
  :straight t
  :after engrave-faces
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-num-mode))
  :init
  (auto-fill-mode -1)


  :config
  (setq org-directory "~/org")
  ;;(defvar my-latex-font-size "12pt" "Default font size for LaTeX exports.")
  (setq org-latex-compiler "xelatex")
  (setq org-agenda-files (list "~/org/agenda"))
  (add-to-list  'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  (setq org-capture-templates
        '(
          ("g" "General To-Do"
           entry (file+headline "~/org/agenda/todos.org" "General Tasks")
           "* TODO [#%^{Priority|A|B|C}] %^{Title} :%^{Tag|private|work|activity}:%^g\n:Created: %U\nSCHEDULED:%^{Scheduled to begin}t DEADLINE:%^{DEADLINE}T\n%?"
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
                        ("private" . ?p)
                        ("work" . ?w)
                        ("activity" . ?a)
                        (:endgroup . nil)

                        ;; ticket types
                        (:startgroup . nil)
                        ("@bug" . ?b)
                        ("@feature" . ?u)
                        ("@spike" . ?k)
                        (:endgroup . nil)

                        ;; Ticket flags
                        ("@scheduled" . ?s)
                        ("@emergency" . ?e)
                        ("@research" . ?r)

                        ;; Meeting types
                        (:startgroup . nil)
                        ("general" . ?g)
                        ("big_sprint_review" . ?r)
                        ("normal_sprint_review" . ?n)
                        ("ai" . ?a)
                        (:endgroup . nil)

                        ;; Code TODO tags
                        ("QA" . ?q)
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

   :general
  (:states 'normal
           (hc/leader
             :infix "o"
             ""  '(nil :which-key "org-mode")
             "a" '("agenda" . org-agenda)
             "l" '("org-store a link" . org-store-link)
             "c" '("capture some tasks..." . org-capture)
             ))
  )

(use-package org-super-agenda
              :straight t
              :config
              (setq org-agenda-custom-commands
                    '(
                      ("o" "Overwatch"
                       (
                        (agenda ""
                                (
                                 (org-agenda-remove-tags t)
                                 (org-agenda-span 7)
                                 )
                                )
                        (tags-todo "+private"
                                 (
                                  (org-agenda-remove-tags nil)
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




(load (expand-file-name (concat user-emacs-directory "elisp/dired+/dired+")))
;;(with-eval-after-load 'dired
;;  (require 'dired+))

(use-package avy
  :straight t
  :config
  )

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-avy-menu
  :straight t
  :after flyspell-correct)


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
  :config
  (setq bibtex-autokey-year-length 4
    bibtex-autokey-name-year-separator "-"
    bibtex-autokey-year-title-separator "-"
    bibtex-autokey-titleword-separator "-"
    bibtex-autokey-titlewords 2
    bibtex-autokey-titlewords-stretch 1
    bibtex-autokey-titleword-length 5)
  :bind (bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
  )

(use-package org-roam
  :straight
  ;;(:host github
  ;; :repo "org-roam/org-roam"
  ;; :files (:defaults "extensions/*"))
  :custom
  (org-roam-directory (file-truename "~/.org/"))
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





;;(with-eval-after-load 'lsp-mode
;;  (define-key lsp-mode-map (kbd "SPC") nil)) ;; Stop SPC from triggering LSP menu
;;
;;(with-eval-after-load 'python
;;  (define-key python-mode-map (kbd "SPC") nil))



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
(use-package python
  :defer t
  :straight (:build t)
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  :config
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  :general (hc/leader-major
             :keymaps 'python-mode-map
             :packages 'lsp-mode
             "l" '(:keymap lsp-command-map :which-key "lsp"))
  )


(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap
  :hook ((python-mode . lsp)
         (org-mode . lsp)
         (text-mode . lsp)
         (markdown-mode . lsp)
         (LaTeX-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
  )
)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
  )


(use-package lsp-ltex
  :straight t
  :after lsp-mode
  :init
  (setq lsp-ltex-version "16.0.0")
  :hook ((org-mode . lsp)
         (text-mode  . lsp)
         (markdown-mode . lsp)
         (LaTeX-mode . lsp))
  :config
  (setq lsp-ltex-language "de-DE")
  (setq lsp-ltex-mother-tongue "de-DE")
  (setq lsp-ltex-file-extension-whitelist '("org" "txt" "md" "tex"))
  (setq lsp-ltex-disabled-rules '(["MORFOLOGIK_RULE_DE"]))
  (setq lsp-log-io t)
  )

(defun my/lsp-ltex-set-documentKind ()
  (cond
   ((derived-mode-p 'org-mode)
    (setq-local lsp-ltex-documentKinds ["plaintext"]))
   ((derived-mode-p 'markdown-mode)
    (setq-local lsp-ltex-documentKinds ["markdown"]))
   ((derived-mode-p 'LaTeX-mode)
    (setq-local lsp-ltex-documentKinds ["latex"]))
   (t
    (setq-local lsp-ltex-documentKinds ["plaintext"]))))

(add-hook 'lsp-mode-hook #'my/lsp-ltex-set-documentKind)


(use-package rust-mode
  :straight t
  :init
  ;;(setq rust-mode-treesitter-derive t)
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (let ((dot-cargo-bin (expand-file-name "~/.cargo/bin/")))
    (setq rust-rustfmt-bin (concat dot-cargo-bin "rustfmt")
          rust-cargo-bin (concat dot-cargo-bin "cargo")
          rust-format-on-save t)))


(use-package eglot
  :straight t
  (eglot
   :type built-in)
  :config
  (add-to-list 'eglot-server-programs
   '(rust-mode . ("rust-analyzer" :initializationOptions
                 (:procMacro (:enable t)
                  :cargo (:buildScripts (:enable t)
                                        :features "all")))))
   (add-to-list 'eglot-server-programs
     '(python-mode . ("/home/shoshin/miniconda3/bin/basedpyright-langserver" "--stdio"))))

;;; init-consult.el  --- Working with consult -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package consult
  :straight t
  :custom
  (consult-preview-key 'any)
  (defvar consult--source-bookmark
    `(:name     "Bookmark"
      :narrow   ?m
      :category bookmark
      :face     consult-bookmark
      :history  bookmark-history
      :items    ,#'bookmark-all-names
      :action   ,#'consult--bookmark-action))
      :general
      (:states 'normal
           (hc/leader
             :infix "c"
             ""  '(nil :which-key "consult ...")
             "y" '("store selection in register" . consult-register-store)
             "p" '("load slection from register" . consult-register)
             "c" '("lookup string"  . consult-ripgrep)
             "l" '("lookup line"  . consult-line)
             "f" '("focus lines" . consult-focus-lines)
             "f" '("fd for files ... " . consult-fd)
             "m" '("go to marked position" . consult-global-mark)
             )
       )
  (general-define-key
   :keymaps 'evil-normal-state-map
   [remap evil-paste-before] 'consult-yank-from-kill-ring
   )
  :config
  (require 'keymap)
  (require 'cl-seq)
  (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
  (cl-nsubstitute-if
    '(consult-ripgrep "Find regexp")
    (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-regexp))
    project-switch-commands)
  (setq project-read-file-name-function #'consult-project-find-file-with-preview)
  (defun consult-project-find-file-with-preview (prompt all-files &optional pred hist _mb)
    (let ((prompt (if (and all-files
                           (file-name-absolute-p (car all-files)))
                      prompt
                    ( concat prompt
                      ( format " in %s"
                        (consult--fast-abbreviate-file-name default-directory)))))
          (minibuffer-completing-file-name t))
      (consult--read (mapcar
                      (lambda (file)
                        (file-relative-name file))
                      all-files)
                     :state (consult--file-preview)
                     :prompt (concat prompt ": ")
                     :require-match t
                     :history hist
                     :category 'file
                     :predicate pred)))
;;  (defvar +consult-source-neighbor-file
;;    `(:name     "File in current directory"
;;      :narrow   ?.
;;      :category file
;;      :face     consult-file
;;      :history  file-name-history
;;      :state    ,#'consult--file-state
;;      :new      ,#'consult--file-action
;;      :items
;;      ,(lambda ()
;;         (let ((ht (consult--buffer-file-hash)) items)
;;           (dolist (file (completion-pcm--filename-try-filter
;;                          (directory-files "." 'full "\\`[^.]" nil 100))
;;                         (nreverse items))
;;             (unless (or (gethash file ht) (not (file-regular-p file)))
;;               (push (file-name-nondirectory file) items))))))
;;    "Neighboring file source for `consult-buffer'.")
;;
;;  (unless (memq '+consult-source-neighbor-file consult-buffer-sources)
;;  (let ((p (member 'consult--source-buffer consult-buffer-sources)))
;;    (setcdr p (cons '+consult-source-neighbor-file (cdr p)))))
;;  (defun consult-project-find-file-with-preview (prompt all-files &optional pred hist _mb)
;;    (let ((prompt (if (and all-files
;;                           (file-name-absolute-p (car all-files)))
;;                      prompt
;;                    ( concat prompt
;;                      ( format " in %s"
;;                        (consult--fast-abbreviate-file-name default-directory)))))
;;          (minibuffer-completing-file-name t))
;;      (consult--read (mapcar
;;                      (lambda (file)
;;                        (file-relative-name file))
;;                      all-files)
;;                     :state (consult--file-preview)
;;                     :prompt (concat prompt ": ")
;;                     :require-match t
;;                     :history hist
;;                     :category 'file
;;                     :predicate pred)))
;;  (defun +orderless-fix-dollar (word &optional _index _total)
;;    (concat word (if (boundp 'consult--tofu-regexp)
;;                     (concat consult--tofu-regexp "*$")
;;                   "$")))
;;  (add-to-list 'orderless-affix-dispatch-alist '(?$ . +orderless-fix-dollar))
;;  (require 'dom)
;;  (require 'url-util)
;;  (require 'xml)
;;  (defun consult--xdg-recent-file-list ()
;;    "Get a list of recently used files on XDG-compliant systems.
;;
;;  This function extracts a list of files from the file
;;  `recently-used.xbel' in the folder `xdg-data-home'.
;;
;;    For more information on this specification, see
;;    https://www.freedesktop.org/wiki/Specifications/desktop-bookmark-spec/"
;;      (let ((data-file (expand-file-name "recently-used.xbel" (xdg-data-home)))
;;            (xml-parsing-func (if (libxml-available-p)
;;                                  #'libxml-parse-xml-region
;;                                #'xml-parse-region)))
;;        (if (file-readable-p data-file)
;;            (delq nil
;;                  (mapcar (lambda (bookmark-node)
;;                            (when-let ((local-path (string-remove-prefix
;;                                                    "file://"
;;                                                    (dom-attr bookmark-node 'href))))
;;                              (let ((full-file-name (decode-coding-string
;;                                                     (url-unhex-string local-path)
;;                                                     'utf-8)))
;;                                (when (file-exists-p full-file-name)
;;                                  full-file-name))))
;;                          (nreverse (dom-by-tag (with-temp-buffer
;;                                                  (insert-file-contents data-file)
;;                                                  (funcall xml-parsing-func
;;                                                           (point-min)
;;                                                           (point-max)))
;;                                                'bookmark))))
;;          (message "consult: List of XDG recent files not found")
;;          '())))
;;  (defun consult--recent-files-sort (file-list)
;;    "Sort the FILE-LIST by modification time, from most recent to least recent."
;;    (thread-last
;;        file-list
;;      ;; Use modification time, since getting file access time seems to count as
;;      ;; accessing the file, ruining future uses.
;;      (mapcar (lambda (f)
;;                (cons f (file-attribute-modification-time (file-attributes f)))))
;;      (seq-sort (pcase-lambda (`(,f1 . ,t1) `(,f2 . ,t2))
;;                  ;; Want existing, most recent, local files first.
;;                  (cond ((or (not (file-exists-p f1))
;;                             (file-remote-p f1))
;;                         nil)
;;                        ((or (not (file-exists-p f2))
;;                             (file-remote-p f2))
;;                         t)
;;                        (t (time-less-p t2 t1)))))
;;      (mapcar #'car)))
;;  (defun consult--recent-files-mixed-candidates ()
;;    "Return a list of files recently used by Emacs and the system.
;;
;;  These files are sorted by modification time, from most recent to least."
;;    (thread-last
;;        (consult--recent-system-files)
;;      (seq-filter #'recentf-include-p)
;;      (append (mapcar #'substring-no-properties recentf-list))
;;      delete-dups
;;      (consult--recent-files-sort)))
;;  (defcustom consult-include-system-recent-files nil
;;    "Whether to include files used by other programs in `consult-recent-file'."
;;    :type 'boolean
;;    :group 'consult)
;;  ;;;###autoload
;;  (defun consult-recent-file ()
;;    "Find recent using `completing-read'."
;;    (interactive)
;;    (find-file
;;     (consult--read
;;      (or (mapcar #'abbreviate-file-name
;;                  (if consult-include-system-recent-files
;;                      (consult--recent-files-mixed-candidates)
;;                    recentf-list))
;;          (user-error "No recent files"))
;;      :prompt "Find recent file: "
;;      :sort nil
;;      :require-match t
;;      :category 'file
;;      :state (consult--file-preview)
;;      :history 'file-name-history)))
;;  (defvar consult--source-system-file
;;    `(:name     "System file"
;;                :narrow   ?F
;;                :category file
;;                :face     consult-file
;;                :history  file-name-history
;;                :action   ,#'consult--file-action
;;                :items
;;                ,(lambda ()
;;                   (let ((ht (consult--buffer-file-hash)))
;;                     (mapcar #'abbreviate-file-name
;;                             (seq-remove (lambda (x) (gethash x ht))
;;                                         (consult--recent-system-files))))))
;;    "Recent system file candidate source for `consult-buffer'.")
;;
;;  (defvar consult--source-mixed-file
;;    `(:name     "File"
;;                :narrow   ?f
;;                :category file
;;                :face     consult-file
;;                :history  file-name-history
;;                :action   ,#'consult--file-action
;;                :items
;;                ,(lambda ()
;;                   (let ((ht (consult--buffer-file-hash)))
;;                     (mapcar #'abbreviate-file-name
;;                             (seq-remove (lambda (x) (gethash x ht))
;;                                         (consult--recent-files-mixed-candidates))))))
;;    "File candidate source for `consult-buffer', including system files.
;;  This is meant as a replacement for `consult--source-file'.")
;;
;;  ;; Example: using the "mixed" source in `consult-buffer':
;;  (setq consult-buffer-sources
;;        '( consult--source-hidden-buffer
;;           consult--source-buffer
;;           consult--source-mixed-file
;;           consult--source-bookmark
;;           consult--source-project-buffer
;;           consult--source-project-file))
;;
)

(use-package consult-dir
  :straight t
  :general
  (hc/leader
    :infix "b"
    ""  '(nil :which-key "buffer management")
    "d" '("Consult-Dir" . consult-dir)
    "f" '("goto previous buffer" . consult-dir-jump-file)
    "b" '("Show Consult Buffers" . consult-buffer)
    "p" '("goto previous buffer". previous-buffer)
    "n" '("goto next buffer"  . next-buffer)
    "m" '("messages" . (lambda ()
                         (interactive)
                         (pop-to-buffer "*Messages*")))
    )
)
;;(use-package consult-flyspell
;;  :straight (consult-flyspell :type git :host gitlab :repo "OlMon/consult-flyspell" :branch "master")
;;  :config
;;  ;; default settings
;;  (setq consult-flyspell-select-function nil
;;        consult-flyspell-set-point-after-word t
;;        consult-flyspell-always-check-buffer nil)
;;)

(use-package consult-eglot
  :defer t
  :after lsp
  :straight (:build t)
)
(use-package consult-lsp
  :defer t
  :after lsp
  :straight (:build t)
)

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
)

(provide 'init-consult)

;;; init-consult.el ends here

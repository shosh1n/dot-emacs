;;; init-vterm .el  --- Working with crontab -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package vterm
  :defer t
  :straight (:build t)
  :config
  ;; Ensure we load vterm
  (require 'vterm)
  ;; -------------------------------
  ;; Overload vterm directory resolution
  ;; -------------------------------
  (defun vterm--get-directory (path)
    "[OVERLOADED] Get normalized directory to PATH."
    (when path
      (let (directory)
        (if (string-match "^\\(.*?\\)@\\(.*?\\):\\(.*?\\)$" path)
            (let ((user (match-string 1 path))
                  (host (match-string 2 path))
                  (dir  (match-string 3 path)))
              (if (and (string-equal user user-login-name)
                       (string-equal host (system-name)))
                  (when (file-directory-p dir)
                    (setq directory (file-name-as-directory dir)))
                (setq directory
                      (file-name-as-directory (concat (file-remote-p default-directory) dir)))))
          (when (file-directory-p path)
            (setq directory (file-name-as-directory path))))
        directory)))

  ;; -------------------------------
  ;; Automatically source shell scripts in vterm
  ;; -------------------------------
  (defun me/vterm-setup-scripts ()
    "Automatically source my shell scripts in every new vterm buffer."
    (let* ((basic-func-script (concat (getenv "HOME") "/dot-emacs/lisp/functions.sh"))
           (vterm-func-script (concat (file-name-directory (find-library-name "vterm"))
                                      "/etc/emacs-vterm-bash.sh")))
      (when (and (derived-mode-p 'vterm-mode) (get-buffer-process (current-buffer)))
        (vterm-send-string (format "source %s" basic-func-script))
        (vterm-send-return)
        (vterm-send-string (format "source %s" vterm-func-script))
        (vterm-send-return))))

  (add-hook 'vterm-mode-hook #'me/vterm-setup-scripts)

  ;; -------------------------------
  ;; Helpers for TRAMP-safe find-file-other-window
  ;; -------------------------------
  (defun me/vterm--ffow-resolver (file)
    "Resolve FILE for remote / sudo contexts."
    (cond
     ((string-prefix-p "/sudo::" file)
      (doom--sudo-file-path
       (concat (file-remote-p default-directory)
               (substring-no-properties file 7))))
     ((string-prefix-p "/" file)
      (concat (file-remote-p default-directory) file))
     (t file)))

  (defun me/vterm--find-file-other-window-wrapper (file)
    "Wrapper for ffow that handles remote paths."
    (find-file-other-window (me/vterm--ffow-resolver file)))

  (add-to-list 'vterm-eval-cmds
               '("find-file-other-window" me/vterm--find-file-other-window-wrapper))

  ;; -------------------------------
  ;; Always create a new vterm buffer
  ;; -------------------------------
  ;; Save original vterm function
  (defvar me/original-vterm (symbol-function 'vterm)
  "Original vterm function before aliasing.")
  (defun me/vterm-popup (&optional buffer-name)
    "Create a new vterm buffer in a dedicated popup frame."
    (interactive)
    (let* ((buf-name (or buffer-name (generate-new-buffer-name "*vterm*")))
           (buffer (get-buffer-create buf-name))
           (frame (make-frame `((name . ,buf-name)
                                (width . 100)
                                (height . 30)
                                (minibuffer . nil)
                                (undecorated . t)
                                (window-size-fixed . t)
                                (no-other-frame . t)))))
      ;; Set transparency
      (set-frame-parameter frame 'alpha '(85 . 85))
      (with-selected-frame frame
        (switch-to-buffer buffer)
        (unless (derived-mode-p 'vterm-mode)
          (funcall me/original-vterm buf-name))
        (run-at-time "0.1 sec" nil #'me/vterm-setup-scripts))
      frame))

  ;; Override vterm command safely
  (defalias 'vterm #'me/vterm-popup))
(provide 'init-vterm )

;;; init-vterm .el ends here

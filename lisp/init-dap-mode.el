;; init-dap-mode.el  --- Working with dap-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dap-mode
  :defer t
  :straight (:build t)
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)

  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "RUST::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::RUN"
         :gdboath "rust-lldb"
         :target nil
         :cwd nil)
   )
  )

(provide 'init-dap-mode)

;;; init-dap-mode.el ends here

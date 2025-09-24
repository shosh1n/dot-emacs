;;; init-consult.el  --- Working with consult -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(provide 'init-consult)

;;; init-consult.el ends here

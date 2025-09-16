;;; init-auctex.el --- Description -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package auctex
  :straight (:build t)
  :hook (tex-mode . prettify-symbols-mode)
  :hook (latex-mode . lsp-deferred)
  :config
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad)))))))

  (defun preview-larger-previews ()
    (setq preview-scale-function
          (lambda () (* 1.25
                   (funcall (preview-scale-from-face))))))
  )

(provide 'init-auctex)
;;; init-auctex.el ends here

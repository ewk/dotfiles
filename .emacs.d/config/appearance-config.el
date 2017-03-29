;;; appearance-config --- Summary

;;; Commentary:

;;;

;;; Code:


(delete-selection-mode t)
(transient-mark-mode t)
(setq select-enable-clipboard t) ;; Interact with system clipboard


;; fix terminal output characters
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(provide 'appearance-config)
;;; appearance-config ends here

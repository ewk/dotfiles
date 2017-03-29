;;; alias-config --- Summary

;;; Commentary:

;;;

;;; Code:


;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remap Meta-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; better way to enable ibuffer
(defalias 'list-buffers 'ibuffer)

;; ctags
(setq path-to-ctags "/usr/bin/etags")

;; invoke with ‘m-x create-tags’
(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "etags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
    )

(provide 'alias-config)
;;; alias-config ends here

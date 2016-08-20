;;; emacsclient-config.el --- Summary

;;; Commentary:
;;  Configuration for applications to connect to the running Emacs process.

;;; Code:


;; Set 'EDITOR=emacsclient [-t]' to connect to the running process
(require 'server)
(unless (server-running-p)
  (server-start))

;; Switch back to Emacs when another application invokes emacsclient
(defun px-raise-frame-and-give-focus ()
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(add-hook 'server-switch-hook 'px-raise-frame-and-give-focus)

(provide 'emacsclient-config)
;;; emacsclient-config ends here

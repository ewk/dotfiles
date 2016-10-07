;;; appearance-config --- Summary

;;; Commentary:

;;;

;;; Code:

;; Turn off splash screen and scratch mode message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Menu bar
(tool-bar-mode -1) ;; Hide toolbar buttons
(define-key menu-bar-tools-menu [games] nil)

;; Show filename in frame
(setq frame-title-format
      ;;(list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b")));;)
;;(setq frame-title-format '(buffer-file-name "%f" ("%b")))
;;(setq frame-title-format "%b - emacs") ; Use buffer name as frame title
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Themes
(if window-system
    (load-theme 'zenburn t)
  (load-theme 'wombat t))

;; Window size
(setq initial-frame-alist '((top . 00) (left . 00)
                            (width . 90) (height . 50)))

;; Appearance
(if (eq system-type 'darwin)
    (set-default-font "Menlo-14")
  (set-default-font "Droid Sans Mono-14"))

;; TODO keep cursor at same point when scrolling; use C-l for now

;; Modeline
(column-number-mode 1) ; Show column number in mode-line

(global-linum-mode 1) ; Line numbers in all buffers
(delete-selection-mode t)
(transient-mark-mode t)
(setq select-enable-clipboard t) ;; Interact with system clipboard

;; Turn off beeping and improve keyboard response
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t) ;; I am not a robot. I mean, the humans are dead.

;; Highlighting
(electric-pair-mode t) ;; Close pairs automatically
(show-paren-mode t) ; Highlight parenthesis pairs
(setq blink-matching-paren t) ; Blinking parenthesis, on by default
(setq show-paren-style 'expression) ; Highlight text between parens
(global-hl-line-mode t) ; Highlight cursor line
(set-face-background 'hl-line "black") ; Set line highlight color
(set-face-foreground 'highlight nil) ; Keep syntax highlighitng on highlighted line
(blink-cursor-mode 0)

;; fix terminal output characters
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(provide 'appearance-config)
;;; appearance-config ends here

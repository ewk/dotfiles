;; Show filename in frame
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Backups and autosave
(setq make-backup-files nil) ; No backup files ~
(setq auto-save-timeout 30) ; Autosave every 30 seconds; also the default

;; Buffer management
(setq pop-up-frame t) ; Buffers in separate frames
(setq frame-title-format "%b - emacs") ; Use buffer name as frame title
(setq enable-recursive-minibuffers t) ; Stack minibuffers
(setq large-file-warning-threshold nil) ; Don't warn opening large files

;; Where is aspell?
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))

(setq-default ispell-list-command "list")


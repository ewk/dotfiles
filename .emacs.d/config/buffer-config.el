;;; Code

(set-language-environment "UTF-8")

;; Backups and autosave
(setq make-backup-files nil) ; No backup files ~
(setq auto-save-timeout 30) ; Autosave every 30 seconds; also the default
(global-auto-revert-mode t) ; Reload files changed outside Emacs

;; Buffer management
(setq pop-up-frame t) ; Buffers in separate frames
(setq enable-recursive-minibuffers t) ; Stack minibuffers
(setq large-file-warning-threshold nil) ; Don't warn opening large files

;; Where is aspell?
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))

(setq-default ispell-list-command "list")

;; Enable spell checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


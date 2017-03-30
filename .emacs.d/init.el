;;; init.el --- Summary

;;; Commentary:

;;; Code:

;;
;; General Config
;;
(load "~/.emacs.d/config/appearance-config.el")

;; Turn off scrollbars early in startup to avoid window width weirdness.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(define-key menu-bar-tools-menu [games] nil)

;; Window size
(setq initial-frame-alist '((top . 00) (left . 00)
                            (width . 90) (height . 50)))
;; Turn off splash screen and scratch mode message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Initialize must come before configurations installed packages.
(package-initialize)
(load "~/.emacs.d/config/package-config.el")

(require 'cl-lib)
(set-language-environment "UTF-8")

(column-number-mode 1) ; Show column number in mode-line
(global-linum-mode 1) ; Line numbers in all buffers

;; Turn off beeping and improve keyboard response
(blink-cursor-mode 0) ; Disable cursor blinking
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t) ;; I am not a robot. I mean, the humans are dead.

;; Show filename in frame
(setq frame-title-format
      ;;(list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b")));;)
;;(setq frame-title-format '(buffer-file-name "%f" ("%b")))
;;(setq frame-title-format "%b - emacs") ; Use buffer name as frame title
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Buffer management
(setq pop-up-frame t) ; Buffers in separate frames
(setq enable-recursive-minibuffers t) ; Stack minibuffers
(setq large-file-warning-threshold nil) ; Don't warn opening large files
(savehist-mode 1) ; Save minibuffer history

;; Themes
(if window-system
    (load-theme 'material t)
  (load-theme 'wombat t))

;; Cursorline
(global-hl-line-mode t) ; Highlight cursor line
(set-face-background 'hl-line "black") ; Set line highlight color
(set-face-foreground 'highlight nil) ; Keep syntax highlighitng on highlighted line

;; Font
(if (eq system-type 'darwin)
    (set-frame-font "Menlo-14")
  (set-frame-font "Droid Sans Mono-14"))

;; TODO keep cursor at same point when scrolling; use C-l for now

;; Highlighting
(electric-pair-mode t) ;; Close pairs automatically
(show-paren-mode t) ; Highlight parenthesis pairs
(setq blink-matching-paren t) ; Blinking parenthesis, on by default
(setq show-paren-style 'expression) ; Highlight text between parens
(transient-mark-mode t) ;; Highlight mark selection

;; Editing
(delete-selection-mode t) ;; Replace selected text
(setq select-enable-clipboard t) ;; Interact with system clipboard

;; Fix terminal output characters
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)


;;
;; Backups and autosave
;;
(setq make-backup-files nil) ; No backup files ~
(setq auto-save-timeout 30) ; Autosave every 30 seconds; also the default
(global-auto-revert-mode t) ; Reload files changed outside Emacs

;; Remember last position in each file- .emacs.d/places must exist
(save-place-mode 1)

;;
;; Indentation
;;

;; Tabs and spaces
(setq-default indent-tabs-mode t) ; Whether tabs or spaces are used for indentation

;; tab-width, c-basic-offset, and cperl-indent-level should always be the same
(setq-default tab-width 8) ;; Width of a tab

;; Indentation in CC mode applies to C as well as most other languages
(setq-default c-basic-offset 8)
(setq-default cperl-indent-level 8) ;; Perl, of course, is different
;;(setq c-default-style "linux")

;; Sentences end with one space
(setq sentence-end-double-space nil)

;; Auto indent on newline - Ctrl-j
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Add blank line to end of buffer
(setq next-line-add-newlines t)

;; Do not wrap text in programming mode
(add-hook 'c-mode-hook (lambda () (setq truncate-lines t)))

;; Soft wrap plain text
;;(add-hook 'text-mode-hook
;;	  (lambda ()
;;	    ;;(setq visual-line-mode 1)
;;	    'turn-on-auto-fill
;;	    ))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;
;; Syntax and filetype
;;
(global-flycheck-mode) ;; Enable global syntax check

;; Syntax of these languages can be fussy
;; C, Perl, sh, zsh, and mmix inherit CC mode
;; Make must always use tabs, never spaces.
(setq html-indent-level 2) ;; 2 soft spaces
(setq ruby-indent-level 2) ;; 2 soft spaces
(setq css-indent-offset 4) ;; 4 space tabs

;; Go mode
(add-hook 'go-mode-hook
          (lambda ()
	    (require 'company-go)
	    (setq gofmt-command "goimports")
	    (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

;; Set major mode for additional file types
(setq auto-mode-alist
  (append
    '(("\\.hdl$" . vhdl-mode))
    auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("rc$" . conf-mode))
(require 'log-edit)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . vc-git-log-edit-mode))

;; Version control
(global-git-gutter-mode t)
(git-gutter:linum-setup)
(add-to-list 'vc-handled-backends 'Git)

;;
;; Whitespace
;;

;; Toggle whitespace mode for Emacs session
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Highlight lines over 80 columns. Requires whitespace-style (face)
(setq whitespace-line-column 81)

;; Select types of whitespace to highlight
(setq whitespace-style '(face tabs tab-mark newline newline-mark lines-tail trailing))

;; Select characters to represent whitespace
;; All numbers are Unicode codepoint. View with (insert-char 182)
(setq whitespace-display-mappings '(
	(tab-mark 9 [9654 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▶」
        (newline-mark 10 [182 10]) ; 10 LINE FEED 「¶」
   ))

;; Customize colors highlighting whitespace
(custom-set-faces
  '(whitespace-line ((t (:foreground "black" :background "magenta"))))
  '(whitespace-newline ((t (:foreground "white" :background nil))))
  '(whitespace-tab ((t (:foreground "white" :background nil))))
  '(whitespace-trailing ((t (:foreground "red" :background "yellow"))))
  )

;; Empty lines
(setq-default indicate-empty-lines t)

;; This may interfere with version control and next-line-add-newlines
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;; Completion
;;

;; Turn on company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Trigger completion with TAB
;(global-set-key "\t" 'company-complete)
(setq company-tooltip-limit 15)
(setq company-idle-delay 0)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))

; Interactive do things- find file auto complete
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)

;;
;;
;; Spell checking
;;

;; Where is aspell?
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;; Enable spell checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;
;; Ctags
;;

;;
;; Alias customization
;;
(load "~/.emacs.d/config/alias-config.el")

(load "~/.emacs.d/config/org-config.el")

;;
;; Emacsclient
;;
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


(provide 'init)
;;; init ends here

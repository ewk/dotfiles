(set-language-environment "UTF-8")

;; Load path
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/modules/")
;(add-to-list 'load-path "~/.emacs.d/modules/auto-complete-1.3.1")

;; Replace package archive with melpa
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Appearance
(set-default-font "Inconsolata-14" "Menlo-12")
(require 'zenburn-theme)
(column-number-mode 1) ; Show column number in mode-line
(global-linum-mode 1) ; Line numbers in all buffers
;; Window size
(setq initial-frame-alist '((top . 10) (left . 30)
                            (width . 90) (height . 50)))

;; Tabs and spaces
(setq-default tab-width 8) ;; Set tab width
(setq-default c-basic-offset 8) ;; Applies to C and most languages
(setq-default cperl-indent-level 8) ;; Perl, of course, is different
(setq-default indent-tabs-mode t) ; nil will use spaces instead of tabs
;(setq sentence-end-double-space nil) ; Sentences end with one space
(global-set-key (kbd "C-c w") 'whitespace-mode) ; view all whitespace characters
;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Menu bar
(define-key menu-bar-tools-menu [games] nil)
(tool-bar-mode 0)
(setq inhibit-startup-message t) ; No message at startup

;; enable electric pair in all buffers
(electric-pair-mode t)
(defun my-disable-electric-indentation ()
  "Stop ';', '}', etc. from re-indenting the current line." 
  (c-toggle-electric-state -1)) 
(add-hook 'c-mode-common-hook 'my-disable-electric-indentation)

;; Highlighting
(show-paren-mode 1) ; Highlight parenthesis pairs
(setq blink-matching-paren t) ; Blinking parenthesis, on by default
(setq show-paren-style 'expression) ; Highlight text between parens
(global-hl-line-mode t) ; Highlight cursor line
(blink-cursor-mode 0)
(setq visible-bell t)

; Interactive do things- find file auto complete
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)
;(global-set-key (kbd "C-x C-b") 'ibuffer) ; replace buffer management
; better way to enable ibuffer
(defalias 'list-buffers 'ibuffer)

;; Buffer management
(setq pop-up-frame t) ; Buffers in separate frames
(setq frame-title-format "%b - emacs") ; Use buffer name as frame title
(setq enable-recursive-minibuffers t) ; Stack minibuffers

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
;; (setq confirm-kill-emacs 'yes-or-no-p) ;; Never quit accidentally

;; Backups and autosave
(setq make-backup-files nil) ; No backup files ~
(setq auto-save-timeout 60) ; Autosave every minute

;; Print argument list of function you are writing
(eldoc-mode 1)

;; Default mode
(setq default-major-mode 'text-mode) ; Text-mode is default mode
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; auto-formatting in text-mode

;; unique buffer names
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Got a wheel mouse!
(mouse-wheel-mode 1)

(setq kill-whole-line t)

;; Some small hacks Chris Kirchen
(setq user-mail-address "edkovsky@gmail.com")
(setq display-time-24hr-format t)
(display-time-mode 1)
(setq isearch-lazy-highlight nil)
(setq Info-use-header-line nil)
(setq Info-fontify nil)
(setq ps-paper-type 'a4)
(setq kill-read-only-ok t)
(setq diff-switches "-urN")

;; (require ruby-mode)
;; .rb and .rbx are Ruby files
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rbx$" . ruby-mode))

;; go-mode
;;(require 'go-mode)
;;(add-hook 'go-mode-hook 'subword-mode)
;;(add-hook 'go-mode-hook #'(lambda () (setq fill-column 79)))
;;(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

;; Use a better perl mode
(fset 'perl-mode 'cperl-mode)

;; For emacsclient
;(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c885b3bb5811fe87fef057db52b00794c177660c24376ae220e5f2a71ae34b83" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)

;; company-mode autocomplete
;(require 'company)
;(add-hook 'after-init-hook 'global-company-mode)

;; auto-complete mode
;(add-to-list 'load-path "~/.emacs.d")
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(require 'auto-complete-config)
;(ac-config-default)

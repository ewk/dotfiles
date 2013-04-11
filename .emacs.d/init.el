;; Load path
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/modules/")

;; Window size
(setq initial-frame-alist '((top . 10) (left . 30)
                            (width . 90) (height . 50)))
;; Tabs and spaces
(setq-default tab-width 2) ;; Should probably stop doing this
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
;;(setq-default c-basic-offset 2)
;;(setq sentence-end-double-space nil) ; Sentences end with one space

;; Appearance
(set-default-font "Inconsolata-14" "Menlo-12")
(require 'zenburn-theme)
(column-number-mode 1) ; Show column number in mode-line
(global-linum-mode 1) ; Line numbers in all buffers

;; Dictionary
(setq ispell-dictionary "english") ; Set ispell dictionary

;; Menu bar
(define-key menu-bar-tools-menu [games] nil)
(tool-bar-mode 0)
(setq inhibit-startup-message t) ; No message at startup

;; enable autopair in all buffers
(require 'autopair)
(autopair-global-mode)

;; Highlighting
(show-paren-mode 1) ; Highlight parenthesis pairs
(setq blink-matching-paren-distance nil) ; Blinking parenthesis
(setq show-paren-style 'expression) ; Highlight text between parens
(global-hl-line-mode t) ; Highlight cursor line

; Interactive do things- find file auto complete
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(icomplete-mode t) ; Completion in mini-buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; Buffer management
(setq pop-up-frame t) ; Buffers in separate frames
(setq frame-title-format "%b - emacs") ; Use buffer name as frame title
(setq enable-recursive-minibuffers t) ; Stack minibuffers

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

;; Backups and autosave
(setq make-backup-files nil) ; No backup files ~
(setq auto-save-timeout 60) ; Autosave every minute

;; Print argument list of function you are writing
(eldoc-mode 1)

;; Default mode
(setq default-major-mode 'text-mode) ; Text-mode is default mode
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; auto-formatting in text-mode

;; Keyboard shortcuts
;; Make Emacs autoindent
	(define-key global-map (kbd "RET") 'newline-and-indent)

;; Kernel style
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))
;; End Kernel style

;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; Auto-complete
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

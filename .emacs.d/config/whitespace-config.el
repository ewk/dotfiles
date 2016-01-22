;; Empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Whitespace
(global-set-key (kbd "C-c w") 'whitespace-mode) ; view all whitespace characters
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Tabs and spaces
(setq-default tab-width 8) ;; Set tab width
;;(setq c-default-style "linux")
(setq-default c-basic-offset 8) ;; Applies to C and most languages
(setq-default cperl-indent-level 8) ;; Perl, of course, is different
;(setq-default indent-tabs-mode t) ; nil will use spaces instead of tabs
(setq sentence-end-double-space nil) ; Sentences end with one space
;; Auto indent on newline - Ctrl-j
(define-key global-map (kbd "RET") 'newline-and-indent)
;; Add blank line to end of buffer
(setq next-line-add-newlines t)

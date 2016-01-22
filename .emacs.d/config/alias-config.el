;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remap Meta-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; better way to enable ibuffer
(defalias 'list-buffers 'ibuffer)

;; Set EDITOR=emacsclient [-t] to connect to the running process
(require 'server)
(unless (server-running-p)
  (server-start))

function man --description "colorized man page output"
  # http://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized

  # bold
  set -x LESS_TERMCAP_md (set_color --bold blue)
  set -x LESS_TERMCAP_me (set_color normal)

  # inverted colors
  set -x LESS_TERMCAP_so (set_color 949494)
  set -x LESS_TERMCAP_se (set_color normal)

  # underline
  set -x LESS_TERMCAP_us (set_color afafd7 --underline)
  set -x LESS_TERMCAP_ue (set_color normal)

  set -lx GROFF_NO_SGR yes # fix groff misbehavior on Fedora

  command man $argv
end

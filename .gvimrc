" Set font
if has('gui_running')
  if has('macunix') "Mac must come first or it will try to load unix font
    set guifont=Menlo:h14 
  elseif has('unix')
    silent! set guifont=Droid\ Sans\ Mono\ 14
    "silent! set guifont=Deja\ Vu\ Sans\ Mono\ 14
    "silent! set guifont=Inconsolata\ Medium\ 14
  elseif has('win32')
    set guifont=Consolas:h11:cANSI
  endif
endif

" Size of GVim window
set lines=50 columns=90 

" Start without toolbar
set guioptions=aegmrL 

" Set font
if has('gui_running')
  if has('macunix') "Mac must come first or it will try to load Unix font
    silent! set guifont=Menlo:h14
  elseif has('unix')
    silent! set guifont=Droid\ Sans\ Mono\ 14
    "silent! set guifont=Deja\ Vu\ Sans\ Mono\ 14
    "silent! set guifont=Inconsolata\ Medium\ 14
  elseif has('win32')
    silent! set guifont=Consolas:h11:cANSI
  endif
endif

" Size of GVim window
set lines=50 columns=90

" Visual mode selection is available to system clipboard.
" Use console dialogs instead of popup windows.
" Change text in tab labels.
" Use the gVim icon.
" Grey out unused menu items.
" And menu bar is present.
" Right hand scrollbar is always present.
" Left hand scrollbar is present for vertical split.
" Start without toolbar: !T.
" see :help guioptions for full details.
set guioptions=+aceigmrL

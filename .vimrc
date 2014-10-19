" source /path/to/external/config/file

" Use Vim settings, rather then Vi settings 
set nocompatible 

" Use UTF-8.
set encoding=utf-8 

" Hide startup message
set shortmess+=I 

" pathogen will load any file or directory in .vim/bundle; must load before filetype
filetype off
execute pathogen#infect()
filetype plugin indent on
"call pathogen#runtime_append_all_bundles()
"call pathogen#infect()
"call pathogen#helptags() 

" ================ General Config ====================
set number "enable line numbering at paragraphs
set backspace=indent,eol,start  "Allow backspace in insert mode
set history=50 " number of commands and searches to keep in history
set undolevels=50 "many levels of undo 
set gcr=a:blinkon0              "Disable cursor blink
set autoread                    "Reload files changed outside vim
set visualbell " no beeping
set noerrorbells " I said no beeping!
set title " change terminal title
set autochdir "pwd is always same as current file 
set hidden " Hides buffers instead of closing them 
syntax on " Enable syntax highlighting.
set modelines=0 " For security? 
set ttyfast
autocmd FocusLost * :wa " Autosave
set grepprg=ack\ -k " use ack instead of grep; doesn't change vimgrep
let g:netrw_liststyle=1 " Use list style in Netrw :E

" Remember last location in file
if has("autocmd")
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal g'\"" | endif
endif

" ================ Search Settings  =================
set incsearch        "Find the next match as you type 
set hlsearch         "Highlight searches by default
set viminfo='100,f1  "Save up to 100 marks, enable capital marks
set ignorecase       "Ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase, case-sensitive otherwise
" Use very magic search by default
nnoremap / /\v
vnoremap / /\v
"set path+=Projects/** "Allows :find file to drill down subdirectories without specifying full path

" ================ Turn Off Swap Files ==============
set nobackup   " No more backup files
set noswapfile "No more swap files for git to ignore; turn this on for large files
set nowb       " Prevents automatic write backup before overwriting file

" ================ Indentation ======================
set autoindent    " Always autoindent and copy previous indentation on copy 
"set smartindent   " Automatically indent when adding a curly bracket, etc.
set smarttab      " insert tabs on the start of a line according to shiftwidth, not tabstop
" You will nearly always want tabstop == softtabstop == shiftwidth
set tabstop=4     " Width of tab character
set softtabstop=4 " Fine tune amount of white space to be inserted; higher precedence than tabstop
set expandtab     " use spaces in place of tab characters using softtabstop value 
set shiftwidth=4  " Amount of white space to insert or remove in normal mode
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set copyindent    " Copy the structure of the existing lines indent when autoindenting a new line. Uses spaces instead of tabs if expandtab is set   

" Syntax of these languages is fussy over tabs Vs spaces
autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
" autocmd FileType python setlocal ts=4 sts=4 sw=4 set noexpandtab
" vim mangles kernel patches; need a better solution- this is useless
"autocmd FileType patch setlocal ts=8 sts=8 sw=8 noexpandtab nowrap
autocmd FileType c setlocal ts=8 sts=8 sw=8 noexpandtab nowrap
autocmd FileType html setlocal ts=2 sts=2 sw=2 expandtab nowrap
autocmd FileType css setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType javascript setlocal ts=4 sts=4 sw=4 noexpandtab
autocmd FileType ruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType perl setlocal ts=8 sts=8 sw=8 noexpandtab nowrap
autocmd FileType mix setlocal ts=8 sts=8 sw=8 noexpandtab nowrap

" Treat .rss files as XML
autocmd BufNewFile,BufRead *.rss setfiletype xml
autocmd BufNewFile,BufRead *.mixal setfiletype asm

"filetype plugin indent on " load plugin and Indent based on filetype
runtime macros/matchit.vim
"set matchpairs+=<:>

" ================ File Format ====================== 
" Margins and word wrapping
set wrap " Wrap sets how text is displayed based on window size. Textwidth sets column width in the buffer.
"set textwidth=80 " Warning is set in Visual Clues section; enforcement is not automatic with textwidht turned off
" set wrapmargin=5 "wordwrapping at right hand column; ignored if textwidth is on
set whichwrap+=<,>,h,l  " backspace and cursor keys wrap to next/prev lines 

" Linebreak
" set linebreak "break at word. 
set showbreak=… "does what it says 

" Whitespace
set listchars=tab:▸\ ,eol:¬,extends:#,nbsp:·
set list " highlight trailing whitespace; conflicts with linebreak
autocmd filetype html,xml set listchars-=tab:>. " except for html

" Paste formatting
set formatoptions=tcq "This is Vim's default 
set nopaste "no formatting is done when paste is ON 

"set nostartofline "keep cursor at same position when scrolling
set virtualedit=block " Visual select white space

" ================ Folds ============================
set foldmethod=indent   "fold based on indent, can also be syntax
set foldnestmax=3       "deepest fold is 3 levels
set nofoldenable        "don't fold by default
set foldcolumn=2 "Display fold depth 

" ================ Completion =======================
set wildmode=full
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildignorecase          "ignore case on filename completion using :
set wildignore+=*.o,*.obj,*.git,*.rbc,*.swp,*.bak,*.pyc,*.class
set ofu=syntaxcomplete#Complete " turn on omnicompletion 

" ================ Scrolling ========================
set scrolloff=8         "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

" ================ Status line ======================
set laststatus=2                             "Always show status line
set statusline=
set statusline+=%-3.3n\                      " buffer number
set statusline+=%f\                          " filename
set statusline+=%h%m%r%w                     " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}] " file type
set statusline+=%=                           " right align remainder
set statusline+=0x%-8B                       " character value
set statusline+=%-14(%l,%c%V%)               " line, character
set statusline+=%<%P                         " file position
set ruler                                    " Show line number, cursor position.
set showcmd                                  " Display incomplete commands.
set showmode                                 " Show editing mode

" ================ Spell Checking ======================
set spell
set mousemodel=popup

" ================ Color scheme ======================
if has("gui_running")
    colorscheme zenburn
else
    colorscheme badwolf "zenburn
endif

" ================ Visual clues ======================
set startofline "keep cursor at same position when scrolling
"if has("gui_running")
    set cursorline
    highlight CursorLine guibg=black
"endif
highlight ColorColumn guibg=magenta
" set colorcolumn=81
" instead setting colorcolumn, matchadd only highlights lines pas 80 columns
call matchadd('ColorColumn', '\%81v', 100)
set showmatch " highlight matching pairs
highlight Visual guibg=DarkSlateGray ctermbg=lightblue
" stop typing commands in insert mode! 
autocmd InsertEnter * hi Normal ctermbg=234 guibg=#000000
autocmd InsertLeave * hi Normal ctermbg=232 guibg=#3f3f3f

" ================ CTags ======================
noremap <Leader>t :!ctags --extra=+f -R *<CR><CR>
"Load tags recursively from working directory
set tags=./tags;

" ================ Templates ======================
"Load template for new files
autocmd BufNewFile * silent! 0r $VIMHOME/templates/%:e.tpl

" ================ Keyboard customization ======================
" change the mapleader from \ to ,
let mapleader="," 

" Clear highlighted search
nnoremap <silent> <leader>/ :nohlsearch<CR>

" quick exit from insert mode
:inoremap jk <esc>

" Strip all trailing whitespace in the current file
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Use leader to paste from system clipboard instead of global register gymnastics
noremap <leader>v "+p
"inoremap <leader>v <esc>"+gPi

" And copy to system clipboard
noremap <leader>c "+y

" To save, press ctrl-s.
nnoremap <c-s> :w<CR> 
inoremap <c-s> <Esc>:w<CR>a

" Easy window navigation
noremap <C-h> <C-w>h 
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Toggle in insert mode to paste large amounts of text in terminal vim
set pastetoggle=<F2> 

" Use Q to reformat the current paragraph (or selection) after pasting
vnoremap Q gq 
nnoremap Q gqap

" When you forget to open a file with sudo
cnoremap w!! w !sudo tee % >/dev/null 

" Opens an edit command with the path of the currently edited file filled in
" Normal mode: <Leader>e
noremap <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Inserts the path of the currently edited file into a command
" Command mode: Ctrl+P
cnoremap <C-P> <C-R>=expand("%:p:h") . "/" <CR>

" Keep cursor in place when joining lines
nnoremap J mzJ`z

" Move by rows rather than lines
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" Swap ; and :
nnoremap ; :
nnoremap : ;

" ================ Useful abbreviations ======================
iab retrun return
iab pritn print
iab hbs #! /bin/sh
iab hbp #! /usr/bin/env perl<CR>use 5.014; use warnings; use autodie;<CR>
iab hbr #! /usr/bin/env ruby -w
autocmd FileType c iabbrev for( for (i = 0; x < var; x++) {<cr><cr>}

" ================ Functions ======================


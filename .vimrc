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
set autochdir										"pwd is always same as current file 
set hidden " Hides buffers instead of closing them 
syntax on " Enable syntax highlighting.
set modelines=0 " For security? 
set ttyfast
au FocusLost * :wa " Autosave

" Remember last location in file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal g'\"" | endif
endif

" ================ Search Settings  =================
set incsearch        "Find the next match as you type 
set hlsearch         "Higlight searches by default
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
set smartindent   " Automatically indent when adding a curly bracket, etc.
set smarttab      " insert tabs on the start of a line according to shiftwidth, not tabstop
" You will nearly always want tabstop == softtabstop == shiftwidth
set tabstop=2     " Width of tab character
set softtabstop=2 " Fine tune amount of white space to be inserted; higher prcedence than tabstop
set expandtab     " use spaces in place of tab characters using softtabstop value 
set shiftwidth=2  " Amount of white space to insert or remove in normal mode
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set copyindent    " Copy the structure of the existing lines indent when autoindenting a new line. Uses spaces instead of tabs if expandtab is set   

" Syntax of these languages is fussy over tabs Vs spaces
autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType python setlocal ts=4 sts=4 sw=4 set noexpandtab
autocmd FileType c setlocal ts=8 sts=8 sw=8 noexpandtab
autocmd FileType html setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType css setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType javascript setlocal ts=4 sts=4 sw=4 noexpandtab
  
" Treat .rss files as XML
autocmd BufNewFile,BufRead *.rss setfiletype xml

"filetype plugin indent on " load plugin and Indent based on filetype
runtime macros/matchit.vim

" ================ File Format ====================== 
" Margins and word wrapping
set wrap
set textwidth=80
" set wrapmargin=5 "wordwrapping at right hand column; ignored if textwidth is on
set whichwrap+=<,>,h,l  " backspace and cursor keys wrap to next/prev lines 

" Linebreak
" set linebreak "break at word. 
set showbreak=… "does what it says 

" Whitespace
set list " highlight trailing whitespace; conflicts with linebreak 
set listchars=tab:▸\ ,eol:¬,extends:#,nbsp:.
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
    colorscheme desert 
endif

" ================ Visual clues ======================
set startofline "keep cursor at same position when scrolling
set cursorline
highlight CursorLine guibg=black ctermbg=lightblue
set showmatch " highlight matching pairs

" ================ CTags ======================
map <Leader>rt :!ctags --extra=+f -R *<CR><CR>
"Load tags auotmatically from working directory
:set tags=./tags,/~/Projects

"Load template for new files
:autocmd BufNewFile * silent! 0r $VIMHOME/templates/%:e.tpl

" Sample abbreviation for C files
:iabbrev <buffer> for (x=0;x<var;x++){<cr><cr>}

"Load tags automatically from working directory
:set tags=./tags,/~/Projects

" ================ Keyboard customization ======================
" change the mapleader from \ to ,
let mapleader="," 

" Clear highlighted search
nmap <silent> <leader>/ :nohlsearch<CR>

" Strip all trailing whitespace in the current file
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Use leader to paste from system clipboard instead of global register gymnastics
map <leader>v "+gP

" To save, press ctrl-s.
nmap <c-s> :w<CR> 
imap <c-s> <Esc>:w<CR>a

" Easy window navigation
map <C-h> <C-w>h 
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Toggle in insert mode to paste large amounts of text in terminal vim
set pastetoggle=<F2> 

" Use Q to reformat the current paragraph (or selection) after pasting
vmap Q gq 
nmap Q gqap

" When you forget to open a file with sudo
cmap w!! w !sudo tee % >/dev/null 

" Opens an edit command with the path of the currently edited file filled in
" Normal mode: <Leader>e
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Inserts the path of the currently edited file into a command
" Command mode: Ctrl+P
cmap <C-P> <C-R>=expand("%:p:h") . "/" <CR>

" Keep cursor in place when joining lines
nnoremap J mzJ`z

" Move by rows rather than lines
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" You idiot, stop typing in insert mode! 
au InsertEnter * hi Normal ctermbg=234 guibg=#000000
au InsertLeave * hi Normal ctermbg=232 guibg=#3f3f3f

" source /path/to/external/config/file

" ================ MUST RUN FIRST ====================
set nocompatible " Use Vim settings, rather then Vi settings
set encoding=utf-8 " Use UTF-8
filetype plugin indent on
" ================ END MUST RUN FIRST ====================

" ================ General Config ====================
set shortmess+=I " Hide startup message
let mapleader="," " change the mapleader from \ to ,
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
syntax enable " Enable syntax highlighting.
set modelines=0 " Vim default is on unless root; turn off for security
set ttyfast
let g:netrw_liststyle=1 " Use list style in Netrw :E
augroup AutoSave
    autocmd!
    autocmd FocusLost * :wa " Autosave
    " Strip trailing whitespace before saving. Can restrict to certain
    " filetypes with: *.{md,pl,pl6,rb,t,xml,yaml,go}
    autocmd BufWritePre * :%s/\s\+$//e
    " Remember last location in file:
    autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
                \| execute "normal! g'\"" | endif
augroup END
set lazyredraw " redraw only when we need to for faster macros
set nrformats= " treat all numerals as decimal instead of octal
set fileformats=unix,dos,mac " Prefer Unix over Windows over OS 9 formats

" ================ Visual clues ======================
colorscheme zenburn
set startofline " Keep cursor at same position when scrolling
set cursorline
highlight CursorLine guibg=black
highlight ColorColumn guibg=magenta
call matchadd('ColorColumn', '\%81v', 100) " highlight lines past 80 columns
set showmatch " highlight matching pairs
highlight MatchParen guifg=yellow guibg=black gui=bold

" ================ Search Settings  =================
set incsearch        " Find the next match as you type
set hlsearch         " Highlight searches by default
set viminfo='100,f1  " Save up to 100 marks, enable capital marks
set ignorecase       " Ignore case when searching
set smartcase        " Ignore case if search is all lowercase, else case-sensitive
" Use very magic search by default
nnoremap / /\v
vnoremap / /\v
" Clear highlighted search
nnoremap <silent> <leader>/ :nohlsearch<CR>

" ================ Turn Off Swap Files ==============
set nobackup   " No more backup files
set noswapfile " No more swap files for git to ignore; turn this on for large files
set nowb       " Prevents automatic write backup before overwriting file

" ================ Indentation ======================
set autoindent    " Always autoindent and copy previous indentation on copy
set smarttab      " Insert tabs on the start of a line according to shiftwidth, not tabstop
" You will nearly always want tabstop == softtabstop == shiftwidth
set tabstop=4     " Width of tab character
set softtabstop=4 " Fine tune amount of white space to be inserted; higher precedence than tabstop
set expandtab     " Use spaces in place of tab characters using softtabstop value
set shiftwidth=4  " Amount of white space to insert or remove in normal mode
set shiftround    " Use multiple of shiftwidth when indenting with '<' and '>'
set copyindent    " Copy the structure of the existing lines indent when autoindenting a new line. Uses spaces instead of tabs if expandtab is set

" ================ File Format ======================
set whichwrap+=<,>,h,l,[,] " Movement keys wrap at beginning/end of line

" Highlight trailing whitespace
set listchars=tab:▸\ ,extends:#,nbsp:·,eol:¬
set list " Setting list disables linebreak; never use with showbreak!
set virtualedit=block " Visual select white space

" Syntax of these languages can be fussy
augroup Filetypes
    autocmd!
    autocmd FileType c,perl,sh,zsh,mmix setlocal ts=8 sts=8 sw=8 noexpandtab nowrap
    autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
    autocmd FileType html setlocal ts=2 sts=2 sw=2 expandtab nowrap
    autocmd FileType css setlocal ts=2 sts=2 sw=2 expandtab nowrap
    autocmd FileType ruby setlocal ts=2 sts=2 sw=2 expandtab nowrap
    autocmd BufReadPost,FileReadPost *.h setlocal filetype=c " C header files
    autocmd FileType go setlocal nowrap
augroup END

" ================ Folds ============================
set foldmethod=indent   " Fold based on indent, can also be syntax
set foldnestmax=3       " Deepest fold is 3 levels
set nofoldenable        " Don't fold by default
set foldcolumn=2        " Display fold depth

" ================ Completion =======================
set wildmode=list:longest
set wildmenu                " Enable ctrl-n and ctrl-p to scroll thru matches
set wildignorecase          " Ignore case on filename completion using :
set wildignore+=*.o,*.obj,*.git,*.rbc,*.swp,*.bak,*.pyc,*.class
set omnifunc=syntaxcomplete#Complete " Turn on omnicompletion
" Vim looks for keywords based on values specified by this variable.
" See :help "'complete'" Default is:
" set complete=.,w,b,u,t,i
set completeopt=longest,menuone

" ================ Scrolling ========================
set scrolloff=8         " Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

" ================ Status line ======================
set laststatus=2                             "Always show status line
set statusline=
"set statusline+=%-3.3n\                      " buffer number
set statusline+=%f\                          " filename
set statusline+=%h%m%r%w                     " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}] " file type
set statusline+=%=                           " right align remainder
set statusline+=0x%-8B                       " character value
set statusline+=%-14(%l,%c%V%)               " line, column
set statusline+=%<%P                         " file position
set showcmd                                  " Display incomplete commands.
set showmode                                 " Show editing mode

" ================ Spell Checking ======================
set spell
set mousemodel=popup

" ================ CTags ======================
noremap <Leader>t :!ctags --extra=+f -R *<CR><CR>
set tags=./tags; " Load tags recursively from working directory

" ================ Keyboard customization ======================
" quick exit from insert mode
inoremap jk <esc>

" Paste from system clipboard instead of global register gymnastics
noremap <leader>v "+p
" And copy to system clipboard
noremap <leader>c "+y$

" Easy window navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" When you forget to open a file with sudo
cnoremap w!! w !sudo tee % >/dev/null

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

" Use Tab to trigger completion
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>

" Open the quickfix window automatically after any grep invocation:
autocmd QuickFixCmdPost *grep* cwindow

"Enable goimports to automatically insert import paths instead of gofmt:
let g:go_fmt_command = "goimports"

" Call command to create a scratch buffer
command! Scratch enew | setlocal nobuflisted buftype=nofile bufhidden=wipe noswapfile

" Presentation mode - white background w/o syntax highlighting and list chars
command! Present colorscheme default | set syntax=off nolist

lua require('plugins-config')

set shortmess+=I                " hide startup message
set number                      " enable line numbering
set undolevels=1000             " many levels of undo
set visualbell                  " no beeping
set noerrorbells                " I said no beeping!
set title                       " change terminal title
set autochdir                   " pwd is always same as current file
set nomodeline                  " turn them off
set lazyredraw                  " redraw only when we need to for faster macros
set nobackup
set noswapfile
set nowritebackup               " Prevents automatic write backup before overwriting file
set nofoldenable                " Don't fold by default
set spell                       " Write good
let g:netrw_liststyle=1         " Use list style in Netrw :E

" Automatically write out buffer and save location in file
autocmd FocusGained * silent! checktime     " force autoread to update buffer
augroup AutoSave
    autocmd!
    autocmd FocusLost * :wa " Autosave
    " Remember last location in file:
    autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
                \| execute "normal! g'\"" | endif
augroup END

" Keyboard customization
let mapleader=","               " change the mapleader from '\' to ','
set whichwrap+=<,>,h,l,[,]      " movement keys wrap at beginning/end of line

" Swap ; and :
nnoremap ; :
nnoremap : ;

" Use very magic search by default
nnoremap / /\v
vnoremap / /\v

" Clear highlighted search
nnoremap <silent> <leader>/ :nohlsearch<CR>

" Strip all trailing whitespace in the current buffer
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Copy to system clipboard
set clipboard+=unnamedplus
noremap <leader>c "+y

" Easy window navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Keep cursor in place when joining lines
nnoremap J mzJ`z

" Move by rows rather than lines
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" Do not capture newline with '$' in visual mode
vnoremap $ g_

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

" Visual clues
set showmatch
set termguicolors
lua require('themes-config')
lua require('gitsigns').setup()
lua require('treesitter-config')
lua require('nvim-autopairs').setup{}
silent! set guifont=Fira\ Mono:h15

" Highlight whitespace and lines past 100 columns
set list                " Setting list disables linebreak; never use with showbreak!
set virtualedit=block   " Select white space in Visual mode
set listchars=tab:▸\\u0020
call matchadd('ColorColumn', '\%101v', 100)
highlight ColorColumn ctermbg=131 guibg=#af5f5f
highlight ExtraWhitespace ctermbg=red guibg=#9d1f1f
match ExtraWhitespace /\s\+$/

" Search
set ignorecase          " Ignore case when searching
set smartcase           " Override ignorecase if search is all uppercase

" Indentation
" You will nearly always want tabstop == softtabstop == shiftwidth
set tabstop=4       " Number of visual spaces per TAB
set softtabstop=4   " Number of spaces inserted per TAB; higher precedence than tabstop
set shiftwidth=4    " Amount of white space to insert or remove in normal mode
set expandtab       " Turn TAB into spaces using value of softtabstop
set autoindent      " Always autoindent and copy previous indentation on copy
set shiftround      " Use multiple of shiftwidth when indenting with '<' and '>'
set copyindent      " Copy the existing lines indent when autoindenting a new line.

" Syntax of these languages can be fussy
augroup Filetypes
    autocmd!
    autocmd FileType c,cpp,perl,sh,zsh,mmix,asm setlocal ts=8 sts=8 sw=8 noexpandtab nowrap
    autocmd FileType python setlocal formatoptions+=ro " Insert comment on newline
    autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
    autocmd FileType html,css,javascript setlocal ts=2 sts=2 sw=2 expandtab nowrap
    autocmd BufReadPost,FileReadPost *.h setlocal filetype=c " C header files
    autocmd FileType text,rst setlocal formatoptions+=tcql textwidth=80
augroup END

" Autocompletion
set omnifunc=syntaxcomplete#Complete    " Turn on omnicompletion
set wildmode=list:longest
set wildignorecase                      " Ignore case on filename completion using :
set wildignore+=*.o,*.obj,*.git,*.rbc,*.swp,*.bak,*.pyc,*.class
set completeopt=longest,menuone

" Scrolling
set scrolloff=8
set sidescrolloff=15

" Status line
set statusline=
set statusline+=%f\                             " filename
set statusline+=%h%m%r%w                        " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}]    " file type
set statusline+=%14{get(b:,'gitsigns_head','')} " Current branch from gitsigns plugin
set statusline+=%=                              " right align remainder
set statusline+=0x%-8B                          " character value
set statusline+=%-14(%l,%c%V%)                  " line, column
set statusline+=%<%P                            " file position

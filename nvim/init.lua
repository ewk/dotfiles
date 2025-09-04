require('plugins-config')

vim.opt.shortmess:append({ I = true })  -- hide startup message
vim.opt.number = true                   -- enable line numbering
vim.opt.undolevels = 1000               -- many levels of undo
vim.opt.visualbell = true               -- no beeping
vim.opt.errorbells = false              -- I said no beeping!
vim.opt.title = true                    -- change terminal title
vim.opt.autochdir = true                -- pwd is always same as current file
vim.opt.modeline = false                -- turn them off
vim.opt.lazyredraw = true               -- redraw only when we need to for faster macros
vim.opt.writebackup = false             -- Prevents automatic write backup before overwriting file
vim.opt.foldenable = false              -- Don't fold by default
vim.opt.spell = true                    -- Write good
vim.opt.backup = false
vim.opt.swapfile = false

vim.cmd([[
" Automatically write out buffer and save location in file
autocmd FocusGained * silent! checktime     " force autoread to update buffer
augroup AutoSave
    autocmd!
    autocmd FocusLost * :wa " Autosave
    " Remember last location in file:
    autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
                \| execute "normal! g'\"" | endif
augroup END
]])

-- Keyboard customization
vim.g.mapleader = ","   -- change the mapleader from '\' to ','
-- Movement keys wrap at beginning/end of line
vim.opt.whichwrap:append { ['<'] = true, ['>'] = true, [','] = true, h = true, l = true, }

-- Swap ; and :
vim.keymap.set('n', ';', ':', { noremap = true })
vim.keymap.set('n', ':', ';', { noremap = true })

-- Use very magic search by default
vim.keymap.set({'n', 'v'}, '/', '/\\v', { noremap = true })

-- Clear highlighted search
vim.keymap.set('n', '<Leader>/', '<cmd>nohlsearch<cr>', { silent = true, noremap = true })

-- Strip all trailing whitespace in the current buffer
vim.keymap.set('n', '<Leader>W', ':%s/\\s\\+$//<cr>:let @/=\'\'<cr>', { noremap = true })

-- Copy to system clipboard
vim.opt.clipboard:append({ 'unnamedplus' })

-- Easy window navigation
vim.keymap.set('n', '<C-h>', '<C-w>h', { noremap = true })
vim.keymap.set('n', '<C-j>', '<C-w>j', { noremap = true })
vim.keymap.set('n', '<C-k>', '<C-w>k', { noremap = true })
vim.keymap.set('n', '<C-l>', '<C-w>l', { noremap = true })

-- Keep cursor in place when joining lines
vim.keymap.set('n', 'J', 'mzJ`z', { noremap = true })

-- Move by rows rather than lines
vim.keymap.set('n', 'j', 'gj', { noremap = true })
vim.keymap.set('n', 'k', 'gk', { noremap = true })
vim.keymap.set('n', 'gj', 'j', { noremap = true })
vim.keymap.set('n', 'gk', 'k', { noremap = true })

-- Do not capture newline with '$' in visual mode
vim.keymap.set('v', '$', 'g_', { noremap = true })

vim.cmd([[
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
]])

-- Visual clues
vim.opt.showmatch = true
vim.opt.termguicolors = true
require('themes-config')
require('gitsigns').setup()
require('treesitter-config')
require('nvim-autopairs').setup{}
vim.cmd([[ silent! set guifont=Fira\ Mono:h15 ]])

--Highlight whitespace and lines past 100 columns
vim.opt.list = true                 -- Setting list disables linebreak; never use with showbreak!
vim.opt.virtualedit = { block }     -- Select white space in Visual mode
vim.opt.listchars = { tab = '▸\\u0020' }
vim.cmd.highlight({ "ColorColumn", "ctermbg=131", "guibg=#af5f5f" })
vim.cmd.highlight({ "ExtraWhitespace", "ctermbg=red", "guibg=#9d1f1f" })
vim.fn.matchadd("ColorColumn", '\\%101v', '100')
vim.cmd.match({ "ExtraWhitespace", '/\\s\\+$/' })

-- Search
vim.opt.ignorecase = true         -- Ignore case when searching
vim.opt.smartcase = true          -- Override ignorecase if search is all uppercase

-- Indentation
-- You will nearly always want tabstop == softtabstop == shiftwidth
vim.opt.tabstop = 4         -- Number of visual spaces per TAB
vim.opt.softtabstop = 4     -- Number of spaces inserted per TAB; higher precedence than tabstop
vim.opt.shiftwidth = 4      -- Amount of white space to insert or remove in normal mode
vim.opt.expandtab = true    -- Turn TAB into spaces using value of softtabstop
vim.opt.autoindent = true   -- Always autoindent and copy previous indentation on copy
vim.opt.shiftround = true   -- Use multiple of shiftwidth when indenting with '<' and '>'
vim.opt.copyindent = true   -- Copy the existing lines indent when autoindenting a new line.

-- File syntax
local augroup = vim.api.nvim_create_augroup('Filetypes', { clear = true })

vim.api.nvim_create_autocmd({ "FileType" }, {
    pattern = {"*.c", "*.cpp", "*.sh", "*.mmix", "*.asm", "make"},
    group = 'Filetypes',
    desc = 'Syntax of these languages can be fussy',
    callback = function()
        vim.api.nvim_buf_set_option(0, "tabstop", 8)
        vim.api.nvim_buf_set_option(0, "softtabstop", 8)
        vim.api.nvim_buf_set_option(0, "shiftwidth", 8)
        vim.api.nvim_buf_set_option(0, "expandtab", false)
        vim.api.nvim_buf_set_option(0, "wrap", false)
    end
})

vim.api.nvim_create_autocmd({ "BufReadPost", "FileReadPost" }, {
    pattern = {"*.h"},
    group = 'Filetypes',
    desc = 'C header files',
    callback = function()
        vim.api.nvim_buf_set_option(0, "filetype", 'c')
    end
})

-- Autocompletion
vim.opt.omnifunc = 'syntaxcomplete#Complete'
vim.opt.wildmode = 'list:longest'
vim.opt.wildignorecase = true       -- Ignore case when completing file names and directories.
vim.opt.wildignore = { '*.o', '*.obj', '*.git', '*.rbc', '*.swp', '*.bak', '*.pyc', '*.class' }
vim.opt.completeopt = "longest,menuone"

-- Scrolling
vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 15

local statusline = {
    '%f',                                   -- filename
    '%h%m%r%w',                             -- status flags
    '%10{&filetype}',                       -- file type
    --'%14{get(b:,'gitsigns_head','')}',    -- current branch from gitsigns plugin
    '%=',                                   -- right align remainder
    '0x%-8B',                               -- character value
    '%-14(%l,%c%V%)',                       -- line, column
    '%<%P'                                  -- file position
}
vim.opt.statusline = table.concat(statusline, '')

local nvim_lsp = require'lspconfig'

-- Retain behavior of 'K' command outside LSP buffers
vim.api.nvim_set_keymap('n', 'K', ':lua show_documentation()<cr>', { noremap = true, silent = true })
function show_documentation()
    local filetype = vim.bo.filetype
    if vim.tbl_contains({ 'vim','help' }, filetype) then
        vim.cmd('h '..vim.fn.expand('<cword>'))
    elseif vim.tbl_contains({ 'man' }, filetype) then
        vim.cmd('Man '..vim.fn.expand('<cword>'))
    else
        vim.lsp.buf.hover()
    end
end

-- Custom key mappings for LSP functions
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts) -- needs cmp?
end

-- Configure rust_analyzer
nvim_lsp.rust_analyzer.setup({
    on_attach = on_attach,
    --capabilities=capabilities,
    settings = {
        ["rust-analyzer"] = {
            checkOnSave = {
                command = "clippy", -- default: check
                enable = true
            },
        }
    }
})

-- Enable diagnostic inlays but disable interruption in insert mode
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
        signs = true,
        underline = true,
        update_in_insert = false,
        virtual_text = true,
})

local nvim_lsp = require'lspconfig'

-- Configure rust_analyzer
nvim_lsp.rust_analyzer.setup({
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

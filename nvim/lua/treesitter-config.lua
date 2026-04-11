require("tree-sitter-manager").setup({
    -- Treesitter highlighting is enabled by default.
    -- No auto-updates or auto-install.
    ensure_installed = { "comment", "diff" },
    -- Optional: custom paths
    -- parser_dir = vim.fn.stdpath("data") .. "/site/parser",
    -- query_dir = vim.fn.stdpath("data") .. "/site/queries",
})

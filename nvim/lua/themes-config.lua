require("modus-themes").setup({
    -- Theme comes in two styles `modus_operandi` and `modus_vivendi`
    -- `auto` will automatically set style based on background set with vim.o.background
    style = "auto",
    variant = "default", -- Theme comes in four variants `default`, `tinted`, `deuteranopia`, and `tritanopia`
    transparent = false, -- Transparent background (as supported by the terminal)
    dim_inactive = false, -- "non-current" windows are dimmed
    hide_inactive_statusline = false, -- Hide statuslines on inactive windows.
    styles = {
        -- Style to be applied to different syntax groups
        -- Value is any valid attr-list value for `:help nvim_set_hl`
        comments = { italic = false, bold = false },
        keywords = { italic = false, bold = false },
        functions = { italic = false, bold = false },
        variables = { italic = false, bold = false },
    },

    --- You can override specific highlights to use other groups or a hex color
    --- Function will be called with a Highlights and ColorScheme table
    --- Refer to `~/.local/share/nvim/lazy/modus-themes.nvim/extras/lua/modus_*.lua` for the Highlights and ColorScheme table
    ---@param colors ColorScheme
    on_colors = function(colors) end,

    ---@param highlights Highlights
    ---@param colors ColorScheme
    on_highlights = function(highlights, colors)
        highlights.Boolean = { bold = false, fg = "#c0caf5" }
        highlights.Title = { bold = false, fg = "#c6daff" }
    end,
})

vim.cmd("colorscheme modus")

require("tokyonight").setup({
    style = "night", -- Three styles: `storm`, `moon`, a darker variant `night` and `day`
    light_style = "day",
    transparent = false, -- Enable this to disable setting the background color
    terminal_colors = true,
    styles = {
        -- Style to be applied to different syntax groups
        -- Value is any valid attr-list value for `:help nvim_set_hl`
        comments = { italic = false, bold = false },
        keywords = { italic = false, bold = false },
        functions = { italic = false, bold = false},
        variables = { italic = false, bold = false},
        -- Background styles. Can be "dark", "transparent" or "normal"
        sidebars = "dark", -- style for sidebars, see below
        floats = "dark", -- style for floating windows
    },
    day_brightness = 0.3, -- Adjusts the brightness of the colors of the **Day** style.
    dim_inactive = false, -- dims inactive windows
    lualine_bold = false, -- When `true`, section headers in the lualine theme will be bold

    --- You can override specific color groups to use other groups or a hex color
    --- function will be called with a ColorScheme table
    --- Refer to `extras/lua/tokyonight_*.lua`
    ---@param colors ColorScheme
    on_colors = function(colors) end,

    --- You can override specific highlights to use other groups or a hex color
    --- function will be called with a Highlights and ColorScheme table
    ---@param highlights Highlights
    ---@param colors ColorScheme
    on_highlights = function(highlights, colors)
        highlights.Bold = { bold = false, fg = "#c0caf5" }
        highlights.Title = { bold = false, fg = "#7aa2f7" }
    end,
})

vim.cmd("colorscheme tokyonight")

return {
    -- Kanagawa Theme
    {
        "rebelot/kanagawa.nvim",
        opts = {
            commentStyle = { italic = true },
            functionStyle = {},
            keywordStyle = { italic = true },
            statementStyle = { bold = true },
            typeStyle = {},
            transparent = true,
            dimInactive = false,
            colors = {
                palette = {},
                theme = { wave = {}, lotus = {}, dragon = {}, all = {} },
            },
            overrides = function(colors)
                return {}
            end,
            theme = "wave",
        },
        config = function(_, opts)
            require("kanagawa").setup(opts)
            -- Remove vim.cmd.colorscheme() here
        end,
    },

    -- TokyoDark Theme
    {
        "tiagovla/tokyodark.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            require("tokyodark").setup({
                transparent_background = false,
                gamma = 1.0,
                styles = {
                    comments = { italic = true, bold = true },
                    keywords = { bold = true },
                    identifiers = { italic = true },
                    functions = { bold = true },
                    variables = { bold = true },
                },
                terminal_colors = true,
            })
            -- Remove vim.cmd.colorscheme() here
        end,
    },

    -- Ofirkai Theme
    {
        "ofirgall/ofirkai.nvim",
        lazy = false,
        priority = 1001, -- Higher priority than TokyoDark
        config = function()
            -- Initialize but don't set colorscheme here
        end,
    },

    -- Ofirkai Theme
    {
        "ellisonleao/gruvbox.nvim",
        lazy = false,
        config = function()
            -- Initialize but don't set colorscheme here
        end,
    },
    -- LazyVim config (MUST COME LAST)
    {
        "LazyVim/LazyVim",
        opts = {
            colorscheme = "tokyodark",
        },
        priority = 10000, -- Highest priority to ensure it loads last
    },
}

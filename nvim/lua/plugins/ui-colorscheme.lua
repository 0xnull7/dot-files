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

    -- Gruvbox Theme
    {
        "ellisonleao/gruvbox.nvim",
        lazy = false,
        config = function()
            -- Initialize but don't set colorscheme here
        end,
    },

    -- Yorumi Theme
    {
        "yorumicolors/yorumi.nvim",
        lazy = false,
        config = function()
            -- Initialize but don't set colorscheme here
        end,
    },

    -- OneDark Theme
    {
        "navarasu/onedark.nvim",
        lazy = false,
        priority = 1000, -- make sure to load this before all the other start plugins
        config = function()
            require("onedark").setup({
                style = "darker", -- Options: dark, darker, cool, deep, warm, warmer, light
                transparent = false, -- Show/hide background
                term_colors = true, -- Change terminal color as per the selected theme style
                ending_tildes = false, -- Show the end-of-buffer tildes. By default they are hidden
                cmp_itemkind_reverse = false, -- reverse item kind highlights in cmp menu

                -- Change code style ---
                -- Options are italic, bold, underline, none
                -- You can configure multiple style with comma separated, For e.g., keywords = 'italic,bold'
                code_style = {
                    comments = "italic,bold",
                    keywords = "bold",
                    functions = "bold",
                    strings = "italic,bold",
                    variables = "bold",
                },

                -- Lualine options --
                lualine = {
                    transparent = false, -- lualine center bar transparency
                },

                -- Plugins Config --
                diagnostics = {
                    darker = true, -- darker colors for diagnostic
                    undercurl = true, -- use undercurl instead of underline for diagnostics
                    background = true, -- use background color for virtual text
                },
            })
            -- Enable theme
            -- require("onedark").load()
        end,
    },

    -- OneDarkPro Theme (Check the repo for more customizations)
    {
        "olimorris/onedarkpro.nvim",
        lazy = false,
        priority = 1000, -- Ensure it loads first
    },

    -- LazyVim config (MUST COME LAST)
    {
        "LazyVim/LazyVim",
        priority = 10000, -- Highest priority to ensure it loads last
        opts = {
            colorscheme = "tokyodark",
        },
    },
}

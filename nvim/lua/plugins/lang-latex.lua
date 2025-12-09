return {
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                texlab = {
                    settings = {
                        texlab = {
                            build = {
                                executable = "latexmk",
                                args = {
                                    "-pdflua",
                                    "-lualatex",
                                    "-interaction=nonstopmode",
                                    "-synctex=1",
                                    "-halt-on-error",
                                    "-output-directory=build",
                                    "-e",
                                    "\"$lualatex = 'lualatex %O -shell-escape %S'\"",
                                    "%f",
                                },
                                forwardSearchCache = "build",
                                onSave = false,
                            },

                            root = {
                                detectors = { "root", "command", "args" },
                                file = "main.tex",
                            },

                            auxDirectory = "build",
                            diagnosticsDelay = 300,
                            formatterLineLength = 120,
                            latexFormatter = "none",

                            chktex = {
                                onEdit = false,
                                onOpenAndSave = false,
                            },
                        },
                    },
                },
            },
        },
    },
    -- Formatter Configs
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                -- latex
                tex = { "latexindent" },
                latex = { "latexindent" },
            },

            formatters = {
                -- latex formatter with your existing config
                latexindent = {
                    command = "latexindent",
                    args = {
                        "-m",
                        "-l",
                        vim.fn.expand("~/.config/latexindent/indentconfig.yaml"),
                        "-",
                    },
                    stdin = true,
                    timeout_ms = 8000,
                },

                -- latex formatter with your existing config
                -- texfmt = {
                --     command = "tex-fmt",
                --     args = {
                --         "--config",
                --         vim.fn.expand("~/.config/tex-fmt/tex-fmt.toml"),
                --         "-",
                --     },
                --     stdin = true,
                -- },
            },
        },
    },
    {
        "lervag/vimtex",
        ft = "tex",
        init = function()
            -- Enable vimtex's syntax concealment feature
            vim.g.vimtex_syntax_conceal_disable = 0 -- 0 (default) to enable, 1 to disable

            -- Crucial for vimtex concealment to work:
            vim.opt.conceallevel = 2

            -- How concealment behaves under the cursor:
            -- "" or 0: No concealment under the cursor (recommended for editing)
            -- "nvic": Conceal in Normal, Visual, Insert, Command mode (conceals everything)
            vim.opt.concealcursor = ""
        end,
        -- You can add other vimtex options here if needed,
        -- e.g., for specific conceal settings controlled by g:vimtex_syntax_conceal
        -- config = function()
        --   vim.g.vimtex_syntax_conceal = 'abx' -- Example: Conceal all (default)
        -- end
    },
}

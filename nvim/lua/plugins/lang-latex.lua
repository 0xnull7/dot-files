return {
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                texlab = {
                    settings = {
                        texlab = {
                            -- build = {
                            --     executable = "latexmk",
                            --     args = {
                            --         "-pdflua",
                            --         "-lualatex",
                            --         "-interaction=nonstopmode",
                            --         "-synctex=1",
                            --         "-halt-on-error",
                            --         "-output-directory=build",
                            --         "-e",
                            --         "$lualatex=lualatex %O -shell-escape %S",
                            --         "%f",
                            --     },
                            --     forwardSearchCache = "build",
                            --     onSave = false,
                            -- },

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
                tex = { "latexindent" },
                latex = { "latexindent" },
            },

            formatters = {
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
        lazy = false,
        keys = {
            { "<leader>vv", "<cmd>VimtexCompile<cr>", desc = "VimTeX: Toggle Compile" },
            { "<leader>vk", "<cmd>VimtexStop<cr>", desc = "VimTeX: Stop Compilation" },
            { "<leader>vp", "<cmd>VimtexView<cr>", desc = "VimTeX: Preview PDF" },
            { "<leader>vt", "<cmd>VimtexTocOpen<cr>", desc = "VimTeX: Table of Contents" },
            { "<leader>vc", "<cmd>VimtexClean<cr>", desc = "VimTeX: Clean Aux Files" },
            { "<leader>ve", "<cmd>VimtexErrors<cr>", desc = "VimTeX: Show Error Log" },
            { "<leader>vi", "<cmd>VimtexInfo<cr>", desc = "VimTeX: Project Info" },
        },
        init = function()
            vim.g.vimtex_view_method = "zathura"

            -- METHOD 1: Simple approach - just set the engine
            vim.g.vimtex_compiler_method = "latexmk"
            vim.g.vimtex_compiler_latexmk = {
                engine = "-lua",
                executable = "latexmk",
                callback = 1,
                continuous = 1,
                options = {
                    "-shell-escape",
                    "-verbose",
                    "-file-line-error",
                    "-synctex=1",
                    "-interaction=nonstopmode",
                },
                out_dir = "build",
            }

            vim.g.vimtex_compiler_clean_on_keypress = 1

            -- Automatically close viewer when quitting Neovim
            vim.api.nvim_create_autocmd("VimLeave", {
                group = vim.api.nvim_create_augroup("VimtexCleanup", { clear = true }),
                pattern = "*.tex",
                callback = function()
                    vim.cmd("VimtexClean")
                    vim.cmd("VimtexStop")
                end,
            })

            vim.g.vimtex_quickfix_mode = 0
            vim.g.vimtex_syntax_conceal_disable = 0
            vim.opt.conceallevel = 2
            vim.opt.concealcursor = ""
        end,
    },
}

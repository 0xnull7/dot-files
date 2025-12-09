return {
    -- === 1. Configure the Language Servers (marksman & markdownlint) ===
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                marksman = {
                    filetypes = { "markdown", "quarto" },
                },
                markdownlint = {
                    settings = {
                        config = {
                            default = true,
                            disable = {
                                "MD013", -- Line length (Too verbose. Set soft-wrap instead.)
                                "MD007", -- Unordered list indentation (Allows more flexibility in nesting.)
                                "MD024", -- Multiple headings with the same content
                                "MD025", -- Multiple top-level headings (H1)
                                "MD041", -- First line in file should be a top-level heading
                                "MD046", -- Code block style (fenced vs indented)
                                "MD033", -- Inline HTML (Needed for MDX/custom elements)
                            },

                            ["MD046"] = { style = "fenced" }, -- Force '```' style over indented code blocks.
                            ["MD003"] = { style = "atx" }, -- Force ATX style headings ('# H1') over Setext ('H1\n===')
                            ["MD009"] = { br_spaces = 2 }, -- Lines should end with 2 spaces for hard line break
                        },
                    },
                },
            },
        },
    },

    -- === 2. Configure the Formatter (prettier) ===
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                markdown = { "markdownlint-cli2" },
            },
            -- format_on_save = {
            --     timeout_ms = 500,
            --     lsp_fallback = true,
            -- },
        },
    },

    -- === 3. Configure the Previewer (iamcco/markdown-preview.nvim) ===
    {
        "iamcco/markdown-preview.nvim",
        ft = "markdown",
        cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
        keys = {
            {
                "<leader>mp",
                ft = "markdown",
                "<cmd>MarkdownPreviewToggle<cr>",
                desc = "Toggle Markdown Preview",
            },
            {
                "<leader>mx",
                ft = "markdown",
                "<cmd>MarkdownPreviewStop<cr>",
                desc = "Stop Markdown Preview",
            },
        },
        config = function()
            vim.g.mkdp_auto_start = 0
        end,
    },
}

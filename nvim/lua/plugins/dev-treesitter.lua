return {
    {
        "nvim-treesitter/nvim-treesitter",
        opts = function(_, opts)
            if type(opts.ensure_installed) == "table" then
                -- List of all required language parsers
                vim.list_extend(opts.ensure_installed, {
                    -- "angular",
                    -- "assembly",
                    "bash",
                    "c",
                    "c_sharp",
                    "cmake",
                    "cpp",
                    "css",
                    -- "d",
                    -- "elixir",
                    -- "elm",
                    -- "erlang",
                    -- "fortran",
                    -- "gdscript",
                    "go",
                    -- "haskell",
                    "html",
                    "java",
                    "javascript",
                    "json",
                    "lua",
                    "make",
                    "markdown",
                    "markdown_inline",
                    "powershell",
                    "python",
                    "query", -- for Treesitter queries themselves
                    "racket",
                    "regex",
                    "rust",
                    "sql",
                    "toml",
                    "tsx",
                    "typescript",
                })
            end
        end,
    },

    -- Treesitter Context (shows the scope of the current line)
    {
        "nvim-treesitter/nvim-treesitter-context",
        dependencies = "nvim-treesitter/nvim-treesitter",
        opts = {
            enable = true,
            max_lines = 3, -- Show up to 3 lines of context
            throttle_ms = 100,
            patterns = {
                default = {
                    "class",
                    "function",
                    "method",
                    "for",
                    "while",
                    "if",
                    "switch",
                    "case",
                },
            },
        },
    },
}

return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                toml = { "taplo" },
            },

            formatters = {
                -- TOML formatter with consistent style
                taplo = {
                    command = "taplo",
                    args = {
                        "format",
                        "--indent-string",
                        "  ", -- 2 spaces
                        "--align-entries",
                        "--align-comments",
                        "--reorder-arrays",
                        "-",
                    },
                    stdin = true,
                },
            },
        },
    },
}

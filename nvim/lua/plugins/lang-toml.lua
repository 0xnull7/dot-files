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
                        "-o",
                        "indent_string=  ",
                        "-o",
                        "align_entries=true",
                        "-o",
                        "align_comments=true",
                        "-o",
                        "reorder_arrays=true",
                        "-o",
                        "indent_entries=true",
                        "-o",
                        "array_trailing_comma=true",
                        "-",
                    },
                    stdin = true,
                },
            },
        },
    },
}

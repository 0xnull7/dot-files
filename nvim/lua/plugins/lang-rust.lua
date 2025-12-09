return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                -- Rust
                rs = { "rustfmt" },
            },

            formatters = {
                -- Rust formatter with edition and style preferences
                rustfmt = {
                    command = "rustfmt",
                    args = {
                        "--edition",
                        "2021",
                        "--config",
                        "hard_tabs=false,tab_spaces=4,max_width=100,reorder_imports=true",
                    },
                    stdin = true,
                },
            },
        },
    },
}

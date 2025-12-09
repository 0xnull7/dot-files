return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                -- Database
                sql = { "sqlfluff" },
            },

            formatters = {
                -- SQL formatter with consistent style
                sqlfluff = {
                    command = "sqlfluff",
                    args = {
                        "format",
                        "--dialect",
                        "sqlite", -- Change to your preferred SQL dialect (mysql, snowflake, bigquery, etc.)
                        -- "--config",
                        -- vim.fn.expand("~/.config/sqlfluff/.sqlfluff"), -- Optional: path to your SQLFluff config
                        "-",
                    },
                    stdin = true,
                },
            },
        },
    },
}

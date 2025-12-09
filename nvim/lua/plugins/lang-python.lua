return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                python = { "ruff_format" },
            },

            formatters = {
                -- Ruff Python formatter
                ruff_format = {
                    command = "ruff",
                    args = {
                        "format",
                        "--line-length=100",
                        "--target-version=py313",
                        "--preview",
                        "-",
                    },
                    stdin = true,
                },
            },
        },
    },
}

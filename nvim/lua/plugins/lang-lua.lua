return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                lua = { "stylua" },
            },

            formatters = {
                -- Lua formatter with robust configuration
                stylua = {
                    command = "stylua",
                    args = {
                        "--search-parent-directories",
                        "--column-width=100",
                        "--indent-type=Spaces",
                        "--indent-width=4",
                        "--quote-style=AutoPreferDouble",
                        "--call-parentheses=Always",
                        "-",
                    },
                    stdin = true,
                },
            },
        },
    },
}

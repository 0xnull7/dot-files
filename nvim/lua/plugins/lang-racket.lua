return {

    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                racket_langserver = {},
            },
        },
    },

    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                racket = { "raco_fmt" },
            },

            formatters = {
                raco_fmt = {
                    command = "raco",
                    args = {
                        "fmt",
                        "--width",
                        "100",
                        "--indent",
                        "2",
                    },
                },
            },
        },
    },

    -- To enhance the REPL [Better Evaluation]
    {
        "Olical/conjure",
        ft = { "racket", "scheme" },
        lazy = true,
        init = function() end,
    },
}

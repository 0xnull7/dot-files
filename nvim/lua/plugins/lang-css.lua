return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                css = { "prettier" },
            },

            formatters = {
                -- Enhanced Prettier configuration for all web technologies
                prettier = {
                    prepend_args = {
                        "--print-width=100",
                        "--tab-width=2",
                        "--use-tabs=false",
                        "--semi=true", -- Add semicolons for professional JS/TS
                        "--single-quote=false",
                        "--jsx-single-quote=false",
                        "--trailing-comma=es5", -- More compatible than 'all'
                        "--arrow-parens=always", -- More explicit than 'avoid'
                        "--bracket-spacing=true",
                        "--jsx-bracket-same-line=false",
                        "--prose-wrap=always",
                        "--end-of-line=lf",
                        "--quote-props=as-needed",
                        "--html-whitespace-sensitivity=css",
                        "--vue-indent-script-and-style=false",
                        "--embedded-language-formatting=auto",
                    },
                },
            },
        },
    },
}

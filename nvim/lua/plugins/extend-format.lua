-- You can modify OR disable any plugin/option regarding formatters.

return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                cpp = { "clang_format" },
                c = { "clang_format" },
                objc = { "clang_format" },
                tex = { "latexindent" },
                latex = { "latexindent" },
            },
            formatters = {
                clang_format = {
                    command = "clang-format",
                    args = {
                        "-style={BasedOnStyle: Google, IndentWidth: 4, ColumnLimit: 0}",
                        "-assume-filename",
                        "{filename}",
                    },
                },
                latexindent = {
                    command = "latexindent",
                    args = {
                        "-m",
                        "-l",
                        vim.fn.expand("~/.config/latexindent/indentconfig.yaml"),
                        "-",
                    },
                    stdin = true,
                },
                prettier = {
                    prepend_args = {
                        "--print-width=80",
                        "--tab-width=2",
                        "--use-tabs=false",
                        "--semi=false",
                        "--single-quote=false",
                        "--jsx-single-quote=false",
                        "--trailing-comma=all",
                        "--arrow-parens=avoid",
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

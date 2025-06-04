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
            },
        },
    },
}

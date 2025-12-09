return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                -- Build systems
                cmake = { "cmake_format" },
            },

            formatters = {
                cmake_format = {
                    command = "cmake-format",
                    args = {
                        "--line-width=100",
                        "--tab-size=4",
                        "--use-tabs=false",
                        "--max-subargs-per-line=3",
                        "--separate-ctrl-name-with-space=true",
                        "--separate-fn-name-with-space=true",
                        "--dangle-parens=true",
                        "-",
                    },
                    stdin = true,
                },
            },
        },
    },
}

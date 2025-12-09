return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                -- C#
                cs = { "csharpier" },
            },

            formatters = {
                -- C# formatter with modern C# conventions
                csharpier = {
                    command = "dotnet-csharpier",
                    args = {
                        "--print-width=100",
                        "--end-of-line=LF",
                        "--stdin",
                    },
                    stdin = true,
                },
            },
        },
    },
}

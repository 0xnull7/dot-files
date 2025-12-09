return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                -- Java
                java = { "google_java_format", "clang_format" },
            },

            formatters = {
                -- Java formatter with enhanced professional configuration
                clang_format = {
                    command = "clang-format",
                    args = {
                        "-style={BasedOnStyle: Google, IndentWidth: 4, ColumnLimit: 100, UseTab: Never, AccessModifierOffset: -4, AllowShortIfStatementsOnASingleLine: false, AllowShortFunctionsOnASingleLine: false, PointerAlignment: Right, SortIncludes: CaseSensitive, Standard: c++23, NamespaceIndentation: All, AlignConsecutiveAssignments: true, AlignConsecutiveDeclarations: true, ConstructorInitializerAllOnOneLineOrOnePerLine: true, BinPackParameters: false}",
                        "-assume-filename",
                        "$FILENAME",
                        -- , BreakBeforeBraces: Allman (if you want it)
                    },
                },

                -- Alternative: Use a separate .clang-format config file (recommended)
                -- clang_format_file = {
                --   command = "clang-format",
                --   args = {
                --     "-style=file",  -- This will use .clang-format from your project root
                --     "-assume-filename",
                --     "$FILENAME",
                --   },
                -- },

                -- Java-specific formatter
                google_java_format = {
                    command = "google-java-format",
                    args = {
                        "--aosp",
                        "--skip-sorting-imports",
                        "-",
                    },
                    stdin = true,
                },
            },
        },
    },
}

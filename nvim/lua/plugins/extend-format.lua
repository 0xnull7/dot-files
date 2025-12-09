return {
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                -- C/C++ family
                c = { "clang_format" },
                cpp = { "clang_format" },
                objc = { "clang_format" },

                -- Java/Kotlin
                java = { "clang_format" },
                kt = { "ktlint" },

                -- Scripting languages
                python = { "ruff_format" },
                lua = { "stylua" },

                -- Go
                go = { "gofmt", "goimports" },

                -- Rust
                rs = { "rustfmt" },

                -- Web technologies
                html = { "prettier" },
                css = { "prettier" },
                javascript = { "prettier" },
                javascriptreact = { "prettier" },
                typescript = { "prettier" },
                typescriptreact = { "prettier" },
                vue = { "prettier" },
                json = { "prettier" },

                -- Shell/Bash
                sh = { "shfmt" },
                bash = { "shfmt" },
                zsh = { "shfmt" },

                -- Build systems
                cmake = { "cmake_format" },

                -- Documentation
                markdown = { "prettier" },
                md = { "prettier" },

                -- C#
<<<<<<< HEAD
                -- cs = { "csharpier" },
=======
                cs = { "csharpier" },
>>>>>>> ea41cc011c5e8ac9b01a3773f0f660a3a4f91452

                -- Database
                sql = { "sql_formatter" },

                -- LaTeX
                tex = { "latexindent" },
                latex = { "latexindent" },

                -- Config files
                yaml = { "prettier" },
                yml = { "prettier" },
                toml = { "taplo" },
            },

            formatters = {
                -- C/C++/Java formatter with enhanced professional configuration
                clang_format = {
                    command = "clang-format",
                    args = {
                        "-style={BasedOnStyle: Google, IndentWidth: 4, ColumnLimit: 100, UseTab: Never, AccessModifierOffset: -4, AllowShortIfStatementsOnASingleLine: false, AllowShortFunctionsOnASingleLine: false, PointerAlignment: Right, SortIncludes: CaseSensitive, Standard: c++20, NamespaceIndentation: All, AlignConsecutiveAssignments: true, AlignConsecutiveDeclarations: true, ConstructorInitializerAllOnOneLineOrOnePerLine: true, BinPackParameters: false}",
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

                -- Kotlin formatter with official Android style
                ktlint = {
                    command = "ktlint",
                    args = {
                        "--android",
                        "--stdin",
                        "--format",
                        "-",
                    },
                    stdin = true,
                },

                -- Ruff Python formatter
                -- ruff_format = {
                --     command = "ruff",
                --     args = {
                --         "format",
                --         "--line-length=100",
                --         "--target-version=py311",
                --         "--preview",
                --         "-",
                --     },
                --     stdin = true,
                -- },

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

                -- Go formatters (gofmt is opinionated, goimports for imports)
                gofmt = {
                    command = "gofmt",
                    args = { "-s" }, -- simplify code
                    stdin = true,
                },
                goimports = {
                    command = "goimports",
                    args = {
                        "-local",
                        "github.com/your-org", -- Replace with your actual module path
                    },
                    stdin = true,
                },

                -- Rust formatter with edition and style preferences
                rustfmt = {
                    command = "rustfmt",
                    args = {
                        "--edition",
                        "2021",
                        "--config",
                        "hard_tabs=false,tab_spaces=4,max_width=100,reorder_imports=true",
                    },
                    stdin = true,
                },

                -- Shell formatter with POSIX compliance and safety
                shfmt = {
                    command = "shfmt",
                    args = {
                        "-i",
                        "2", -- 2 spaces indentation
                        "-bn", -- binary operators like && and | may start a line
                        "-ci", -- switch cases will be indented
                        "-sr", -- redirect operators will be followed by a space
                        "-ln",
                        "posix", -- POSIX compliant
                        "-w",
                        "0", -- don't write back, use stdin/stdout
                    },
                    stdin = true,
                },

                -- CMake formatter with modern style
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

                -- C# formatter with modern C# conventions
<<<<<<< HEAD
                -- csharpier = {
                --     command = "dotnet-csharpier",
                --     args = {
                --         "--print-width=100",
                --         "--end-of-line=LF",
                --         "--stdin",
                --     },
                --     stdin = true,
                -- },
=======
                csharpier = {
                    command = "dotnet-csharpier",
                    args = {
                        "--print-width=100",
                        "--end-of-line=LF",
                        "--stdin",
                    },
                    stdin = true,
                },
>>>>>>> ea41cc011c5e8ac9b01a3773f0f660a3a4f91452

                -- SQL formatter with consistent style
                sql_formatter = {
                    command = "sql-formatter",
                    args = {
                        "--language",
                        "sql",
                        "--indent",
                        "2",
                        "--lines-between-queries",
                        "2",
                        "--keyword-case",
                        "upper",
                        "--function-case",
                        "lower",
                        "--identifier-case",
                        "lower",
                    },
                    stdin = true,
                },

                -- LaTeX formatter with your existing config
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

                -- TOML formatter with consistent style
                taplo = {
                    command = "taplo",
                    args = {
                        "format",
                        "--indent-string",
                        "  ", -- 2 spaces
                        "--align-entries",
                        "--align-comments",
                        "--reorder-arrays",
                        "-",
                    },
                    stdin = true,
                },

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

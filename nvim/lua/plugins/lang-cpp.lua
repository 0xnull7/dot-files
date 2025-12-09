return {
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                clangd = {
                    keys = {
                        {
                            "<leader>ch",
                            "<cmd>ClangdSwitchSourceHeader<cr>",
                            desc = "Switch Source/Header (C/C++)",
                        },
                    },

                    -- root_dir = function(fname)
                    --     if type(fname) == "number" then
                    --         fname = vim.api.nvim_buf_get_name(fname)
                    --     end
                    --     if fname == nil or fname == "" then
                    --         return nil
                    --     end
                    --
                    --     local lspconfig_util = require("lspconfig.util")
                    --     return lspconfig_util.root_pattern(
                    --         "Makefile",
                    --         "configure.ac",
                    --         "configure.in",
                    --         "config.h.in",
                    --         "meson.build",
                    --         "meson_options.txt",
                    --         "build.ninja",
                    --         "CMakeLists.txt",
                    --         ".git"
                    --     )(fname) or lspconfig_util.root_pattern(
                    --         "compile_commands.json",
                    --         "compile_flags.txt"
                    --     )(fname) or lspconfig_util.find_git_ancestor(fname)
                    -- end,

                    setup = {
                        clangd = function(_, opts)
                            local clangd_ext_opts = LazyVim.opts("clangd_extensions.nvim")

                            require("clangd_extensions").setup(
                                vim.tbl_deep_extend(
                                    "force",
                                    clangd_ext_opts or {},
                                    { server = opts }
                                )
                            )
                            return false
                        end,
                    },

                    capabilities = {
                        offsetEncoding = { "utf-16" },
                    },

                    cmd = {
                        "clangd",
                        "--background-index", -- Enable background indexing for faster initial load.
                        "--clang-tidy", -- Enable clang-tidy diagnostics.
                        "--header-insertion=iwyu", -- Use 'include-what-you-use' style for header suggestions.
                        "--completion-style=detailed", -- Provide more detailed completion suggestions.
                        "--function-arg-placeholders", -- Insert placeholders for function arguments in completions.
                        "--pch-storage=disk", -- Store precompiled headers on disk for better performance.
                        "--cross-file-rename", -- Enable renaming symbols across multiple files.
                        "--fallback-style=llvm", -- Use LLVM coding style as a fallback for formatting.
                        -- "--malloc-trim",      -- (Optional) Reduces memory usage. May not be available on all systems.
                        -- "--log=verbose",      -- (Optional) Enable verbose logging for debugging.
                        -- "--pretty-print",     -- (Optional) Pretty-print JSON messages for debugging.
                    },

                    -- Initialisation options sent to the language server upon connection.
                    init_options = {
                        usePlaceholders = true, -- Enable snippet-style placeholders in completions.
                        completeUnimported = true, -- Allow completion for unimported symbols.
                        clangdFileStatus = true, -- Enable clangd file status updates (useful for statusline).
                        fallbackFlags = {
                            "-std=c++23",
                            "-xc++", -- Treat files as C++ (important for .h files).
                            "-Wall",
                            "-Wextra",
                            "-Weffc++",
                            "-Wconversion",
                            "-Wsign-conversion",
                            "-pedantic-errors",
                            "-IC:/MinGW/include",
                            "-IC:/MinGW/lib/gcc/x86_64-w64-mingw32/15.1.0/include/",
                            "-IC:/MinGW/x86_64-w64-mingw32/include",
                            "-IC:/Clangd/lib/clang/20/include",
                        },
                        -- resourceDir = "/path/to/clangd/resources", -- (Optional) Specify if clangd resources are in a custom location.
                        -- compilationDatabasePath = "build", -- (Optional) Specify if compile_commands.json is in a specific subdirectory.
                    },
                },
            },
        },
    },
    {
        "p00f/clangd_extensions.nvim",
        lazy = true,
        opts = {
            inlay_hints = {
                inline = false,
                -- Other inlay hint options can be added here, e.g.:
                -- enabled = true,
                -- highlight = "Comment", -- Highlight group for inlay hints
                -- priority = 100,
                -- only_current_line = false,
                -- show_parameter_hints = true,
                -- show_type_hints = true,
                -- show_auto_type_hints = true,
                -- show_deduced_type_hints = true,
                -- show_implicit_casts = true,
                -- show_implicit_conversions = true,
                -- show_template_argument_hints = true,
            },
            ast = {
                role_icons = {
                    type = "",
                    declaration = "",
                    expression = "",
                    specifier = "",
                    statement = "",
                    ["template argument"] = "",
                },
                kind_icons = {
                    Compound = "",
                    Recovery = "",
                    TranslationUnit = "",
                    PackExpansion = "",
                    TemplateTypeParm = "",
                    TemplateTemplateParm = "",
                    TemplateParamObject = "",
                },
            },
        },
    },
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                cpp = { "clang_format" },
            },

            formatters = {
                -- C++ formatter with enhanced professional configuration
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
            },
        },
    },
}

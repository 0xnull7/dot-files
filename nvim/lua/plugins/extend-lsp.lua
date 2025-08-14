-- This file extends the nvim-lspconfig plugin.

return {
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                clangd = {
                    keys = {
                        { "<leader>ch", "<cmd>ClangdSwitchSourceHeader<cr>", desc = "Switch Source/Header (C/C++)" },
                    },

                    root_dir = function(fname)
                        local lspconfig_util = require("lspconfig.util")
                        return lspconfig_util.root_pattern(
                            "Makefile",
                            "configure.ac",
                            "configure.in",
                            "config.h.in",
                            "meson.build",
                            "meson_options.txt",
                            "build.ninja",
                            "CMakeLists.txt",
                            ".git"
                        )(fname) or lspconfig_util.root_pattern(
                            "compile_commands.json",
                            "compile_flags.txt"
                        )(fname) or lspconfig_util.find_git_ancestor(fname)
                    end,

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
                        "--limit-results=100", -- Limit the number of results for completions/diagnostics to 100.
                        "--fallback-style=llvm", -- Use LLVM coding style as a fallback for formatting.
                        -- "--std=c++23", -- Set C++23 as the default standard for clangd.
                        -- "--style=Google", -- Specify the formatting style. Common styles: LLVM, Google, Chromium, Mozilla, WebKit.
                        -- "-Wall", -- Enable all common warnings.
                        -- "-Wextra", -- Enable extra warnings not covered by -Wall.
                        -- "-Weffc++", -- Enable warnings for effective C++ programming (e.g., rule of three/five).
                        -- "-Wconversion", -- Warn for implicit conversions that may change value.
                        -- "-Wsign-conversion", -- Warn for implicit conversions between signed and unsigned types.
                        -- "-pedantic-errors", -- Issue errors for non-standard C++ constructs.
                        -- "--malloc-trim",      -- (Optional) Reduces memory usage. May not be available on all systems.
                        -- "--log=verbose",      -- (Optional) Enable verbose logging for debugging.
                        -- "--pretty-print",     -- (Optional) Pretty-print JSON messages for debugging.
                        -- "--compile-commands-dir=build", -- (Optional) Specify if compile_commands.json is in a non-standard dir.
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

            setup = {
                clangd = function(_, opts)
                    local clangd_ext_opts = LazyVim.opts("clangd_extensions.nvim")

                    require("clangd_extensions").setup(
                        vim.tbl_deep_extend("force", clangd_ext_opts or {}, { server = opts })
                    )
                    return false
                end,
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
            -- Other clangd_extensions.nvim options can be added here, e.g.:
            -- extended_diagnostics = {
            --   enabled = true,
            --   -- Options for extended diagnostics like fix-its, notes, etc.
            -- },
            -- custom_commands = {
            --   -- Define custom clangd commands.
            -- },
        },
    },
}

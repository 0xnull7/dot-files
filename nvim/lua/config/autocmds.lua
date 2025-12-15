-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua

-- Loading Default Template for LaTeX
vim.api.nvim_create_autocmd("BufNewFile", {
    pattern = "*.tex",
    callback = function(args)
        -- Path to your template (adjust as needed)
        local template_path = vim.fn.expand("~/Documents/LaTeX-Docs/Templates/Default Template.tex")
        -- Only insert template if file is empty (new file)

        if vim.fn.line("$") == 1 and vim.fn.getline(1) == "" then
            local lines = vim.fn.readfile(template_path)
            vim.api.nvim_buf_set_lines(args.buf, 0, -1, false, lines)
        end
    end,
})

--Automating .clang-format File Creation
-- Define the content of your .clang-format file using Lua's [[]] block
local CLANG_FORMAT_CONTENT = [[
BasedOnStyle: Google
IndentWidth: 4
ColumnLimit: 100
UseTab: Never
AccessModifierOffset: -4
AllowShortIfStatementsOnASingleLine: false
AllowShortFunctionsOnASingleLine: false
PointerAlignment: Right
SortIncludes: CaseSensitive
Standard: c++23
NamespaceIndentation: All
AlignConsecutiveAssignments: true
AlignConsecutiveDeclarations: true
ConstructorInitializerAllOnOneLineOrOnePerLine: true
BinPackParameters: false
]]

vim.api.nvim_create_autocmd("BufReadPost", {
    group = vim.api.nvim_create_augroup("ClangFormatAutomation", { clear = true }),
    pattern = { "*.c", "*.cpp", "*.h", "*.hpp" }, -- Trigger for C/C++ files
    callback = function(args)
        -- Find the project root using Neovim's built-in utility
        local root = vim.fn.getcwd() -- Simply use current working directory as root

        local clang_format_file = root .. "/.clang-format"

        -- Check if the .clang-format file already exists
        if vim.fn.filereadable(clang_format_file) == 0 then
            -- The file does not exist, so create it
            vim.fn.writefile(vim.split(CLANG_FORMAT_CONTENT, "\n"), clang_format_file)
            print("Created .clang-format file in the project root: " .. root)
        end
    end,
})

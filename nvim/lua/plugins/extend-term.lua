-- Helper function to properly escape paths and construct commands
-- This makes sure paths with spaces or special characters work correctly
local function create_run_cmd(file_path, command_template)
    local escaped_file_dir = vim.fn.fnameescape(vim.fn.fnamemodify(file_path, ":h"))
    local escaped_filename_t = vim.fn.fnameescape(vim.fn.fnamemodify(file_path, ":t"))
    local escaped_filename_r = vim.fn.fnameescape(vim.fn.fnamemodify(file_path, ":t:r"))

    local cmd = command_template
    -- Replace placeholders in the command_template:
    -- %%d: directory (escaped)
    -- %%f: full filename (escaped)
    -- %%r: filename without extension (escaped)
    cmd = string.gsub(cmd, "%%d", escaped_file_dir)
    cmd = string.gsub(cmd, "%%f", escaped_filename_t)
    cmd = string.gsub(cmd, "%%r", escaped_filename_r)

    return cmd
end

-- Helper function to run commands in Toggleterm
local function run_in_toggleterm(cmd_string, language_name)
    if pcall(require, "toggleterm.terminal") then
        require("toggleterm.terminal").Terminal
            :new({
                cmd = cmd_string, -- The clean command string is passed directly here
                direction = "float",
                hidden = false,
                close_on_exit = false, -- Set to 'false' if you want the terminal to stay open for review
                title = "Running " .. language_name .. " Script",
            })
            :toggle()
        vim.notify("Running " .. language_name .. " file...", vim.log.levels.INFO, { title = "Run Script" })
    else
        vim.notify("Toggleterm not loaded. Cannot run " .. language_name .. " file.", vim.log.levels.WARN)
    end
end

return {
    {
        "akinsho/toggleterm.nvim",
        opts = {
            -- Your existing toggleterm options
        },
        keys = {
            -- Keymap for Kotlin files: <leader>rk
            {
                "<leader>rk",
                function()
                    local ft = vim.bo.filetype
                    if ft == "kotlin" then
                        local file = vim.fn.expand("%:p")
                        -- CHANGED: Compile JAR to the current directory (after cd) and run it from there
                        local cmd =
                            create_run_cmd(file, "cd %d && kotlinc %f -include-runtime -d %r.jar && java -jar %r.jar")
                        run_in_toggleterm(cmd, "Kotlin")
                    else
                        vim.notify(
                            "Not a Kotlin file. Use <leader>rk only for Kotlin files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Run current Kotlin file",
            },

            -- Keymap for C files: <leader>rc
            {
                "<leader>rc",
                function()
                    local ft = vim.bo.filetype
                    if ft == "c" then
                        local file = vim.fn.expand("%:p")
                        -- cd to file's directory, compile locally, then run locally
                        local cmd = create_run_cmd(
                            file,
                            "cd %d && gcc -std=c99 -Wall -Wextra -Wconversion -Wsign-conversion -pedantic-errors %f -o %r && ./%r"
                        )
                        run_in_toggleterm(cmd, "C")
                    else
                        vim.notify(
                            "Not a C file. Use <leader>rc only for C files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Run current C file",
            },

            -- Keymap for C++ files: <leader>rcc
            {
                "<leader>rcc",
                function()
                    local ft = vim.bo.filetype
                    if ft == "cpp" then
                        local file = vim.fn.expand("%:p")
                        -- cd to file's directory, compile locally, then run locally
                        local cmd = create_run_cmd(
                            file,
                            "cd %d && g++ -std=c++23 -Wall -Wextra -Weffc++ -Wconversion -Wsign-conversion -pedantic-errors %f -o %r && ./%r"
                        )
                        run_in_toggleterm(cmd, "C++")
                    else
                        vim.notify(
                            "Not a C++ file. Use <leader>rcc only for C++ files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Run current C++ file",
            },

            -- Keymap for C# files: <leader>rcs
            {
                "<leader>rcs",
                function()
                    local ft = vim.bo.filetype
                    if ft == "cs" then
                        local file = vim.fn.expand("%:p")
                        -- cd to file's directory, then run dotnet
                        local cmd = create_run_cmd(file, "cd %d && dotnet run %f")
                        run_in_toggleterm(cmd, "C#")
                    else
                        vim.notify(
                            "Not a C# file. Use <leader>rcs only for C# files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Run current C# file",
            },

            -- Keymap for Java files: <leader>rj
            {
                "<leader>rj",
                function()
                    local ft = vim.bo.filetype
                    if ft == "java" then
                        local file = vim.fn.expand("%:p")
                        -- cd to file's directory, compile locally, then run locally
                        local cmd = create_run_cmd(file, "cd %d && javac %f && java %r")
                        run_in_toggleterm(cmd, "Java")
                    else
                        vim.notify(
                            "Not a Java file. Use <leader>rj only for Java files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Run current Java file",
            },

            -- Keymap for Lua files: <leader>rl
            {
                "<leader>rl",
                function()
                    local ft = vim.bo.filetype
                    if ft == "lua" then
                        local file = vim.fn.expand("%:p")
                        -- cd to file's directory, then run lua
                        local cmd = create_run_cmd(file, "cd %d && lua %f")
                        run_in_toggleterm(cmd, "Lua")
                    else
                        vim.notify(
                            "Not a Lua file. Use <leader>rl only for Lua files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Run current Lua file",
            },

            -- Keymap for Python files: <leader>rp
            {
                "<leader>rp",
                function()
                    local ft = vim.bo.filetype
                    if ft == "python" then
                        local file = vim.fn.expand("%:p")
                        -- cd to file's directory, then run python3
                        local cmd = create_run_cmd(file, "cd %d && python3 %f")
                        run_in_toggleterm(cmd, "Python")
                    else
                        vim.notify(
                            "Not a Python file. Use <leader>rp only for Python files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Run current Python file",
            },

            -- Keymap for LaTeX files:
            {
                "<leader>rx",
                function()
                    local ft = vim.bo.filetype
                    if ft == "tex" then
                        local file = vim.fn.expand("%:p")
                        local cmd = create_run_cmd(
                            file,
                            "cd %d && lualatex -interaction=nonstopmode -synctex=1 -shell-escape %f"
                        )
                        run_in_toggleterm(cmd, "LaTeX Compile")
                    else
                        vim.notify(
                            "Not a LaTeX file. Use <leader>rx only for .tex files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Compile current LaTeX file (lualatex)",
            },

            -- NEW: Keymap for PdfLaTeX: <leader>rlp
            {
                "<leader>rlp",
                function()
                    local ft = vim.bo.filetype
                    if ft == "tex" then
                        local file = vim.fn.expand("%:p")
                        local cmd = create_run_cmd(
                            file,
                            "cd %d && pdflatex -interaction=nonstopmode -synctex=1 -shell-escape %f"
                        )
                        run_in_toggleterm(cmd, "PdfLaTeX Compile")
                    else
                        vim.notify(
                            "Not a LaTeX file. Use <leader>rlp only for .tex files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Compile current LaTeX file (pdflatex)",
            },

            -- NEW: Keymap for XeLaTeX: <leader>rlx
            {
                "<leader>rlx",
                function()
                    local ft = vim.bo.filetype
                    if ft == "tex" then
                        local file = vim.fn.expand("%:p")
                        local cmd = create_run_cmd(
                            file,
                            "cd %d && xelatex -interaction=nonstopmode -synctex=1 -shell-escape %f"
                        )
                        run_in_toggleterm(cmd, "XeLaTeX Compile")
                    else
                        vim.notify(
                            "Not a LaTeX file. Use <leader>rlx only for .tex files.",
                            vim.log.levels.INFO,
                            { title = "Keymap Info" }
                        )
                    end
                end,
                desc = "Compile current LaTeX file (xelatex)",
            },
        },
    },
}

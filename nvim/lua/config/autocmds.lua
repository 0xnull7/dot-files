-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua

-------------------------------------------
-- CUSTOM COMMANDS (LATEX)
-------------------------------------------
local function compile_latex(engine)
    return function()
        local file = vim.fn.expand("%:p")
        local dir = vim.fn.expand("%:p:h")
        local base = vim.fn.expand("%:r")
        local pdf = base .. ".pdf"

        -- Save file
        vim.cmd("update")

        -- Build compilation command based on OS (Your original cross-platform logic)
        local cmd
        if vim.fn.has("win32") == 1 then
            cmd = string.format(
                'cd /D "%s" && %s -interaction=nonstopmode -synctex=1 -shell-escape "%s"',
                dir,
                engine,
                file
            )
        else
<<<<<<< HEAD
            cmd = string.format(
                'cd "%s" && %s -interaction=nonstopmode -synctex=1 -shell-escape "%s"',
                dir,
                engine,
                file
            )
=======
            cmd =
                string.format('cd "%s" && %s -interaction=nonstopmode -synctex=1 -shell-escape "%s"', dir, engine, file)
>>>>>>> ea41cc011c5e8ac9b01a3773f0f660a3a4f91452
        end

        -- Execute
        local result = vim.fn.system(cmd)
        if vim.v.shell_error ~= 0 then
            vim.api.nvim_err_writeln("Compilation failed")
            print(result)
            return
        end
    end
end

vim.api.nvim_create_user_command("Pdflatex", compile_latex("pdflatex"), {})
vim.api.nvim_create_user_command("Xelatex", compile_latex("xelatex"), {})
vim.api.nvim_create_user_command("Lualatex", compile_latex("lualatex"), {})

-------------------------------------------
-- AUTOCMDS
-------------------------------------------

-- 1. Loading Default Template for LaTeX
vim.api.nvim_create_autocmd("BufNewFile", {
    pattern = "*.tex",
    callback = function(args)
        -- Path to your template (adjust as needed)
        local template_path = vim.fn.expand("E:/LaTeX-Docs/Templates/Default Template.tex")
        -- Only insert template if file is empty (new file)

        if vim.fn.line("$") == 1 and vim.fn.getline(1) == "" then
            local lines = vim.fn.readfile(template_path)
            vim.api.nvim_buf_set_lines(args.buf, 0, -1, false, lines)
        end
    end,
})
<<<<<<< HEAD
=======

>>>>>>> ea41cc011c5e8ac9b01a3773f0f660a3a4f91452

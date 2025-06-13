-- In this file I overridden some of lualine plugin configurations and added mine.

local conditions = {
    buffer_not_empty = function()
        return vim.fn.empty(vim.fn.expand("%:t")) ~= 1
    end,
    hide_in_width = function()
        return vim.fn.winwidth(0) > 80
    end,
    check_git_workspace = function()
        local filepath = vim.fn.expand("%:p:h")
        local gitdir = vim.fn.finddir(".git", filepath .. ";")
        return gitdir and #gitdir > 0 and #gitdir < #filepath
    end,
}

return {
    {
        "nvim-lualine/lualine.nvim",
        event = "VeryLazy",
        config = function()
            require("lualine").setup({
                options = {
                    icons_enabled = true,
                    theme = "onedark",
                    component_separators = { left = "", right = "" },
                    section_separators = { left = "", right = "" },
                    disabled_filetypes = {
                        statusline = {},
                        winbar = {},
                    },
                    always_divide_middle = true,
                    globalstatus = true,
                    refresh = {
                        statusline = 100,
                        tabline = 100,
                        winbar = 100,
                    },
                },
                sections = {
                    lualine_a = {
                        "mode",
                        {
                            function()
                                return " "
                            end,
                            separator = { left = "", right = "" },
                            padding = { right = 0 },
                        },
                    },
                    lualine_b = {
                        {
                            "branch",
                            icon = "",
                            color = { gui = "bold" },
                            separator = { left = "", right = "" },
                        },
                        {
                            "diff",
                            symbols = { added = " ", modified = " ", removed = " " },
                        },
                        {
                            "filename",
                            cond = conditions.buffer_not_empty,
                            color = { gui = "bold" },
                        },
                    },
                    lualine_c = {
                        {
                            "diagnostics",
                            sources = { "nvim_diagnostic" },
                            symbols = { error = " ", warn = " ", info = " " },
                        },
                    },
                    lualine_x = {
                        {
                            "%l:%c %p%%|%L",
                            color = { gui = "bold" },
                        },
                    },
                    lualine_y = {
                        {
                            function()
                                local msg = "No Active Lsp"
                                local buf_ft = vim.api.nvim_get_option_value("filetype", { buf = 0 })
                                local clients = vim.lsp.get_clients()
                                if next(clients) == nil then
                                    return msg
                                end
                                for _, client in ipairs(clients) do
                                    local filetypes = client.config.filetypes
                                    if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
                                        return client.name
                                    end
                                end
                                return msg
                            end,
                            icon = "  LSP:",
                            color = { gui = "bold" },
                        },
                        { "filetype", colored = true },
                        { "encoding", fmt = string.upper },
                        { "fileformat", fmt = string.upper },
                    },
                    lualine_z = {
                        {
                            "filesize",
                            cond = conditions.buffer_not_empty,
                            color = { gui = "bold" },
                        },
                    },
                },
                inactive_sections = {
                    lualine_a = {},
                    lualine_b = {},
                    lualine_c = { "filename" },
                    lualine_x = { "location" },
                    lualine_y = {},
                    lualine_z = {},
                },
            })
        end,
    },
}

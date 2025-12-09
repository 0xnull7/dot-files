return {
    "ray-x/web-tools.nvim",
    cmd = {
        "BrowserSync",
        "BrowserOpen",
        "BrowserPreview",
        "BrowserRestart",
        "BrowserStop",
    },
    keys = {
        { "<leader>bs", "<cmd>BrowserSync<CR>", desc = "Start BrowserSync" },
        { "<leader>bo", "<cmd>BrowserOpen<CR>", desc = "Open Browser" },
        { "<leader>br", "<cmd>BrowserRestart<CR>", desc = "Restart Browser" },
        { "<leader>be", "<cmd>BrowserStop<CR>", desc = "Stop Browser" },
    },
    config = function()
        require("web-tools").setup({
            browser_sync_args = {
                "--port",
                "8080",
                "--files",
                "*.html,*.css,*.js",
                -- "--directory", -- Serve the current directory from the project root
                -- "--no-notify", -- Disable browser notifications for a cleaner experience
            },
        })

        -- Autocommand to automatically start BrowserSync and open the browser
        -- when an HTML file is entered or opened.
        vim.api.nvim_create_autocmd({ "BufEnter", "BufReadPost" }, {
            pattern = "*.html",
            callback = function()
                vim.cmd("BrowserSync")
                vim.cmd("BrowserOpen")
            end,
            desc = "Auto-start BrowserSync and open browser for HTML files",
        })
    end,
}

return {
    "ray-x/web-tools.nvim",
    cmd = {
        "BrowserSync",
        "BrowserOpen",
        "BrowserPreview",
        "BrowserRestart",
        "BrowserStop",
        -- Other web-tools commands like TagRename, HurlRun, Npm, Yarn, etc.
    },
    config = function()
        require("web-tools").setup({
            browser_sync_args = {
                "--port",
                "8080",
                "--files",
                "*.html,*.css,*.js", -- Specify files to watch
                -- "--directory", -- Serve the current directory
                -- "--no-notify", -- Disable browser notifications
            },
        })
    end,
}

return {
    "junegunn/vim-easy-align",
    config = function()
        vim.keymap.set("x", "gaic", "<Plug>(EasyAlign)", { desc = "Align inline comments" })
    end,
}

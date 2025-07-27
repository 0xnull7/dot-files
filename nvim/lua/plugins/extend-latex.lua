return {
    "lervag/vimtex",
    ft = "tex",
    init = function()
        -- Enable vimtex's syntax concealment feature
        vim.g.vimtex_syntax_conceal_disable = 0 -- 0 (default) to enable, 1 to disable

        -- Crucial for vimtex concealment to work:
        vim.opt.conceallevel = 2

        -- How concealment behaves under the cursor:
        -- "" or 0: No concealment under the cursor (recommended for editing)
        -- "nvic": Conceal in Normal, Visual, Insert, Command mode (conceals everything)
        vim.opt.concealcursor = ""
    end,
    -- You can add other vimtex options here if needed,
    -- e.g., for specific conceal settings controlled by g:vimtex_syntax_conceal
    -- config = function()
    --   vim.g.vimtex_syntax_conceal = 'abx' -- Example: Conceal all (default)
    -- end
}

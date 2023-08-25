return {
    {
        "nvim-telescope/telescope.nvim",
        name = "telescope",
        dependencies = {
            'nvim-lua/popup.nvim',
            'nvim-lua/plenary.nvim',
            'telescope-fzf-native.nvim',
            'telescope-lsp-handlers.nvim',
            'nvim-telescope/telescope-file-browser.nvim'
        },
        config = function()
            require 'config.telescope'
        end,
        cmd = 'Telescope'
    },
    {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = 'make'
    },
    {
        'gbrlsnchs/telescope-lsp-handlers.nvim',
    },
    {
        'nvim-telescope/telescope-file-browser.nvim'
    }
}

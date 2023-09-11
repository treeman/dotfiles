return {
    'goolord/alpha-nvim',
    lazy = false,
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function ()
        --require'alpha'.setup(require'alpha.themes.startify'.config)
        require('alpha').setup(require'config.dashboard')
    end
}

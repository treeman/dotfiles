# Paths
set -x PATH $PATH /usr/local/sbin /usr/sbin /sbin
set -x PATH ~/.cabal/bin $PATH
set -x PATH ~/bin ~/dotfiles/bin $PATH

# Workaround for local timezone discovery
set -x TZ Europe/Stockholm

# git message editor and other things
set -x EDITOR nvim

alias vim nvim
alias g git

set -x FZF_DEFAULT_COMMAND 'ag --hidden --ignore .git -g ""'

function reload
    source ~/.config/fish/config.fish
end

# Colorscheme
theme_gruvbox dark hard

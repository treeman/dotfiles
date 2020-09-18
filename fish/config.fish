# Paths
set -x PATH $PATH /usr/local/sbin /usr/sbin /sbin
set -x PATH ~/.cabal/bin $PATH
set -x PATH ~/.cargo/bin ~/.cargo/env $PATH
set -x PATH ~/.local/bin $PATH
set -x PATH ~/bin ~/dotfiles/bin $PATH

# Workaround for local timezone discovery
set -x TZ Europe/Stockholm

# git message editor and other things
set -x EDITOR vim

# alias vim nvim
# alias v nvim
alias g git
alias t "task rc:/mnt/c/CetDev/10.5git/personal/profile/jonhi/.taskrc"

set -x FZF_DEFAULT_COMMAND 'fd --type file --hidden --exclude .git'

set -x BASE /mnt/c/CetDev/10.5git/base/
set -x PROFILE /mnt/c/CetDev/10.5git/personal/profile/jonhi/

function reload
    source ~/.config/fish/config.fish
end

# Colorscheme
gruvbox

source ~/.config/fish/gtd.fish


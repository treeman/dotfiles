# Base16 Shell
#eval sh $HOME/.config/base16-shell/base16-default.dark.sh

set -x PATH $PATH /usr/local/sbin /usr/sbin /sbin
set -x PATH ~/.cabal/bin $PATH
set -x PATH ~/bin ~/dotfiles/bin $PATH

# For java
#set -x PATH $PATH /usr/lib64/java/bin

# For rust...?
#set -x LD_LIBRARY_PATH /usr/local/lib /usr/local/lib64 /lib /usr/lib /usr/lib64 $LD_LIBRARY_PATH

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

# Base16 Shell
#eval sh $HOME/.config/base16-shell/base16-default.dark.sh

# Some slackbuild scripts want this
set -x ARCH x86_64

set -x PATH /opt/man-db/bin /opt/man-db/sbin $PATH
set -x PATH $PATH /usr/local/sbin /usr/sbin /sbin
set -x PATH ~/.rakudobrew/bin ~/.cabal/bin $PATH
set -x PATH ~/bin $PATH

# For Krita
set -x PATH $PATH /opt/kde4/inst/bin
set -x KDEDIRS /opt/kde4/inst $KDEDIRS

# git message editor and other things
set -x EDITOR nvim

alias vim nvim
alias g git

# List and sort packages by size
function packages
    fgrep UNCOMPRESSED /var/log/packages/* | awk -F: '{print $3, $1}' | sort -h
end

function reload
    source ~/.config/fish/config.fish
end



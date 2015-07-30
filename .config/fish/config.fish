
# Some slackbuild scripts want this
set -x ARCH x86_64
set -x PATH ~/bin /opt/man-db/bin /opt/man-db/sbin $PATH

alias vim nvim
alias g git

# List and sort packages by size
function packages
    fgrep UNCOMPRESSED /var/log/packages/* | awk -F: '{print $3, $1}' | sort -h
end

function reload
    source ~/.config/fish/config.fish
end


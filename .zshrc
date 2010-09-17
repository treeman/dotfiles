zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '/home/tree/.zshrc'

# Add autocompetion
autoload -Uz compinit
compinit

# History
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob

# Used with slackbuilds to set 64bit environment
export ARCH=x86_64

export JAVAWS_HOME=/usr/lib64/java/javaws

export PATH=.:~/bin:/usr/sbin:/usr/local/sbin:/usr/local/bin:/sbin:/usr/sbin:/bin:/usr/bin:/usr/lib64/java/javaws:/usr/lib64/java/bin

export LD_LIBRARY_PATH=/usr/local/lib:/usr/lib

export EDITOR=/usr/local/bin/vim
export SHELL=/bin/zsh
export BROWSER=/home/tree/bin/uzbl
export TERM='xterm-256color'

# Force the use of my beloved characters
export LANG=en_US.utf8

autoload -U colors && colors

# I prefer a simple color-coded prompt for different users
CLEARCOL=$'\e[0m'
PCHAR='$'

if [ $(id -u) -eq 0 ]; then
    COL=$'\e[1;31m'
    PCHAR='#'
elif [ $(whoami) = tree ]; then
    COL=$'\e[1;33m'
else
    COL=$'\e[1;35m'
fi

PS1="$COL$PCHAR $CLEARCOL"
PS2="$COL> $CLEARCOL"
PS4="$COL+ $CLEARCOL"

# Enable ls color support
if [ "$TERM" != "dumb" ]; then
    eval `dircolors -b`
    alias ls='ls -h --color'
fi

alias lsd='ls -d *(-/DN)' # List dirs and symbolic links to dirs
alias lsa='ls -d .*' # Only list hidden files

alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'
alias xterm='xterm'

alias reboot='su -c reboot'
alias shutdown='su -c "shutdown -h now"'

# Vim key bindings ty
bindkey -v

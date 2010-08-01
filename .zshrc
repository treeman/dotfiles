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

ARCH=x86_64

PATH=$PATH:~/bin:~/rakudo:/usr/sbin
LD_LIBRARY_PATH=/usr/local/lib:/usr/lib

export EDITOR=/usr/local/bin/vim
export SHELL=/bin/zsh

autoload -U colors && colors

if [ $(id -u) -eq 0 ]; then # Root
    PS1=$'\e[1;31m# \e[0m'
    PS2=$'\e[1;31m> \e[0m'
    PS4=$'\e[1;31m+ \e[0m'
else
    PS1=$'\e[1;33m$ \e[0m'
    PS2=$'\e[1;33m> \e[0m'
    PS4=$'\e[1;33m+ \e[0m'
fi

# Enable ls color support
if [ "$TERM" != "dumb" ]; then
    eval `dircolors -b`
    alias ls='ls -h --color'
fi

alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'
alias xterm='xterm -e /bin/zsh'
alias lsd='ls -d *(-/DN)' # List dirs and symbolic links to dirs
alias lsa='ls -d .*' # Only list hidden files

# Vim key bindings ty
bindkey -v

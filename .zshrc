zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '/home/tree/.zshrc'

# Add autocompletion
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

export PATH=.:~/bin:~/.cabal/bin:/usr/sbin:/usr/local/sbin:/usr/local/bin:/sbin:/usr/sbin:/bin:/usr/bin:/usr/lib64/java/javaws:/usr/lib64/java/bin:/usr/share/texmf/bin

export MANPATH=/usr/share/texmf/doc/man:/usr/local/share/man:$MANPATH

export LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib64:/usr/lib64:/usr/local/lib64

export EDITOR=vim
export SHELL=/bin/zsh
#export BROWSER=/home/tree/bin/uzbl
export TERM='xterm-256color'

# Force the use of my beloved characters
export LANG=en_US.utf8

autoload -U colors && colors

#Color table from: http://www.understudy.net/custom.html
black=%{$'\e[0;30m'%}
red=%{$'\e[0;31m'%}
green=%{$'\e[0;32m'%}
brown=%{$'\e[0;33m'%}
blue=%{$'\e[0;34m'%}
purple=%{$'\e[0;35m'%}
cyan=%{$'\e[0;36m'%}
lgray=%{$'\e[0;37m'%}
dgray=%{$'\e[1;30m'%}
lred=%{$'\e[1;31m'%}
lgreen=%{$'\e[1;32m'%}
yellow=%{$'\e[1;33m'%}
lblue=%{$'\e[1;34m'%}
pink=%{$'\e[1;35m'%}
lcyan=%{$'\e[1;36m'%}
white=%{$'\e[1;37m'%}

CLEARCOL=$lgray
PCHAR='$'

# Different color and symbol for root
if [ $(id -u) -eq 0 ]; then
    COL=$lred
    PCHAR='#'
else
    COL=$yellow
fi

PS1="$brown%n@%m:%~$CLEARCOL 
$COL$PCHAR $CLEARCOL"
PS2="$COL> $CLEARCOL"
PS4="$COL+ $CLEARCOL"

# Vi key bindings ty
bindkey -v

# Delete key actually deletes now
bindkey "^[[3~"  delete-char
bindkey "^[3;5~" delete-char

# Enable ls color support
if [ "$TERM" != "dumb" ]; then
    eval `dircolors -b`
    alias ls='ls -h --color'
fi

alias lsd='ls -d *(-/DN)' # List dirs and symbolic links to dirs
alias lsa='ls -d .*' # Only list hidden files

alias g='git'   # Yay go git go!
alias t='task'  # Warrior, lend me your strength!

alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'

# Just lazy
alias reboot='su -c reboot'
alias shutdown='su -c "shutdown -h now"'

alias rshred='shred -n 31337 -z -u'

# Lazy ssh
alias forest='ssh forest'
alias eforest='ssh eforest'
alias liu='ssh liu'

# Some fun and useful
alias filetop="watch -d -n 2 'df; ls -FlAt;'"
alias inram='dd if=/dev/mem | cat | strings'
alias packages="fgrep UNCOMPRESSED /var/log/packages/* | awk -F: '{print \$3,\$1}' | sort -rh" # list installed packages by size
alias mtr='mtr --curses' # don't want an ugly gui

# A lot of work gone?
alias ..='cd ../'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'


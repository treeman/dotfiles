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

export JAVA_HOME=/usr/lib64/java
export JAVAWS_HOME=/usr/lib64/java/javaws

export PATH=.:~/bin:~/.cabal/bin:/opt/jre1.6.0_38/bin/:/opt/sbt/bin:/usr/sbin:/usr/local/sbin:/usr/local/bin:/sbin:/usr/sbin:/bin:/usr/bin:/usr/lib64/java/javaws:/usr/lib64/java/bin:/usr/share/texmf/bin:/usr/share/smlnj/bin:/usr/local/languages/perl6/site/bin:~/.rakudobrew/bin

export MANPATH=/usr/share/texmf/doc/man:/usr/local/share/man:$MANPATH

export LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib64:/usr/lib64:/usr/local/lib64

export EDITOR=vim
export SHELL=/bin/zsh
#export BROWSER=/home/tree/bin/uzbl
export TERM='xterm-256color'

# Force the use of my beloved characters
export LANG=en_US.utf8

autoload -U colors && colors

# To use Krita
export KDEDIRS=$HOME/kde4/inst:$KDEDIRS
export PATH=$HOME/kde4/inst/bin:$PATH

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

# Emacs key bindings ty
bindkey -e

# Delete key actually deletes now
bindkey "^[[3~"  delete-char
bindkey "^[3;5~" delete-char

# Make ls better
LS_OPTIONS="-F -b -T 0 --color=auto"
export LS_OPTIONS
eval `dircolors -b $HOME/.dir_colors`

alias ls='/bin/ls ${=LS_OPTIONS}'
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

# Some fun and useful
alias filetop="watch -d -n 2 'df; ls -FlAt;'"
alias inram='dd if=/dev/mem | cat | strings'
alias packages="fgrep UNCOMPRESSED /var/log/packages/* | awk -F: '{print \$3,\$1}' | sort -rh" # list installed packages by size

# A lot of work gone?
alias ..='cd ../'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'

# Faster screen switching T.T
alias ss='single-screen && startx'
alias ds='dual-screen && startx'

alias g++11='g++ -g -O2 -static -std=gnu++0x -Wall'
alias clang++11='clang++ --std=c++11 -Wall'

utf8()
{
    iconv -f ISO-8859-1 -t UTF-8 $1 > $1.tmp
    rm $1
    mv $1.tmp $1
}

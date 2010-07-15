PATH=$PATH:~/bin:~/rakudo:/usr/sbin
ARCH=x86_64

# Make bash civilized :)
set -o vi

# Dynamic resizing
shopt -s checkwinsize

if [ $(id -u) -eq 0 ]; then # Root
    PS1='\[\e[1;31m\]\$ \[\e[0m\]'
    PS2='\[\e[1;31m\]> \[\e[0m\]'
    PS4='\[\e[1;31m\]+ \[\e[0m\]'
else # Or not
    PS1='\[\e[1;33m\]\$ \[\e[0m\]'
    PS2='\[\e[1;33m\]> \[\e[0m\]'
    PS4='\[\e[1;33m\]+ \[\e[0m\]'
fi

export LS_OPTIONS='--color=auto'
eval `dircolors -b`
alias ls='ls $LS_OPTIONS'

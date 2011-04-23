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

alias ls='ls -hF --color'
alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'

alias xterm='xterm -e /bin/zsh'

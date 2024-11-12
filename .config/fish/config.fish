# Paths
set -x PATH $PATH /usr/local/sbin /usr/sbin /sbin /usr/local/bin
set -x PATH ~/.cabal/bin $PATH
set -x PATH ~/.cargo/bin ~/.cargo/env $PATH
set -x PATH ~/.local/bin $PATH
set -x PATH ~/bin ~/dotfiles/bin $PATH
set -x PATH ~/.linuxbrew/bin $PATH
set -x PATH ~/go/bin $PATH
set -x PATH ~/.yarn/bin $PATH
set -x PATH ~/.fly/bin $PATH
set -x PATH ~/.fly/bin $PATH
set -x PATH ~/src/bitcoin-cash-node/build/src $PATH
set -x PATH /usr/lib/psql15/bin $PATH
set -x PATH ~/.nvim $PATH
set -x PATH ~/.rye/shims $PATH

set -gx PNPM_HOME ~/.local/share/pnpm
set -gx PATH "$PNPM_HOME" $PATH

set -x PERL5LIB ~/perl5/lib/perl5 $PERL5LIB
# Beancount module finding for importers
set -x PYTHONPATH $PYTHONPATH ~/vimwiki/money/accounting/
set -x ELIXIR_LS_LANGUAGE_SERVER $HOME/src/elixir-ls/release/language_server.sh

# Some things requires this
set -x XDG_RUNTIME_DIR /tmp/(id -u)

# Workaround for local timezone discovery
set -x TZ Europe/Stockholm

# git message editor and other things
set -x EDITOR nvim

set -x BROWSER firefox

# Where is this coming from??
set -e PORT

alias vim nvim
# alias v "nvim ~/org/plan.dj"
alias v nvim
alias g git

# Always use neovide multigrid mode
set -x NEOVIDE_MULTIGRID

if test $hostname != "winterfell"
    set -x NORMAL_KEYBOARD 1
end

# zlip decompress
alias zinflate='ruby -r zlib -e "STDOUT.write Zlib::Inflate.inflate(STDIN.read)"'

alias rcp='rsync --verbose --progress --partial'

set -x FZF_DEFAULT_COMMAND 'fd --type file --hidden --exclude .git'

set -gx GPG_TTY (tty)

zoxide init fish | source

function reload
    source ~/.config/fish/config.fish
end

# Colorscheme
gruvbox

set secret_file ~/.env.secrets.fish
if test -e $secret_file
    source $secret_file
end

alias n "nvim -c \":lua require('config.org').open_org_file_telescope('')\""
alias ep "nvim -c \":lua require('config.org').open_org_file_telescope('projects')\""
alias ea "nvim -c \":lua require('config.org').open_org_file_telescope('areas')\""
alias er "nvim -c \":lua require('config.org').open_org_file_telescope('resources')\""
alias eA "nvim -c \":lua require('config.org').open_org_file_telescope('archive')\""
# alias ej "nvim -c \":lua require('config.org').open_weekly_journal()\""

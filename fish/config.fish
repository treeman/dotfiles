# Paths
set -x PATH $PATH /usr/local/sbin /usr/sbin /sbin /usr/local/bin
set -x PATH ~/.cabal/bin $PATH
set -x PATH ~/.cargo/bin ~/.cargo/env $PATH
set -x PATH ~/.local/bin $PATH
set -x PATH ~/bin ~/dotfiles/bin $PATH
set -x PATH ~/.linuxbrew/bin $PATH
set -x PATH ~/go/bin $PATH

# Beancount module finding for importers
set -x PYTHONPATH $PYTHONPATH ~/vimwiki/money/accounting/
set -x ELIXIR_LS_LANGUAGE_SERVER $HOME/src/elixir-ls/release/language_server.sh

# Workaround for local timezone discovery
set -x TZ Europe/Stockholm

# git message editor and other things
set -x EDITOR nvim

alias vim nvim
alias v nvim
alias g git

# zlip decompress
alias zinflate='ruby -r zlib -e "STDOUT.write Zlib::Inflate.inflate(STDIN.read)"'

alias scp='rsync --verbose --progress --partial'

set -x FZF_DEFAULT_COMMAND 'fd --type file --hidden --exclude .git'

set -gx GPG_TTY (tty)

zoxide init fish | source

function reload
    source ~/.config/fish/config.fish
end

# Colorscheme
gruvbox

source ~/.config/fish/gtd.fish

alias ww 'nvim ~/vimwiki/index.markdown'

set secret_file ~/.env.secrets.fish
if test -e $secret_file
    source $secret_file
end

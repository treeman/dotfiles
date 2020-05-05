# Paths
set -x PATH $PATH /usr/local/sbin /usr/sbin /sbin
set -x PATH ~/.cabal/bin $PATH
set -x PATH ~/.cargo/bin ~/.cargo/env $PATH
set -x PATH ~/bin ~/dotfiles/bin $PATH

# Beancount module finding for importers
set -x PYTHONPATH $PYTHONPATH ~/vimwiki/money/accounting/

# Workaround for local timezone discovery
set -x TZ Europe/Stockholm

# git message editor and other things
set -x EDITOR nvim

alias vim nvim
alias g git

# zlip decompress
alias zinflate='ruby -r zlib -e "STDOUT.write Zlib::Inflate.inflate(STDIN.read)"'

set -x FZF_DEFAULT_COMMAND 'ag --hidden --ignore .git -g ""'

function reload
    source ~/.config/fish/config.fish
end

set fish_greeting ""

# Colorscheme
gruvbox



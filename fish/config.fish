# Paths
set -x PATH $PATH /usr/local/sbin /usr/sbin /sbin
set -x PATH ~/.cabal/bin $PATH
set -x PATH ~/.cargo/bin ~/.cargo/env $PATH
set -x PATH ~/.local/bin $PATH
set -x PATH ~/bin ~/dotfiles/bin $PATH

# Beancount module finding for importers
set -x PYTHONPATH $PYTHONPATH ~/vimwiki/money/accounting/

# Workaround for local timezone discovery
set -x TZ Europe/Stockholm

# git message editor and other things
set -x EDITOR nvim

alias vim nvim
alias v nvim
alias g git
alias t task

# zlip decompress
alias zinflate='ruby -r zlib -e "STDOUT.write Zlib::Inflate.inflate(STDIN.read)"'

set -x FZF_DEFAULT_COMMAND 'fd --type file --hidden --exclude .git'

function reload
    source ~/.config/fish/config.fish
end

set fish_greeting ""

# Colorscheme
gruvbox

# Easily add to GTD inbox
alias in 'task add +in'

# Easy handling of tickler files
function tickle
    set deadline $argv[1]
    in +tickle wait:$deadline $argv[2..-1]
end
alias tick tickle

# Sleep on a task...
alias think 'tickle +1d'

# For R&D
alias rnd 'task add +rnd +next +@computer'

# Track maybe tasks inside vimwiki instead
alias maybe "vim $HOME/vimwiki/maybe.markdown"
# Track notes per project
function _note
  set id $argv
  set dir "$HOME/vimwiki/projects/"
  set file "$dir/$id.markdown"

  mkdir -p $dir
  vim "$file"
end
alias n _note

# For things to read
function webpage_title
    # Requires the html-xml-utils package
    wget -qO- $argv | hxclean | hxselect -s '\n' -c  'title' 2>/dev/null
end

function _read
    set input $argv
    set url (echo $input | rg -o "(https?://\S+)")
    if test -z $url
        task add +read +next +@home $input
    else
        set title (webpage_title $url)

        if test -z $title
            echo "No title found from url!"
        else
            echo $title
            set descr "Read: \"$title\""
            task add +read +next +@computer url:$url $descr
        end
    end
end
alias rd _read

function _open_task_to_read
    set id $argv[1]
    set url (task _get $id.url)
    if test -z $url
        echo "No url for task $id!"
    else
        firefox $url
        echo "Opened $url in firefox"
    end
end
alias open-rd _open_task_to_read


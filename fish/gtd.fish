# Easily add to GTD inbox
alias in 't add +in'

# Easy handling of tickler files
# Either supports adding a new task:
#   tickle +1w "New task"
# Or modifying an existing task:
#   tickle +1w 13
function tickle
    set deadline $argv[1]
    set rest $argv[2..-1]
    _mod_or_add $rest wait:$deadline -next +in +tickle
end
alias tick tickle

# Sleep on a task...
alias think 'tickle +1d'

# Easy add to lists
# Can either add a new task:
#   rnd "This awesome book"
# Or modify an existing task:
#   rnd 2
# And they accepts an url and parses the title from the website:
#   rnd https://github.com/fish-shell/fish-shell/issues/5186
function rnd
    _with_url $argv +rnd +next
end
function watch
    _with_url $argv +watch +next
end
function listen
    _with_url $argv +listen +next
end
function rd
    _with_url $argv +read +next
end

# Returns 0 if $argv only contains a single integer, and 1 otherwise
function _single_num
    if test (count $argv) = 1
        if string match -aqr '^[0-9]+$' $argv[1]
            return 0
        end
    end
    return 1
end

# Either mod a task with if the input is a single integer, representing an existing task
# or add a new task.
function _mod_or_add
    set input $argv[1]
    set flags $argv[2..-1]

    if _single_num $input
        t mod $input -in $flags
    else
        t add $flags $input
    end
end

# For things to read
function webpage_title
    # Requires the html-xml-utils package
    wget -qO- $argv | hxclean | hxselect -s '\n' -c  'title' 2>/dev/null
end

# Like _mod_or_add but if the input is an url it creates a new task with the url: parameter
# and the description taken from the website title.
function _with_url
    set input $argv[1]
    set flags $argv[2..-1]

    set url (echo $input | rg -o "(https?://\S+)")
    if test "x-$url" = "x-"
        _mod_or_add $input $flags
    else
        set title (webpage_title $url)

        if test "x-$title" = "x-"
            echo "No title found from url!"
        else
            echo $title
            t add $flags url:$url $title
        end
    end
end

function _open_task_to_read
    set id $argv[1]
    set url (task _get $id.url)
    if test "x-$url" = "x-"
        echo "No url for task $id!"
    else
        firefox $url
        echo "Opened $url in firefox"
    end
end
alias topen _open_task_to_read

# Track maybe tasks inside vimwiki instead
alias maybe "vim $HOME/vimwiki/projects/maybe.markdown"
# Track notes per project
function _note
  set id $argv
  set dir "$HOME/vimwiki/projects/"
  set file "$dir$id.markdown"

  mkdir -p $dir
  vim "$file"
end
alias n _note


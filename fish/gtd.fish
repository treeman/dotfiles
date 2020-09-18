# Easily add to GTD inbox
alias in 't add +in'

# Easy handling of tickler files
function tickle
    set deadline $argv[1]
    in +tickle wait:$deadline $argv[2..-1]
end
alias tick tickle

# Sleep on a task...
alias think 'tickle +1d'

# For R&D
alias rnd 't add +rnd +next +@computer'

# For things to read
function webpage_title
    # Requires the html-xml-utils package
    wget -qO- $argv | hxclean | hxselect -s '\n' -c  'title' 2>/dev/null
end

function _read
    set input $argv
    set url (echo $input | rg -o "(https?://\S+)")
    if test -z $url
        t add +read +next +@home $input
    else
        set title (webpage_title $url)

        if test -z $title
            echo "No title found from url!"
        else
            echo $title
            set descr "Read: \"$title\""
            t add +read +next +@computer url:$url $descr
        end
    end
end
alias rd _read

function _open_task_to_read
    set id $argv[1]
    set url (t _get $id.url)
    if test -z $url
        echo "No url for task $id!"
    else
        firefox $url
        echo "Opened $url in firefox"
    end
end
alias open-rd _open_task_to_read


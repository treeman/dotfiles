function fish_greeting
    if contains -- $USER root toor
        return
    end

    set projects (projects_without_next)
    if test -n projects
        echo "Attention: The following projects don't currently have a next action:"
        echo $projects
        echo
    end

    set wating = (task +waiting +PENDING count)
    if test -n wating
        echo "Any progress on these waiting-fors?"
        task +waiting +PENDING ls
    end
end


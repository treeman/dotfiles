function countdown
    set date1 (expr (date +%s) + $argv[1])
    set last $argv[1]
    while test $date1 -ge (date +%s)
        set diff (expr $date1 - (date +%s))
        if test $diff != $last
            echo $diff
            set last $diff
        end
        sleep 0.1
    end
end

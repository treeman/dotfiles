function last_modified
  if test -z $argv[1]
    echo "Specify path"
  else
    find $argv[1] -type f -print0 | xargs -0 stat --format '%Y :%y %n' | sort -nr | cut -d: -f2-
  end
end


# Only allow certain file endings
complete -c explodepkg -x -a "(
__fish_complete_suffix (commandline -ct) .tbz 'bzip2 package'
)"
complete -c explodepkg -x -a "(
__fish_complete_suffix (commandline -ct) .tlz 'lzma package'
)"
complete -c explodepkg -x -a "(
__fish_complete_suffix (commandline -ct) .txz 'xz package'
)"
complete -c explodepkg -x -a "(
__fish_complete_suffix (commandline -ct) .tgz 'gzip package'
)"

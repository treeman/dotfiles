# There are more options available but I'm lazy.
# Should probably make an autogenerator from the help screen.

# Only allow certain file endings
complete -c installpkg -x -a "(
__fish_complete_suffix (commandline -ct) .tbz 'bzip2 package'
)"
complete -c installpkg -x -a "(
__fish_complete_suffix (commandline -ct) .tlz 'lzma package'
)"
complete -c installpkg -x -a "(
__fish_complete_suffix (commandline -ct) .txz 'xz package'
)"
complete -c installpkg -x -a "(
__fish_complete_suffix (commandline -ct) .tgz 'gzip package'
)"

complete -c installpkg -l terse -d 'Install the package displaying only a
                                    single description line to stdout'
complete -c installpkg -l warn -d 'Generate a list of files that would be
                                   overwritten to the standard  output,
                                   but  do  not actually  install  the  package'
complete -c installpkg -l md5sum -d 'Record the package md5sum in the metadata
                                     written in /var/log/packages'


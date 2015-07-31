# There are more options available but I'm lazy.
# Should probably make an autogenerator from the help screen.

# Only allow certain file endings
complete -c upgradepkg -x -a "(
__fish_complete_suffix (commandline -ct) .tbz 'bzip2 package'
)"
complete -c upgradepkg -x -a "(
__fish_complete_suffix (commandline -ct) .tlz 'lzma package'
)"
complete -c upgradepkg -x -a "(
__fish_complete_suffix (commandline -ct) .txz 'xz package'
)"
complete -c upgradepkg -x -a "(
__fish_complete_suffix (commandline -ct) .tgz 'gzip package'
)"

complete -c upgradepkg -l dry-run -d 'Output a report about which packages would be
                                      installed or upgraded but  don\'t  actually  perform  the
                                      upgrades'
complete -c upgradepkg -l intall-new -d 'If specified, install new packages in addition to
                                         upgrading existing ones'
complete -c upgradepkg -l reinstall -d 'Used if you  want  to  upgrade  all
                                        packages even if the same version is already installed'


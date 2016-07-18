# Could make some smart way of autocompleting with some valid extensions,
# but leave package name to the user.

complete -c makepkg -s l -l linkadd -a 'y n' -x -d 'If y, add any symbolic links found to the
                                                    install script and delete them, otherwise prompt'
complete -c makepkg -s p -l prepend -d 'If this option is given, then any symbolic links added to
                                        doinst.sh will be prepended to the  existing script'
complete -c makepkg -s c -l chown -a 'y n' -x -d 'If y, makepkg will reset all directory permissions
                                                  to 755 and ownership to root:root, otherwise prompt'


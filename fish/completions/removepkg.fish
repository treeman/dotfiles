# This saves sooo much time.
# Can also specify only base package name etc, but hard to make completion for.
complete -x -c removepkg -a "(ls /var/log/packages)"
complete -c removepkg -o warn -d 'Generate a report about which files and directories would be
                                  removed but does not actually remove the package'
complete -c removepkg -o preserve -d 'Reconstruct the complete package subtree in
                                      /var/log/setup/tmp/preserved_packages/packagename'
complete -c removepkg -o copy -d 'Construct a copy of the package under
                                  /var/log/setup/tmp/preserved_packages/packagename
                                  but don\'t remove it'
complete -c removepkg -o keep -d 'Save the intermediate files created by removepkg'


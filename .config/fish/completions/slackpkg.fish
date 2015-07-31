# There are more options not handled here.
# Could also extend matchings?

function __fish_using_command
  set cmd (commandline -opc)
  if [ (count $cmd) -gt 1 ]
    if [ $argv[1] = $cmd[2] ]
      return 0
    end
  end
  return 1
end

function __fish_slackpkg_first_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 -a $cmd[1] = 'slackpkg' ]
    return 0
  end
  return 1
end

complete -x -c slackpkg -n '__fish_slackpkg_first_command' -a update -d 'Download and update files and package indexes'
complete -x -c slackpkg -n '__fish_using_command update' -a gpg -d 'Update Slackware\'s GPG key'
complete -x -c slackpkg -n '__fish_slackpkg_first_command' -a check-updates -d 'Check if there is any news on Slackware\'s ChangeLog.txt'

complete -x -c slackpkg -n '__fish_slackpkg_first_command' -a clean-system -d 'Remove all packages which are not present in the official Slackware package set'
complete -x -c slackpkg -n '__fish_slackpkg_first_command' -a upgrade-all -d 'Sync all packages installed in your machine with the selected mirror'
complete -x -c slackpkg -n '__fish_slackpkg_first_command' -a install-new -d 'Install packages which are added to the official Slackware package set'

complete -c slackpkg -n '__fish_slackpkg_first_command' -a install -d 'Download and install packages'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a upgrade -d 'Download and upgrade packages'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a reinstall -d 'Same as install, but for packages already installed'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a remove -d 'Remove installed packages'

complete -c slackpkg -n '__fish_slackpkg_first_command' -a blacklist -d 'Blacklist a package'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a download -d 'Only download (do not install) a package'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a info -d 'Show package information (works with only ONE package)'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a search -d 'Search packages that have a selected name'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a file-search -d 'Search for a specific file in the entire package collection'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a new-config -d 'Search for new configuration files and ask to user what to do with them'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a generate-template -d 'Create a template with all official Slackware packages installed in your machine'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a install-template -d 'Install selected template'
complete -c slackpkg -n '__fish_slackpkg_first_command' -a remove-template -d 'Remove selected template. Be careful.'
complete -x -c slackpkg -n '__fish_slackpkg_first_command' -a help -d 'Show help'


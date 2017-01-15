# Base16 Shell
#eval sh $HOME/.config/base16-shell/base16-default.dark.sh

# Some slackbuild scripts want this
set -x ARCH x86_64

set -x PATH /opt/man-db/bin /opt/man-db/sbin $PATH
set -x PATH $PATH /usr/local/sbin /usr/sbin /sbin
set -x PATH ~/.rakudobrew/bin ~/.cabal/bin $PATH
set -x PATH ~/bin ~/dotfiles/bin $PATH

# For Krita
set -x PATH $PATH /opt/kde4/inst/bin
set -x KDEDIRS /opt/kde4/inst $KDEDIRS

# For java
set -x PATH $PATH /usr/lib64/java/bin

# For rust...?
set -x LD_LIBRARY_PATH /usr/local/lib /usr/local/lib64 /lib /usr/lib /usr/lib64 $LD_LIBRARY_PATH

# Workaround for local timezone discovery
set -x TZ Europe/Stockholm

# git message editor and other things
set -x EDITOR nvim

alias vim nvim
alias g git

# List and sort packages by size
function packages
    fgrep UNCOMPRESSED /var/log/packages/* | awk -F: '{print $3, $1}' | sort -h
end

function reload
    source ~/.config/fish/config.fish
end

function 32dev
  set -x -g ARCH i486
  set -x -g PATH /usr/bin/32 /usr/lib/qt/bin $PATH
  # This is actually the /usr/bin/32/gcc wrapper
  set -x -g CC gcc
  set -x -g CXX g++
  # This is actually the /usr/bin/32/gfortran wrapper
  set -x -g FC gfortran
  set -x -g F77 gfortran

  # Check for Qt4 and the old 'compatibility install' of Qt3
  if test -d /opt/kde3/lib/qt3
    set -x -g QTDIR /opt/kde3/lib/qt3
  end
  if test -d /usr/lib/qt
    set -x -g QT4DIR /usr/lib/qt
  end

  set -x -g LD_LIBRARY_PATH /usr/local/lib /lib /usr/lib $LD_LIBRARY_PATH

  set -x -g PKG_CONFIG_PATH /usr/local/lib/pkgconfig /usr/lib/pkgconfig $PKG_CONFIG_PATH

  echo "ARCH" (echo $ARCH)
end


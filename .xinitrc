#!/bin/sh

# fix broken $UID on some system...
if test "x$UID" = "x"; then
    if test -x /usr/xpg4/bin/id; then
        UID=`/usr/xpg4/bin/id -u`;
    else
        UID=`id -u`;
    fi
fi

# $XDG_CONFIG_HOME defines the base directory relative to which user specific
# configuration files should be stored. If $XDG_CONFIG_HOME is either not set
# or empty, a default equal to $HOME/.config should be used.
if test "x$XDG_CONFIG_HOME" = "x" ; then
  XDG_CONFIG_HOME=$HOME/.config
fi
[ -d "$XDG_CONFIG_HOME" ] || mkdir "$XDG_CONFIG_HOME"

# $XDG_CACHE_HOME defines the base directory relative to which user specific
# non-essential data files should be stored. If $XDG_CACHE_HOME is either not
# set or empty, a default equal to $HOME/.cache should be used.
if test "x$XDG_CACHE_HOME" = "x" ; then
  XDG_CACHE_HOME=$HOME/.cache
fi
[ -d "$XDG_CACHE_HOME" ] || mkdir "$XDG_CACHE_HOME"


# set up XDG user directores.  see
# http://freedesktop.org/wiki/Software/xdg-user-dirs
if type xdg-user-dirs-update >/dev/null 2>&1; then
    xdg-user-dirs-update
fi

if test -f "$XDG_CONFIG_HOME/user-dirs.dirs"; then
    . "$XDG_CONFIG_HOME/user-dirs.dirs"
    # i'm deliberately not 'export'-ing the XDG_ vars, because you shouldn't
    # rely on the env vars inside apps, since the file could be changed at
    # any time by the user.  this is solely here for migration purposes.

    # a bit of user dir migration...
    if test -d "$HOME/Desktop" -a ! -L "$HOME/Desktop" \
            -a "$XDG_DESKTOP_DIR" \
            -a "$HOME/Desktop" != "$XDG_DESKTOP_DIR"
    then
        echo "Migrating $HOME/Desktop to $XDG_DESKTOP_DIR..."
        test -d "$XDG_DESKTOP_DIR" && rmdir "$XDG_DESKTOP_DIR"
        mv "$HOME/Desktop" "$XDG_DESKTOP_DIR" || echo "Desktop migration failed" >&2
    fi

    if test -d "$HOME/Templates" -a ! -L "$HOME/Templates" \
            -a "$XDG_TEMPLATES_DIR" \
            -a "$HOME/Templates" != "$XDG_TEMPLATES_DIR"
    then
        echo "Migrating $HOME/Templates to $XDG_TEMPLATES_DIR..."
        test -d "$XDG_TEMPLATES_DIR" && rmdir "$XDG_TEMPLATES_DIR"
        mv "$HOME/Templates" "$XDG_TEMPLATES_DIR" || echo "Templates migration failed" >&2
    fi
fi


# Modify libglade and glade environment variables so that
# it will find the files installed by Xfce
export LIBGLADE_MODULE_PATH="$LIBGLADE_MODULE_PATH:/usr/lib64/libglade/2.0"
export GLADE_CATALOG_PATH="$GLADE_CATALOG_PATH:"
export GLADE_PIXMAP_PATH="$GLADE_PIXMAP_PATH:"
export GLADE_MODULE_PATH="$GLADE_MODULE_PATH:"


# Export GTK_PATH so that GTK+ can find the Xfce theme engine
export GTK_PATH="$GTK_PATH:/usr/lib64/gtk-2.0"


# For now, start with an empty list
XRESOURCES=""

# Has to go prior to merging Xft.xrdb, as its the "Defaults" file
test -r "/etc/xfce/xdg/xfce4/Xft.xrdb" && XRESOURCES="$XRESOURCES /etc/xfce/xdg/xfce4/Xft.xrdb"
test -r $HOME/.Xdefaults && XRESOURCES="$XRESOURCES $HOME/.Xdefaults"

BASEDIR=$XDG_CONFIG_HOME/xfce4
if test -r "$BASEDIR/Xft.xrdb"; then
  XRESOURCES="$XRESOURCES $BASEDIR/Xft.xrdb"
elif test -r "$XFCE4HOME/Xft.xrdb"; then
  mkdir -p "$BASEDIR"
  cp "$XFCE4HOME/Xft.xrdb" "$BASEDIR"/
  XRESOURCES="$XRESOURCES $BASEDIR/Xft.xrdb"
fi

# merge in X cursor settings
test -r "$BASEDIR/Xcursor.xrdb" && XRESOURCES="$XRESOURCES $BASEDIR/Xcursor.xrdb"

# ~/.Xresources contains overrides to the above
test -r "$HOME/.Xresources" && XRESOURCES="$XRESOURCES $HOME/.Xresources"

# load all X resources (adds /dev/null to avoid an empty list that would hang the process)
cat /dev/null $XRESOURCES | xrdb -nocpp -merge -

# load local modmap
test -r $HOME/.Xmodmap && xmodmap $HOME/.Xmodmap

# Launch xscreensaver (if available), but only as non-root user
if test $UID -gt 0 -a -z "$VNCSESSION"; then
    if test x"`which xscreensaver 2>/dev/null`" != x""; then
        xscreensaver -no-splash &
    elif test x"`which gnome-screensaver 2>/dev/null`" != x""; then
        gnome-screensaver &
    fi
fi

# Use ssh-agent if installed and not already running.  Run it separately
# so it populates the environment here, so we can clean it up later.
sshagent=`which ssh-agent`
kill_sshagent=0
if test -z "$SSH_AGENT_PID" -a "$sshagent" -a "x$sshagent" != "xno"; then
    eval `$sshagent -s`
    kill_sshagent=1
fi

# Use dbus-launch if installed.
if test x"$DBUS_SESSION_BUS_ADDRESS" = x""; then
    dbuslaunch=`which dbus-launch`
    if test x"$dbuslaunch" != x"" -a x"$dbuslaunch" != x"no"; then
        eval `$dbuslaunch --sh-syntax --exit-with-session`
    fi
fi

# Run xfce4-session if installed
xfcesm=`which xfce4-session`
case "x$xfcesm" in
    x|xno*)
        ;;
    *)
        ck-launch-session $xfcesm

        if test $kill_sshagent -eq 1; then
            eval `$sshagent -k`
        fi
        exit 0
        ;;
esac


##################
# IMPORTANT NOTE #
##################

# Everything below here ONLY gets executed if you are NOT using xfce4-session
# (Xfce's session manager).  If you are using the session manager, everything
# below is handled by it, and the code below is not executed at all.  If you're
# not sure if you're using the session manager, type 'ps -e|grep xfce4-session'
# in a terminal while Xfce is running.

##################


# this is only necessary when running w/o xfce4-session
xsetroot -solid black -cursor_name watch

# or use old-fashioned startup script otherwise

xfsettingsd &
xfwm4 --daemon

# start up stuff in $XDG_CONFIG_HOME/autostart/, as that looks to be
# the new standard.  if that directory doesn't exist, try the old
# ~/Desktop/Autostart method.  we're not going to do any migration here.
if test -d "$XDG_CONFIG_HOME/autostart"; then
  for i in ${XDG_CONFIG_HOME}/autostart/*.desktop; do
    grep -q -E "^Hidden=true" "$i" && continue
    if grep -q -E "^OnlyShowIn=" "$i"; then
      # need to test twice, as lack of the line entirely means we still run it
      grep -E "^OnlyShowIn=" "$i" | grep -q 'XFCE;' || continue
    fi
    grep -E "^NotShowIn=" "$i" | grep -q 'XFCE;' && continue

    # check for TryExec
    trycmd=`grep -E "^TryExec=" "$i" | cut -d'=' -f2`
    if test "$trycmd"; then
      type "$trycmd" >/dev/null 2>&1 || continue
    fi

    cmd=`grep -E "^Exec=" "$i" | cut -d'=' -f2`
    if test "$cmd" && type "$cmd" >/dev/null 2>&1; then
      $cmd &
    fi
  done
elif test -d "$HOME/Desktop/Autostart"; then
  for i in `ls -1 -L ${HOME}/Desktop/Autostart/ 2>/dev/null`; do
    if test -x $HOME/Desktop/Autostart/$i; then
      $HOME/Desktop/Autostart/$i &
    fi
  done
fi

xfdesktop&
orage &

panel=`which xfce4-panel`
case "x$panel" in
    x|xno*)
        ;;
    *)
        $panel
        ret=$?
        while test $ret -ne 0; do
            xmessage -center -file - -timeout 20 -title Error <<EOF
A crash occured in the panel
Please report this to the xfce4-dev@xfce.org list
or on http://bugs.xfce.org
Meanwhile the panel will be restarted
EOF
            cat >&2 <<EOF
A crash occured in the panel
Please report this to the xfce4-dev@xfce.org list
or on http://bugs.xfce.org
Meanwhile the panel will be restarted
EOF
            $panel
            ret=$?
        done
        ;;
esac

if test $kill_sshagent -eq 1; then
    eval `$sshagent -k`
fi

xsetroot -bg white -fg red  -solid black -cursor_name watch

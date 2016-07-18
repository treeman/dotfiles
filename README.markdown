My config files for stuff.

Place in ~/dotfiles.

```
ln -s ~/dotfiles/fish/ ~/.config/
ln -s ~/dotfiles/.nvim ~/.config/nvim
ln -s ~/dotfiles/.nvimrc ~/.config/nvim/init.vim
```

And symlink others to ~.

Add `~/dotfiles/bin` to path.

# Different setups

Layouts differ from my portable and different layouts for single and dual screen modes on the desktop. Scripts `dual-screen` and `single-screen` toggle symlinks to `~/.xmonad/xmonad.hs` and `~/.xinitrc`. For portable manually set respective symlinks.

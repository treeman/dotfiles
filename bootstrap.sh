#!/bin/sh

mkdir -p ~/.config
for dir in ~/dotfiles/.config/*
do
    echo $dir
    ln -s $dir ~/.config/
done

ln -s ~/dotfiles/.commitlintrc.json ~/
ln -s ~/dotfiles/.conky ~/
ln -s ~/dotfiles/.dir_colors ~/
ln -s ~/dotfiles/.ghci ~/
ln -s ~/dotfiles/.gitconfig ~/
ln -s ~/dotfiles/.screenrc ~/
ln -s ~/dotfiles/.xmonad ~/
ln -s ~/dotfiles/bin ~/

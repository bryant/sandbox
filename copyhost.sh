#!/bin/sh

HOST=192.168.1.9
DIRS=3rd/z .config .ghc .icewm .terminfo .vim
FILES=.bash_history .bash_logout .bash_profile .bashrc .gdbinit .gitconfig \
    .inputrc .profile .tmux.conf .vimrc .Xdefaults .Xresources

ssh-copy-id bryant@$HOST && \
    rsync -aviz --relative $DIRS $FILES bryant@$HOST:~/

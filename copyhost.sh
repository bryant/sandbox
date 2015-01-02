#!/bin/sh

if [ $# -ne 1 ]
then
    echo "Usage: $0 host_ip"
    exit
fi

HOST=$1
DIRS="3rd/z .config .ghc .icewm .terminfo .vim"
FILES=".bash_history .bash_logout .bash_profile .bashrc .gdbinit .gitconfig \
    .inputrc .profile .tmux.conf .vimrc .Xdefaults .Xresources"

ssh-copy-id bryant@$HOST && \
    rsync -aiz --relative $DIRS $FILES bryant@$HOST:~/

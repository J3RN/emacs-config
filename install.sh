#!/bin/bash

if ! [ -d ~/.emacs.d ]; then
    mkdir ~/.emacs.d
fi

ln -s $(pwd)/init.el        ~/.emacs.d
ln -s $(pwd)/256x256@2x.png ~/.emacs.d
ln -s $(pwd)/themes         ~/.emacs.d
ln -s $(pwd)/games          ~/.emacs.d

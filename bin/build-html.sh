#!/usr/bin/env bash

if [ -z "$2" ]
then
    emacs --batch -l ./build.el  --eval "(to-html \"./user-outlines/$1.org\")"
else
    emacs --batch -l ./build.el  --eval "(to-html \"./user-outlines/$1.org\" \"$2\")"
fi

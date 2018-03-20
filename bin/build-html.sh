#!/usr/bin/env bash

emacs --batch -l ./build.el  --eval '(to-html ./user-outlines/$1.org t)'

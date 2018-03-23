#!/usr/bin/env bash

emacs --batch -l ./build.el  --eval "(to-elisp \"./user-outlines/$1.org\" t)"

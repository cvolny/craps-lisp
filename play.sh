#!/bin/sh

LISP="clisp"

$LISP -q -q -i craps.lisp -x '(play-game)'

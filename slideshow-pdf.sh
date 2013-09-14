#!/bin/bash

DIR=`dirname $1`
BASE=`basename $1 .rkt`
PS="$DIR/$BASE.ps"

~/plt/racket/bin/slideshow --trust -P -o $PS $1
ps2pdf $PS


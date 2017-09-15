#! /bin/bash
<<'COMMENT'
I have created this script because I am too lazy to type all the compression command.
This is a shell script that given the folder name, it will create a .tar.gz with the
files of the folder and will exclude the not wanted files.

Usage:
$ chmod -x compressor_helper.sh
$ ./compressor_helper <FOLDER>

e.g.
$ ./compressor_helper GlaXiFer_P01
COMMENT

RACKET_FILES="$1/*.rkt"
README="$1/readme.txt"
tar cvf "$1.tar.gz" $RACKET_FILES $README

# Check if the tar command succeed
if [ $? -eq 0 ]; then
    echo OK " :)"
else
    echo FAIL " :("
fi



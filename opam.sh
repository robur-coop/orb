#!/bin/sh

#usage: opam config var prefix -> return $PREFIX
#usage: opam config var lib -> return $PREFIX/lib
#usage: opam var prefix -> return $PREFIX
#usage: opam var lib -> return $PREFIX/lib
#usage: opam subst (do nothing)
#usage: opam --version [] -> return 2.1.2

if [ "$1" = "--version" ]; then
    echo "2.1.2";
    exit 0;
fi

if [ $# = 3 ]; then
    if [ "$1" = "config" ]; then
        if [ "$2" = "var" ]; then
            if [ "$3" = "prefix" ]; then
                echo "$PREFIX"
            elif [ "$3" = "lib" ]; then
                echo "$PREFIX/lib"
            else
                exit 1
            fi
        else
            exit 1
        fi
    else
        exit 1
    fi
elif [ $# = 2 ]; then
    if [ "$1" = "var" ]; then
        if [ "$2" = "prefix" ]; then
            echo "$PREFIX"
        elif [ "$2" = "lib" ]; then
            echo "$PREFIX/lib"
        else
            exit 1
        fi
    else
        exit 1
    fi
elif [ $# = 1 ]; then
    if [ "$1" = "subst" ]; then
        exit 0
    else
        exit 1
    fi
else
    exit 1
fi


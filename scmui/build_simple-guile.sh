#! /bin/sh

gcc -o simple-guile simple-guile.c \
    $(pkg-config --cflags --libs guile-2.2)

# now ./simple-guile has (my-username) function

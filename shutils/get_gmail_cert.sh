#! /bin/sh

openssl s_client -connect imap.gmail.com:993 \
        -showcerts \
        2>&1  < /dev/null \
    | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' \
    | head -n 27

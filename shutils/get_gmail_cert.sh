#! /bin/sh

CERT_FILE=~/.gmail.crt
CERT_RES=$(openssl s_client -connect imap.gmail.com:993 \
               -showcerts \
               2>&1  < /dev/null \
           | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' \
           | head -n 27)

if [ $? = 0 ]; then
    echo "saving gmail certificate" \
         "\n$CERT_RES\n" \
         "to $CERT_FILE"
    echo "$CERT_RES" > $CERT_FILE
else
    echo "failed to get gmail cert $CERT_RES" 2>&1
fi

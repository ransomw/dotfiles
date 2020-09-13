#! /usr/local/bin/zsh

CERT_FILE=~/.fastmail.crt

print --- "saving gmail certificate to ${CERT_FILE}"

CERT_RES=$(openssl s_client -connect imap.fastmail.com:993 \
           -showcerts \
           2>&1  < /dev/null \
       |sed \
            '/^-----BEGIN CERTIFICATE-----$/,/^-----END CERTIFICATE-----$/!d')
CERT_STATUS=$?

if [[ $CERT_STATUS = 0 ]]; then
    print -- $CERT_RES > $CERT_FILE
    print -- 'saved certificate'
else
    echo "failed to get gmail cert ${CERT_RES} (${CERT_STATUS})" 2>&1
fi

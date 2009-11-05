#!/bin/sh
umask 077 && touch host.key host.cert host.info host.pem
openssl genrsa 2048 > host.key
openssl req -new -x509 -nodes -sha1 -days 3650 -key host.key > host.cert
openssl x509 -noout -fingerprint -text < host.cert > host.info
cat host.cert host.key > host.pem

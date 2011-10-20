#!/bin/sh

#
# Used to verify that Yaws augments the client response with
# a 302 status code as required by the CGI 1.1 spec when only
# a Location header is returned without a status code.
#
echo 'Location: http://localhost:1234/redirect'
echo
exit 0

%    -*- Erlang -*- 
%    File:        defs.hrl  (/mnt/disk2/jb/work/yaws/yaws/applications/mail/src/defs.hrl)
%    Author:        Johan Bevemyr
%    Created:        Wed Oct 29 23:39:30 2003
%    Purpose:   

-ifndef(DEFS_HRL).
-define(DEFS_HRL, true).


%% Create config file in /etc/mail/yaws-webmail.conf

-record(cfg, {ttl = 1800, % 30 minuter TTL
              popserver = "localhost",
              maildomain = "foo.bar",
              smtpserver = "localhost",
              sendtimeout = 3000}).


-endif.

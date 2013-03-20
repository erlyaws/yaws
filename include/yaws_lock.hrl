-ifndef(_YAWS_LOCK).
-define(_YAWS_LOCK, true).

-define(LOCK_LIFETIME, 900). % lock lifetime in seconds: 15 minutes
-define(CLEANUP_INTERVAL, 60). % cleanup interval in seconds: 1 minute

-record(lock,{
            path=undefined,     % resource path
            id=undefined,       % uid
            owner=anonymous,    % lock owner if submitted
            depth=infinity,     % 0|infinity
            scope=exclusive,    % exclusive|shared
            type=write,         % write
            timeout=0,          % ?LOCK_LIFETIME or shorter
            timestamp=0         % erlang:now()
        }).

-endif.

-ifndef(_YAWS_DAV).
-define(_YAWS_DAV, true).

-define(LOCK_LIFETIME, 300000). % lock lifetime in milliseconds: 5 minutes
-define(CLEANUP_INTERVAL, 60000). % cleanup interval in milliseconds: 1 minute

-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
                                         [?MODULE, ?LINE | Y])).

-record(resource,{
            name,               % normalized name of resource
            info                % file_info record of mapped file
        }).
        
-record(davlock,{
            id=undefined,       % uid
            owner=anonymous,    % lock owner if submitted
            depth=infinity,     % 0|infinity
            scope=exclusive,    % exclusive|shared
            type=write,         % RFC4918 specifies only one type
            timeout=0,          % ?LOCK_LIFETIME or shorter
            timestamp=0         % erlang:now()
        }).

-endif.

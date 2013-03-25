-ifndef(_YAWS_DAV).
-define(_YAWS_DAV, true).

-record(resource,{
            name,               % normalized name of resource
            info                % file_info record of mapped file
        }).
-record(upload, {
            fd,
            tempname,
            filename
        }).
        
-endif.

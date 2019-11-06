%%%----------------------------------------------------------------------
%%% File    : yaws_outmod.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose :
%%% Created :  4 Nov 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-module(yaws_outmod).
-author('klacke@hyber.org').


-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").

-export([out401/3,
         out404/3,
         out404/1,
         out/1,
         crashmsg/3]).


%% The default error 404 error delivery module
%% This function can be used to generate
%% a special page on 404's (it doesn't even have to be a 404)




out404(Arg) ->
    out404(Arg, get(gc), get(sc)).
out404(Arg, GC, SC) ->
    Req = Arg#arg.orig_req,
    {abs_path, Path} = Req#http_request.path,
    B = not_found_body(Path, GC, SC),
    [{status, 404},
     {header, {content_type, "text/html"}},
     {header, {connection, "close"}},
     {html, B}].



%% The default error 401 error delivery module
%% This function can be used to generate
%% a special page on 401's (it doesn't even have to be a 401)
out401(_Arg, _Auth, _Realm) ->
    {ehtml,
     [{html,[],
       [
        {body, [],
         [{h1,[], "401 authentication needed"}
         ]
        }
       ]
      }
     ]
    }.

%% Deprecated, out401/3 will be used
out(_Arg) ->
     {ehtml,
      [{html,[],
        [
         {body, [],
          [{h1,[], "401 authentication needed"}
          ]
         }
        ]
       }
      ]
     }.

not_found_body(Path, _GC, _SC) ->
    L = ["<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
         "<HTML><HEAD>"
         "<TITLE>404 Not Found</TITLE>"
         "</HEAD><BODY>"
         "<H1>Not Found</H1>"
         "The requested URL ",
         yaws_api:htmlize(Path),
         " was not found on this server.<P>"
         "<HR>",
         yaws:address(),
         "  </BODY></HTML>"
        ],
    list_to_binary(L).




%% possibility to customize crash messages,

%% while developing
%% it's extremely convenient to get the crash messages in the browser,
%% however not in production :-)
%% This function can only return an {ehtml, EH} or an {html, HTML}
%% value, no status codes, no headers etc.
crashmsg(_Arg, _SC, L) ->
    %% Hide user/password in auth structures
    RE = "{\"[^\"]+\"\\\s*,\\\s*(md5|ripemd160|sha|sha224|sha256|sha384|sha512)\\\s*,\\\s*[^}]+}",
    L1 = re:replace(L, RE, "#####", [global, noteol, {return, list}]),
    error_logger:format("~s", [L1]),
    {ehtml,
     [{html, [],
       [{body, [],
         [{h2, [], "Internal error, yaws code crashed"},
          {br},
          {hr},
          {pre, [], yaws_api:htmlize(L1)},
          {hr}]
        }
       ]
      }
     ]
    }.

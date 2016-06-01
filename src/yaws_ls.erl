%% -*- coding: latin-1 -*-
%%%----------------------------------------------------------------------
%%% File    : yaws_ls.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose :
%%% Created :  5 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%% Modified: 13 Jan 2004 by Martin Bjorklund <mbj@bluetail.com>
%%% Modified:    Jan 2006 by Sébastien Bigot <sebastien.bigot@tremplin-utc.net>
%%%----------------------------------------------------------------------

-module(yaws_ls).
-author('klacke@hyber.org').

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

-include_lib("kernel/include/file.hrl").
-export([list_directory/6, out/1]).

%% Exports for EUNIT.
-export([parse_query/1, trim/2]).

-define(FILE_LEN_SZ, 45).

list_directory(_Arg, CliSock, List, DirName, Req, DoAllZip) ->
    {abs_path, Path} = Req#http_request.path,
    {DirStr, Pos, Direction, Qry} = parse_query(Path),
    ?Debug("List=~p Dirname~p~n", [List, DirName]),

    Descriptions = read_descriptions(DirName),

    L0 = lists:zf(
           fun(F) ->
                   File = DirName ++ [$/|F],
                   FI = file:read_file_info(File),
                   file_entry(FI, DirName, F, Qry,Descriptions)
           end, List),

    L1 = lists:keysort(Pos, L0),

    L2 = if Direction == normal -> L1;
            Direction == reverse -> lists:reverse(L1)
         end,

    L3 = [Html || {_, _, _, _, Html} <- L2],

    Body = [ doc_head(DirStr),
             dir_header(DirName,DirStr),
             table_head(Direction),
             parent_dir(),
             if
                 DoAllZip == true ->
                     allzip();
                 DoAllZip == true_nozip ->
                     [];
                 true ->
                     []
             end,

             %%              if DoAllGZip == true -> alltgz() end,
             %%              if DoAllBZip2 == true -> alltbz2() end,

             %%              if DoAllZip == true -> alltgz() end,
             %%              if DoAllZip == true -> alltbz2() end,

             L3,
             table_tail(),
             dir_footer(DirName),%yaws:address(),
             doc_tail()
           ],

    B = unicode:characters_to_binary(Body),

    %% Always use UTF-8 encoded file names. So, set the UTF-8 charset in the
    %% Content-Type header
    NewCT = case yaws:outh_get_content_type() of
                undefined ->
                    "text/html; charset=utf-8";
                CT0 ->
                    [CT|_] = yaws:split_sep(CT0, $;),
                    CT++"; charset=utf-8"
            end,
    yaws:outh_set_content_type(NewCT),

    yaws_server:accumulate_content(B),
    yaws_server:deliver_accumulated(CliSock),
    yaws_server:done_or_continue().

parse_query(Path) ->
    case string:tokens(Path, [$?]) of
        [DirStr, [PosC, $=, DirC] = Q] ->
            Pos = case PosC of
                      $m -> 2;
                      $M -> 2; % last modified
                      $s -> 3;
                      $S -> 3; % size
                      $d -> 4;
                      $D -> 4; % description
                      _  -> 1  % name (default)
                  end,
            Dir = case DirC of
                      $r -> reverse;
                      _  -> normal
                  end,
            {DirStr, Pos, Dir, "/?"++Q};
        _ ->
            {Path, 1, normal, "/"}
    end.

parse_description(Line) ->
    L = string:strip(Line),
    Pos = string:chr(L,$ ),
    Filename = string:substr(L, 1, Pos-1),
    D = string:substr(L,Pos+1),
    Description = string:strip(D,left),
    {Filename,Description}.

read_descriptions(DirName) ->
    File = filename:join(DirName, "MANIFEST.txt"),
    case file:read_file(File) of
        {ok,Bin} -> Lines = string:tokens(binary_to_list(Bin),"\n"),
                    lists:map(fun parse_description/1,Lines);
        _ -> []
    end.

get_description(Name,Descriptions) ->
    case lists:keysearch(Name,1,Descriptions) of
        {value, {_,Description}} -> Description;
        _ -> []
    end.

doc_head(DirName) ->
    EncDirName = file_display_name(yaws_api:url_decode(DirName)),
    HtmlDirName = yaws_api:htmlize(EncDirName),
    ?F("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
       "<html>\n"
       " <head>\n"
       "  <meta charset=\"utf-8\">"
       "  <title>Index of ~ts</title>\n"
       "  <style type=\"text/css\">\n"
       "    img { border: 0; padding: 0 2px; vertical-align: text-bottom; }\n"
       "    td  { font-family: monospace; padding: 2px 3px; text-align:left;\n"
       "          vertical-align: bottom; white-space: pre; }\n"
       "    td:first-child { text-align: left; padding: 2px 10px 2px 3px; }\n"
       "    table { border: 0; }\n"
       "  </style>\n"
       "</head> \n"
       "<body>\n",
       [list_to_binary(HtmlDirName)]
      ).

doc_tail() ->
    "</body>\n"
        "</html>\n".

table_head(Direction) ->
    NextDirection = if Direction == normal  -> "r";
                       Direction == reverse -> "n"
                    end,
    ["<table>\n"
     "  <tr>\n"
     "    <td><img src=\"/icons/blank.gif\" alt=\"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\"/><a href=\"?N=",NextDirection,"\">Name</a></td>\n"
     "    <td><a href=\"?M=",NextDirection,"\">Last Modified</a></td>\n"
     "    <td><a href=\"?S=",NextDirection,"\">Size</a></td>\n"
     "    <td><a href=\"?D=",NextDirection,"\">Description</a></td>\n"
     "  </tr>\n"
     "  <tr><th colspan=\"4\"><hr/></th></tr>\n"].

table_tail() ->
    "  <tr><th colspan=\"4\"><hr/></th></tr>\n"
        "</table>\n".


dir_footer(DirName) ->
    File = DirName ++ [$/ | "README.txt"],
    case file:read_file(File) of
        {ok,Bin} -> "<pre>\n" ++ binary_to_list(Bin) ++ "</pre>\n";
        _ -> yaws:address()
    end.

dir_header(DirName,DirStr) ->
    File = DirName ++ [$/ | "HEADER.txt"],
    case file:read_file(File) of
        {ok,Bin} ->
            "<pre>\n" ++ binary_to_list(Bin) ++ "</pre>\n";
        _ ->
            EncDirStr   = file_display_name(yaws_api:url_decode(DirStr)),
            HtmlDirName = yaws_api:htmlize(EncDirStr),
            ?F("<h1>Index of ~ts</h1>\n", [list_to_binary(HtmlDirName)])
    end.

parent_dir() ->
    {Gif, Alt} = list_gif(directory,"."),
    ?F("  <tr>\n"
       "    <td><img src=~p alt=~p/><a href=\"..\">Parent Directory</a></td>\n"
       "    <td></td>\n"
       "    <td>-</td>\n"
       "    <td></td>\n"
       "  </tr>\n",
       ["/icons/" ++ Gif,
        Alt
       ]).

%% FIXME: would be nice with a good size approx.  but it would require
%% a deep scan of possibly the entire docroot, (and also some knowledge
%% about zip's compression ratio in advance...)
allzip() ->
    {Gif, Alt} = list_gif(zip,""),
    ?F("  <tr>\n"
       "    <td><img src=~p alt=~p/><a href=\"all.zip\">all.zip</a></td>\n"
       "    <td></td>\n"
       "    <td>-</td>\n"
       "    <td>Build a zip archive of current directory</td>\n"
       "  </tr>\n",
       ["/icons/" ++ Gif,
        Alt]).

%% alltgz() ->
%%    {Gif, Alt} = list_gif(zip,""),
%%    ?F("  <tr>\n"
%%       "    <td><img src=~p alt=~p/><a href=\"all.tgz\">all.tgz</a></td>\n"
%%       "    <td></td>\n"
%%       "    <td>-</td>\n"
%%       "    <td>Build a gzip archive of current directory</td>\n"
%%       "  </tr>\n",
%%       ["/icons/" ++ Gif,
%%        Alt]).

%% alltbz2() ->
%%    {Gif, Alt} = list_gif(zip,""),
%%    ?F("  <tr>\n"
%%       "    <td><img src=~p alt=~p/><a href=\"all.tbz2\">all.tbz2</a></td>\n"
%%       "    <td></td>\n"
%%       "    <td>-</td>\n"
%%       "    <td>Build a bzip2 archive of current directory</td>\n"
%%       "  </tr>\n",
%%       ["/icons/" ++ Gif,
%%        Alt]).

is_user_dir(SP) ->
    case SP of
        [$/,$~ | T] -> User = string:sub_word(T,1,$/),
                       case catch yaws:user_to_home(User) of
                           {'EXIT', _} ->
                               false;
                           Home ->
                               {true,Home}
                       end;
        _ -> false
    end.

out(A) ->
    SP = A#arg.server_path,
    PP = A#arg.appmod_prepath,
    Dir = case is_user_dir(SP) of
              {true,Home} -> Home ++ "/public_html";
              false -> A#arg.docroot
          end ++ PP,

    %%    {html,?F("<h2>~p</h2>",[Dir])}.

    YPid = self(),

    Forbidden_Paths = accumulate_forbidden_paths(),
    case filename:basename(A#arg.server_path) of
        "all.zip" -> spawn_link(fun() -> zip(YPid, Dir, Forbidden_Paths) end),
                     {streamcontent, "application/zip", ""}
                     %%        "all.tgz" -> spawn_link(fun() -> tgz(YPid, Dir) end),
                     %%                     {streamcontent, "application/gzip", ""};
                     %%        "all.tbz2" -> spawn_link(fun() -> tbz2(YPid, Dir) end),
                     %%                     {streamcontent, "application/gzip", ""}
    end.


generate_random_fn() ->
    Bytes = try yaws_dynopts:rand_bytes(64) of
                B when is_bitstring(B) ->
                    B
            catch _:_ ->
                    %% for installations without crypto
                    << <<(yaws_dynopt:random_uniform(256) - 1)>> || _ <- lists:seq(1,64) >>
            end,
    << Int:512/unsigned-big-integer >> = << Bytes/binary >>,
    integer_to_list(Int).

mktempfilename([]) ->
    {error, no_temp_dir};
mktempfilename([Dir|R]) ->
    RandomFN = generate_random_fn(),
    Filename = filename:join(Dir, RandomFN),
    case file:open(Filename, [write]) of
        {ok, FileHandle} ->
            {ok, {Filename, FileHandle}};
        _Else ->
            mktempfilename(R)
    end.

mktempfilename() ->
    %% TODO: Add code to determine the temporary directory on various
    %% operating systems.
    PossibleDirs = ["/tmp", "/var/tmp"],
    mktempfilename(PossibleDirs).

zip(YPid, Dir, ForbiddenPaths) ->
    {ok, RE_ForbiddenNames} = re:compile("\\.yaws\$", [unicode]),
    Files = dig_through_dir(Dir, ForbiddenPaths, RE_ForbiddenNames),
    {ok, {Tempfile, TempfileH}} = mktempfilename(),
    file:write(TempfileH, lists:foldl(fun(I, Acc) ->
                                              [Acc, list_to_binary(file_display_name(I)), "\n"]
                                      end, [], Files)),
    file:close(TempfileH),
    process_flag(trap_exit, true),
    %% TODO: find a way to directly pass the list of files to
    %% zip. Erlang ports do not allow stdin to be closed
    %% independently; however, zip needs stdin to be closed as an
    %% indicator that the list of files is complete.
    P = open_port({spawn, "zip -q -1 - -@ < " ++ Tempfile},
                  [{cd, Dir},use_stdio, binary, exit_status]),
    F = fun() ->
                file:delete(Tempfile)
        end,
    stream_loop(YPid, P, F).

accumulate_forbidden_paths() ->
    SC = get(sc),
    Auth = SC#sconf.authdirs,
    lists:foldl(fun({Path, _Auth}, Acc) ->
                        Acc ++ [Path]
                end, [], Auth).


%% tgz(YPid, Dir) ->
%%    process_flag(trap_exit, true),
%%    P = open_port({spawn, "tar cz ."},
%%                  [{cd, Dir},use_stdio, binary, exit_status]),
%%    stream_loop(YPid, P).

%% tbz2(YPid, Dir) ->
%%     process_flag(trap_exit, true),
%%     P = open_port({spawn, "tar cj ."},
%%                   [{cd, Dir},use_stdio, binary, exit_status]),
%%     stream_loop(YPid, P).

dir_contains_indexfile(_Dir, []) ->
    false;
dir_contains_indexfile(Dir, [File|R]) ->
    case file:read_file_info(filename:join(Dir, File)) of
        {ok, _} ->
            true;
        _Else ->
            dir_contains_indexfile(Dir, R)
    end.

dir_contains_indexfile(Dir) ->
    Indexfiles = [".yaws.auth", "index.yaws", "index.html", "index.htm"],
    dir_contains_indexfile(Dir, Indexfiles).

dig_through_dir(Basedirlen, Dir, ForbiddenPaths, RE_ForbiddenNames) ->
    Dir1 = string:sub_string(Dir, Basedirlen),
    case {lists:member(Dir1, ForbiddenPaths),
          dir_contains_indexfile(Dir)} of
        {true,_} ->
            [];
        {_,true} ->
            [];
        {false, false} ->
            {ok, Files} = file:list_dir(Dir),
            lists:foldl(fun(I, Acc) ->
                                Filename = filename:join(Dir, I),
                                case {file:read_file_info(Filename),
                                      re:run(Filename, RE_ForbiddenNames)} of
                                    {_, {match, _}} ->
                                        Acc;
                                    {{ok, #file_info{type=directory}}, _} ->
                                        Acc ++ dig_through_dir(
                                                 Basedirlen,
                                                 Filename,
                                                 ForbiddenPaths,
                                                 RE_ForbiddenNames);
                                    {{ok, #file_info{type=regular}}, _} ->
                                        Acc ++ [string:sub_string(
                                                  Filename, Basedirlen)];
                                    _Else ->
                                        Acc %% Ignore other files
                                end
                        end, [], Files)
    end.

dig_through_dir(Dir, ForbiddenPaths, RE_ForbiddenNames) ->
    dig_through_dir(length(Dir) + 1,
                    Dir,
                    ForbiddenPaths,
                    RE_ForbiddenNames).

stream_loop(YPid, P, FinishedFun) ->
    receive
        {P, {data, Data}} ->
            yaws_api:stream_chunk_deliver_blocking(YPid, Data),
            stream_loop(YPid, P, FinishedFun);
        {P, {exit_status, _}} ->
            yaws_api:stream_chunk_end(YPid),
            FinishedFun();
        {'EXIT', YPid, Status} ->
            FinishedFun(),
            exit(Status);
        Else ->
            FinishedFun(),
            error_logger:error_msg("Could not deliver zip file: ~p\n", [Else])
    end.

file_entry({ok, FI}, _DirName, Name, Qry, Descriptions) ->
    ?Debug("file_entry(~p) ", [Name]),
    Ext = filename:extension(Name),
    {Gif, Alt} = list_gif(FI#file_info.type, Ext),
    QryStr = if FI#file_info.type == directory -> Qry;
                true -> ""
             end,

    EncName  = file_display_name(Name),
    Description = get_description(Name,Descriptions),

    Entry =
        ?F("  <tr>\n"
           "    <td><img src=~p alt=~p/><a href=~p title=\"~ts\">~ts</a></td>\n"
           "    <td>~s</td>\n"
           "    <td>~s</td>\n"
           "    <td>~s</td>\n"
           "  </tr>\n",
           ["/icons/" ++ Gif,
            Alt,
            yaws_api:url_encode(Name) ++ QryStr,
            list_to_binary(EncName),
            list_to_binary(trim(EncName,?FILE_LEN_SZ)),
            datestr(FI),
            sizestr(FI),
            Description]),
    ?Debug("Entry:~p", [Entry]),

    {true, {EncName, FI#file_info.mtime, FI#file_info.size, Description, Entry}};

file_entry(_Err, _, _Name, _, _) ->
    ?Debug("no entry for ~p: ~p", [_Name, _Err]),
    false.

trim(L,N) ->
    trim(L,N,[]).
trim([_H1,_H2,_H3]=[H|T], 3=I, Acc) ->
    trim(T, I-1, [H|Acc]);
trim([H1,H2,H3|_T], 3=_I, Acc) when H1 < 128, H2 < 128, H3 < 128 ->
    lists:reverse(Acc) ++ "..&gt;";
trim([H1,H2,H3|_T], 3=_I, [H0|Acc]) ->
    %% Drop UTF8 continuation bytes: 10xxxxxx
    Hs0 = lists:dropwhile(fun(Byte) -> Byte bsr 6 == 2#10 end, [H3,H2,H1,H0]),
    %% Drop UTF8 leading byte: 11xxxxxx
    Hs = lists:dropwhile(fun(Byte) -> Byte bsr 6 == 2#11 end, Hs0),
    lists:reverse(Hs++Acc) ++ "..&gt;";
trim([H|T], I, Acc) ->
    trim(T, I-1, [H|Acc]);
trim([], _I, Acc) ->
    lists:reverse(Acc).

%% FI -> 16-Jan-2006 23:06
datestr(FI) ->
    {{Year, Month, Day}, {Hour, Min, _}} = FI#file_info.mtime,
    io_lib:format("~s-~s-~w ~s:~s",
                  [yaws:mk2(Day),yaws:month(Month),Year,
                   yaws:mk2(Hour),yaws:mk2(Min)]).

sizestr(FI) when FI#file_info.size > 1000000 ->
    ?F("~.1fM", [FI#file_info.size / 1000000]);
sizestr(FI) when FI#file_info.size > 1000 ->
    ?F("~wk", [trunc(FI#file_info.size / 1000)]);
sizestr(FI) when FI#file_info.size == 0 ->
    ?F("0k", []);
sizestr(_FI) ->
    ?F("1k", []). % As apache does it...

list_gif(directory, ".") ->
    {"back.gif", "[DIR]"};
list_gif(regular, ".txt") ->
    {"text.gif", "[TXT]"};
list_gif(regular, ".c") ->
    {"c.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(regular, ".dvi") ->
    {"dvi.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(regular, ".pdf") ->
    {"pdf.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(regular, _) ->
    {"layout.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(directory, _) ->
    {"dir.gif", "[DIR]"};
list_gif(zip, _) ->
    {"compressed.gif", "[DIR]"};
list_gif(_, _) ->
    {"unknown.gif", "[OTH]"}.


%% Assume that all file names are UTF-8 encoded. If the VM uses ISO-latin-1
%% encoding, then no conversion is needed (file already returns the byte
%% representation of file names). If the VM uses UTF-8, we need to do a little
%% conversion to return the byte representation of file names.
file_display_name(Name) ->
    case file:native_name_encoding() of
        latin1 -> Name;
        utf8   -> binary_to_list(unicode:characters_to_binary(Name))
    end.

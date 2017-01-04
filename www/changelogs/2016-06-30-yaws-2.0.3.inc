<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/25bd6ba" target="_blank">25bd6ba</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-25bd6ba');">
    Preparing for rel 2.0.3
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-25bd6ba');">
    // Klacke
  </div>
  <div id="msg-25bd6ba" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-25bd6ba');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Thu Jun 30 18:10:56 2016</strong>

Preparing for rel 2.0.3
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3f1c71f" target="_blank">3f1c71f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3f1c71f');">
    Manage modules/functions deprecated in Erlang/OTP 19.0
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3f1c71f');">
    // Christopher Faulet
  </div>
  <div id="msg-3f1c71f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3f1c71f');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Jun 30 14:38:54 2016</strong>

Manage modules/functions deprecated in Erlang/OTP 19.0

'crypto:rand_bytes/1' and the 'random' module have been deprecated in Erlang/OTP
19.0. Instead, we must use, respectively, 'crypto:strong_rand_bytes' and the
'rand' module.

Fix #277
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8099e42" target="_blank">8099e42</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8099e42');">
    Remove unneeded binding in yaws_websocket:send/2 call
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8099e42');">
    // Steve Vinoski
  </div>
  <div id="msg-8099e42" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8099e42');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Jun 19 02:45:55 2016</strong>

Remove unneeded binding in yaws_websocket:send/2 call

In yaws_api.erl, websocket_send was calling yaws_websocket:send/2 with
a second argument of #ws_frame{}=Frame instead of just Frame.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bd4f262" target="_blank">bd4f262</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bd4f262');">
    Merge pull request #275 from dvaergiller/soap_srv_handle_request_concurrent
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bd4f262');">
    // Claes Wikstrom
  </div>
  <div id="msg-bd4f262" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bd4f262');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Jun 9 15:07:03 2016</strong>

Merge pull request #275 from dvaergiller/soap_srv_handle_request_concurrent

Soap srv handle request concurrent
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d250952" target="_blank">d250952</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d250952');">
    Make ways_soap_srv handle requests in parallel
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d250952');">
    // Mattias Jansson
  </div>
  <div id="msg-d250952" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d250952');">
    <pre>
<strong>Author: Mattias Jansson &lt;janmattias@gmail.com&gt;</strong>
<strong>Date:   Tue May 10 15:17:47 2016</strong>

Make ways_soap_srv handle requests in parallel

This change makes the yaws_soap_srv:handler/4 function call the
request/5 function directly, instead of having the gen_server do
that. This avoids blocking the gen_server when handling requests and
should provide much better parallelism.

The gen_server now only manages WSDL models. The handler function will
call the gen_server in order to retrieve the model. The rest should now
be executed in the regular Yaws workers.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c037acb" target="_blank">c037acb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c037acb');">
    Fix a infinite loop when a client sends a request on "." or "./some-path"
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c037acb');">
    // Christopher Faulet
  </div>
  <div id="msg-c037acb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c037acb');">
    <pre>
<strong>Author: Christopher Faulet &lt;cfaulet@qualys.com&gt;</strong>
<strong>Date:   Thu May 12 11:04:25 2016</strong>

Fix a infinite loop when a client sends a request on "." or "./some-path"

When a client sends a request on "." or "./some-path", Yaws will loop
infinitly. This is reproducable by using a catch-all appmod (on "/")

The loop can occur in "yaws_server:is_revproxy/3",
"yaws_server:is_redirect_map/2" or "yaws_server:filter_auths/2", depending on
the configuration. In these functions there is a check on the path to return
when it is equal to "/". We must do the same when the path is ".".

This patch fixes #271.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/92ec1fc" target="_blank">92ec1fc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-92ec1fc');">
    Merge pull request #268 from matthiasl/ml_auth_in_ets
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-92ec1fc');">
    // Claes Wikstrom
  </div>
  <div id="msg-92ec1fc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-92ec1fc');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu May 12 10:22:19 2016</strong>

Merge pull request #268 from matthiasl/ml_auth_in_ets

Move username/password tuples from server state to an ETS table
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5f188bf" target="_blank">5f188bf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5f188bf');">
    Add yaws_api:websocket_send for ws_state records
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5f188bf');">
    // Steve Vinoski
  </div>
  <div id="msg-5f188bf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5f188bf');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 7 14:29:13 2016</strong>

Add yaws_api:websocket_send for ws_state records

Yaws documentation has been instructing users to call
yaws_api:websocket_send with a ws_state record as the first argument,
but there was no clause of that function supporting this. Add two new
clauses of yaws_api:websocket_send to fix this problem. Change
basic_echo_callback_extended example to call yaws_api:websocket_send
instead of yaws_websockets:send.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bcdbd78" target="_blank">bcdbd78</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bcdbd78');">
    Add Erlang/OTP 18.3 support in .travis.yml
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bcdbd78');">
    // Steve Vinoski
  </div>
  <div id="msg-bcdbd78" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bcdbd78');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri May 6 15:40:55 2016</strong>

Add Erlang/OTP 18.3 support in .travis.yml
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/671cbc2" target="_blank">671cbc2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-671cbc2');">
    Merge pull request #269 from leoliu/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-671cbc2');">
    // Steve Vinoski
  </div>
  <div id="msg-671cbc2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-671cbc2');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu May 5 16:28:13 2016</strong>

Merge pull request #269 from leoliu/master

Don't crash on unknown query string in yaws_ls:parse_query/1
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ea103ac" target="_blank">ea103ac</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ea103ac');">
    Don't crash on unknown query string in yaws_ls:parse_query/1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ea103ac');">
    // Leo Liu
  </div>
  <div id="msg-ea103ac" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ea103ac');">
    <pre>
<strong>Author: Leo Liu &lt;sdl.web@gmail.com&gt;</strong>
<strong>Date:   Thu May 5 13:56:18 2016</strong>

Don't crash on unknown query string in yaws_ls:parse_query/1

and also make it case-insensitive.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ed00ae4" target="_blank">ed00ae4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ed00ae4');">
    Fix json parsing for UTF-8 strings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ed00ae4');">
    // Christopher Faulet
  </div>
  <div id="msg-ed00ae4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ed00ae4');">
    <pre>
<strong>Author: Christopher Faulet &lt;cfaulet@qualys.com&gt;</strong>
<strong>Date:   Wed Apr 27 16:42:31 2016</strong>

Fix json parsing for UTF-8 strings

Surrogate pairs were not handled during the string decoding and triggered an
exception when encountered. This bug was reported in the issue #264.

Some improvements have been made to sanitize error handling. And unit tests
about json parsing have been added (tests have been retrieved from jiffy and
jansson erlang projects).
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1465a76" target="_blank">1465a76</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1465a76');">
    Improve yaws_ls:trim/3 to trim at UTF8 char boundary
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1465a76');">
    // Leo Liu
  </div>
  <div id="msg-1465a76" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1465a76');">
    <pre>
<strong>Author: Leo Liu &lt;sdl.web@gmail.com&gt;</strong>
<strong>Date:   Mon Apr 25 07:28:56 2016</strong>

Improve yaws_ls:trim/3 to trim at UTF8 char boundary
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f9795f1" target="_blank">f9795f1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f9795f1');">
    Password can contain colon fixed.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f9795f1');">
    // Klacke Wikstrom
  </div>
  <div id="msg-f9795f1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f9795f1');">
    <pre>
<strong>Author: Klacke Wikstrom &lt;cwikstro@cisco.com&gt;</strong>
<strong>Date:   Thu Apr 28 16:38:20 2016</strong>

Password can contain colon fixed.

Issue reported in https://github.com/klacke/yaws/issues/267
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fe20983" target="_blank">fe20983</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fe20983');">
    Merge pull request #266 from matthiasl/ml_docfixes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fe20983');">
    // Claes Wikstrom
  </div>
  <div id="msg-fe20983" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fe20983');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Apr 27 23:16:15 2016</strong>

Merge pull request #266 from matthiasl/ml_docfixes

Ml docfixes
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0e48053" target="_blank">0e48053</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0e48053');">
    Fix incomplete and outdated documentation of .yaws_auth
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0e48053');">
    // Matthias Lang
  </div>
  <div id="msg-0e48053" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0e48053');">
    <pre>
<strong>Author: Matthias Lang &lt;matthias@corelatus.se&gt;</strong>
<strong>Date:   Wed Apr 27 15:17:22 2016</strong>

Fix incomplete and outdated documentation of .yaws_auth

Previously, the yaws documentation recommended storing username/password
pairs in .yaws_auth, in the docroot. This is a bad idea because it can
result in exposing plaintext username/password via editor backup files.

Previously, the yaws documentation was missing documentation for the
realm, pam, authmod, file, allow, deny and order directives in .yaws_auth.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0ef2dbf" target="_blank">0ef2dbf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0ef2dbf');">
    Fix documentation error: yaws unpacks to a directory with a version
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0ef2dbf');">
    // Matthias Lang
  </div>
  <div id="msg-0ef2dbf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0ef2dbf');">
    <pre>
<strong>Author: Matthias Lang &lt;matthias@corelatus.se&gt;</strong>
<strong>Date:   Mon May 4 10:41:08 2015</strong>

Fix documentation error: yaws unpacks to a directory with a version
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/37f44d0" target="_blank">37f44d0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-37f44d0');">
    Move username/password tuples from server state to an ETS table
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-37f44d0');">
    // Matthias Lang
  </div>
  <div id="msg-37f44d0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-37f44d0');">
    <pre>
<strong>Author: Matthias Lang &lt;matthias@corelatus.se&gt;</strong>
<strong>Date:   Wed Apr 20 22:53:03 2016</strong>

Move username/password tuples from server state to an ETS table

Previously, basic authentication in YAWS stored {Username, Password}
tuples as a list in the server state. By default, a crash in YAWS
generates a crash dump which is served over HTTP, thus remotely
exposing all passwords and usernames in plaintext.

This patch moves the {Username, Password} to an ETS table so that
sensitive information is kept on the server rather than leaked.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8c0773f" target="_blank">8c0773f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8c0773f');">
    Fix yaws_api:url_encode/1 to handle unicode chars
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8c0773f');">
    // Leo Liu
  </div>
  <div id="msg-8c0773f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8c0773f');">
    <pre>
<strong>Author: Leo Liu &lt;sdl.web@gmail.com&gt;</strong>
<strong>Date:   Fri Apr 15 08:03:46 2016</strong>

Fix yaws_api:url_encode/1 to handle unicode chars
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/60101e7" target="_blank">60101e7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-60101e7');">
    Use id to build the command line in the windows startup script
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-60101e7');">
    // Christopher Faulet
  </div>
  <div id="msg-60101e7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-60101e7');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Apr 5 22:26:04 2016</strong>

Use id to build the command line in the windows startup script

This fixes a warning about an unused variable.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bb547e7" target="_blank">bb547e7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bb547e7');">
    Do not use erlang:now/0 anymore in Yaws applications
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bb547e7');">
    // Christopher Faulet
  </div>
  <div id="msg-bb547e7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bb547e7');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Apr 5 22:02:22 2016</strong>

Do not use erlang:now/0 anymore in Yaws applications

erlang:now/1 was deprecated in Erlang/OTP 18.0. We rely on yaws:get_time_tuple/0
instead.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/49fb138" target="_blank">49fb138</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-49fb138');">
    Fix typo in documentation & manpages
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-49fb138');">
    // Christopher Faulet
  </div>
  <div id="msg-49fb138" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-49fb138');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Apr 5 21:53:32 2016</strong>

Fix typo in documentation & manpages
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e6ffea1" target="_blank">e6ffea1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e6ffea1');">
    Move Erlang compatibily checks from the configure to a module
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e6ffea1');">
    // Christopher Faulet
  </div>
  <div id="msg-e6ffea1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e6ffea1');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Apr 1 16:12:34 2016</strong>

Move Erlang compatibily checks from the configure to a module

Now, the new module yaws_dynopts does all tests on the Erlang/OTP release that
were done before in the configure. So it is possible to compile Yaws with an
Erlang/OTP release and run it with another, all features should be dynamically
detected.

Because these checks can be expensive, when Yaws is started, we generate and
compile a static version of yaws_dynopts.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b9df33b" target="_blank">b9df33b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b9df33b');">
    Fix the values of the SC flags to have successive powers of 2
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b9df33b');">
    // Christopher Faulet
  </div>
  <div id="msg-b9df33b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b9df33b');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Mar 31 17:17:39 2016</strong>

Fix the values of the SC flags to have successive powers of 2
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2b7b584" target="_blank">2b7b584</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2b7b584');">
    Remove doc about deprecated values for the SSL option 'verify'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2b7b584');">
    // Christopher Faulet
  </div>
  <div id="msg-2b7b584" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2b7b584');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Mar 31 17:08:52 2016</strong>

Remove doc about deprecated values for the SSL option 'verify'

In the old SSL implentation, 0,1 and 2 were the possible values for the 'verify'
option. For the new one, only verify_peer and verify_none are officialy
supported and documentated. Old values are still supported for backward
compatiliby, but not documented.

We do the same in Yaws. Old values are translated into new ones. So if their
support is removed from the SSL application in a futur Erlang/OTP release, Yaws
will still work. Documentation has been updated to remove info about the old
values.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1fe8f4d" target="_blank">1fe8f4d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1fe8f4d');">
    Remove very old use_old_ssl directive
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1fe8f4d');">
    // Christopher Faulet
  </div>
  <div id="msg-1fe8f4d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1fe8f4d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Mar 31 16:34:46 2016</strong>

Remove very old use_old_ssl directive

This directive was added to use old SSL implementation. It was removed in
Erlang/OTP R15. So, because Yaws works with releases upper to R14B02, this
option is only supported with Erlang/OTP R14B02, R14B03 and R14B04. For all
other supported releases, Yaws will fails is use_old_ssl is set to true.

To be honest, if you use SSL in Yaws, you'd better upgrade your Erlang/OTP
release. The new SSL implementation is far better than the old one and many bugs
are fixed in recent releases.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a2e4fb4" target="_blank">a2e4fb4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a2e4fb4');">
    Add SNI support
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a2e4fb4');">
    // Christopher Faulet
  </div>
  <div id="msg-a2e4fb4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a2e4fb4');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Mar 31 15:03:58 2016</strong>

Add SNI support

SSL application supports SNI since Erlang/OTP 18.0. With this patch, when SNI is
available, Yaws is able tohandle SNI clients if it is configured to do it.

To configure SNI, 2 new directives has been added:

 * sni: this global directive can be use to enable SNI or explicitly disable
   it. By default SNI support is disabled. It can also be used to globally
   refuse non SNI clients or not.

 * require_sni: this server directive can be used to reject non SNI clients and
   clients providing an unknown SNI hostname for a specific server.

See man/yaws.conf.5 for details.
Documentation has been updated and a testsuite has been added.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/536462f" target="_blank">536462f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-536462f');">
    Define canned sequence to easy test scripting in Makefiles
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-536462f');">
    // Christopher Faulet
  </div>
  <div id="msg-536462f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-536462f');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Mar 31 14:59:26 2016</strong>

Define canned sequence to easy test scripting in Makefiles
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/20dc4d3" target="_blank">20dc4d3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-20dc4d3');">
    Add Erlang/OTP 18.2 support in .travis.yml
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-20dc4d3');">
    // Christopher Faulet
  </div>
  <div id="msg-20dc4d3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-20dc4d3');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Mar 31 14:50:09 2016</strong>

Add Erlang/OTP 18.2 support in .travis.yml
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/521d726" target="_blank">521d726</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-521d726');">
    Unlink the stat process before stopping it to not receive 'EXIT' message
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-521d726');">
    // Christopher Faulet
  </div>
  <div id="msg-521d726" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-521d726');">
    <pre>
<strong>Author: Christopher Faulet &lt;cfaulet@qualys.com&gt;</strong>
<strong>Date:   Wed Apr 13 11:02:23 2016</strong>

Unlink the stat process before stopping it to not receive 'EXIT' message

This message was wrongly associated to the death of an acceptor. So the sessions
counter was wrongly decremented. Instead of checking the Pid in 'EXIT' messages
(costly), we unlink the stat process.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c38975a" target="_blank">c38975a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c38975a');">
    Merge pull request #259 from leoliu/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c38975a');">
    // Steve Vinoski
  </div>
  <div id="msg-c38975a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c38975a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Apr 1 05:23:31 2016</strong>

Merge pull request #259 from leoliu/master

Fix coding cookies in a few files
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9fc90b6" target="_blank">9fc90b6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9fc90b6');">
    Fix coding cookies in a few files
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9fc90b6');">
    // Leo Liu
  </div>
  <div id="msg-9fc90b6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9fc90b6');">
    <pre>
<strong>Author: Leo Liu &lt;sdl.web@gmail.com&gt;</strong>
<strong>Date:   Fri Apr 1 04:52:38 2016</strong>

Fix coding cookies in a few files

Coding Latin-1 (with capital `L') is not recognized by Emacs.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7b385ad" target="_blank">7b385ad</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7b385ad');">
    Merge pull request #258 from choptastic/patch-1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7b385ad');">
    // Steve Vinoski
  </div>
  <div id="msg-7b385ad" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7b385ad');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Mar 30 04:08:06 2016</strong>

Merge pull request #258 from choptastic/patch-1

Remove "endpoint gone away" message
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f1196ef" target="_blank">f1196ef</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f1196ef');">
    Remove "endpoint gone away" message
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f1196ef');">
    // Jesse Gumm
  </div>
  <div id="msg-f1196ef" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f1196ef');">
    <pre>
<strong>Author: Jesse Gumm &lt;sigmastar@gmail.com&gt;</strong>
<strong>Date:   Wed Mar 30 02:58:21 2016</strong>

Remove "endpoint gone away" message

When a websocket ping isn't ponged, yaws currently generates an "endpoint gone away" message. It seems kind of unnecessary to spam the error logger with it. Thoughts?
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7b3baeb" target="_blank">7b3baeb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7b3baeb');">
    Fix #257: docs for compression_level config setting
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7b3baeb');">
    // Steve Vinoski
  </div>
  <div id="msg-7b3baeb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7b3baeb');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Mar 26 14:46:51 2016</strong>

Fix #257: docs for compression_level config setting

Fix the compression_level config setting documentation, which
incorrectly showed the name of the setting as compress_level. Thanks
to Sergei Golovan for reporting this.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d889daa" target="_blank">d889daa</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d889daa');">
    Fix --ctltrace message about output file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d889daa');">
    // Steve Vinoski
  </div>
  <div id="msg-d889daa" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d889daa');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Mar 24 03:29:11 2016</strong>

Fix --ctltrace message about output file

Running "yaws --ctltrace" previously printed a message saying output
was being sent to a particular file, but the output was actually sent
elsewhere. Modify the message to instead point the user to the
directory where trace output will be sent.

Thanks to Leo Liu for reporting this.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3f38f85" target="_blank">3f38f85</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3f38f85');">
    Use inet_parse:ntoa/1 in yaws_ctl to format IP addrs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3f38f85');">
    // Steve Vinoski
  </div>
  <div id="msg-3f38f85" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3f38f85');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Mar 22 16:35:58 2016</strong>

Use inet_parse:ntoa/1 in yaws_ctl to format IP addrs

Instead of using ad hoc code use inet_parse:ntoa/1 to parse IP
addresses for status output. Note that we have to use
inet_parse:ntoa/1 rather than inet:ntoa/1, since the latter is not
available in older Erlang/OTP releases.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d3dcbf7" target="_blank">d3dcbf7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d3dcbf7');">
    Improve yaws script argument checking
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d3dcbf7');">
    // Steve Vinoski
  </div>
  <div id="msg-d3dcbf7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d3dcbf7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Mar 22 14:28:26 2016</strong>

Improve yaws script argument checking

Check for required arguments for some yaws script options and print
help and exit if the arguments are missing. Previously the script
would just assume the arguments were present, which would cause Erlang
crash dumps in some cases.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8188e92" target="_blank">8188e92</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8188e92');">
    Add support for new HTTP 451 status code
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8188e92');">
    // Steve Vinoski
  </div>
  <div id="msg-8188e92" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8188e92');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Mar 18 13:45:40 2016</strong>

Add support for new HTTP 451 status code
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9fcc6b5" target="_blank">9fcc6b5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9fcc6b5');">
    no server header if value is an empty string. fix #201
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9fcc6b5');">
    // Andrei Neculau
  </div>
  <div id="msg-9fcc6b5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9fcc6b5');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrei.neculau@gmail.com&gt;</strong>
<strong>Date:   Mon Mar 7 23:34:01 2016</strong>

no server header if value is an empty string. fix #201
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5d0eccc" target="_blank">5d0eccc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5d0eccc');">
    Merge pull request #255 from richcarl/load-with-empty-beam-path
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5d0eccc');">
    // Steve Vinoski
  </div>
  <div id="msg-5d0eccc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5d0eccc');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Mar 2 13:20:53 2016</strong>

Merge pull request #255 from richcarl/load-with-empty-beam-path

Load generated modules with empty path to beam file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/47c5350" target="_blank">47c5350</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-47c5350');">
    Load generated modules with empty path to beam file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-47c5350');">
    // Richard Carlsson
  </div>
  <div id="msg-47c5350" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-47c5350');">
    <pre>
<strong>Author: Richard Carlsson &lt;richardc@klarna.com&gt;</strong>
<strong>Date:   Wed Mar 2 09:56:20 2016</strong>

Load generated modules with empty path to beam file

Some tools will behave incorrectly if the beam file path reported by the
code server points to an existing file which isn't actually a beam file.
Leaving it empty signals that the code cannot be found on disk.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e701316" target="_blank">e701316</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e701316');">
    Fix ybed embedded supervision example
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e701316');">
    // Steve Vinoski
  </div>
  <div id="msg-e701316" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e701316');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Feb 24 15:00:43 2016</strong>

Fix ybed embedded supervision example

Modify the code for the ybed supervision example to link the ybed
child to its supervisor, specify the temporary restart strategy for
that child, and have the child exit normally after it starts the
necessary Yaws children in its supervisor. We use temporary rather
than permanent because the child needs to run only once, whenever the
supervisor starts.

Thanks to Lyn Headley for tracking down these issues and suggesting
the fixes described here.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/95af0b3" target="_blank">95af0b3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-95af0b3');">
    Fix trace log not catching the first request (refactoring)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-95af0b3');">
    // Andrei Neculau
  </div>
  <div id="msg-95af0b3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-95af0b3');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrein@tail-f.com&gt;</strong>
<strong>Date:   Fri Feb 19 16:23:38 2016</strong>

Fix trace log not catching the first request (refactoring)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ebfb12c" target="_blank">ebfb12c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ebfb12c');">
    When using yaws in embedded mode, the api yaws_config:hard_setconf/2 configured the optional tracing after the virt server was configured. This lead to a bug where the first accept() call didn't end up in the trace log, subsequent accept() calls did though.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ebfb12c');">
    // Klacke Wikstrom
  </div>
  <div id="msg-ebfb12c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ebfb12c');">
    <pre>
<strong>Author: Klacke Wikstrom &lt;cwikstro@cisco.com&gt;</strong>
<strong>Date:   Fri Feb 19 12:40:21 2016</strong>

When using yaws in embedded mode, the api yaws_config:hard_setconf/2
configured the optional tracing after the virt server was configured.
This lead to a bug where the first accept() call didn't end up in the
trace log, subsequent accept() calls did though.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/aa5f7ec" target="_blank">aa5f7ec</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-aa5f7ec');">
    remove error_logger call on websockets when sending the keep alive
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-aa5f7ec');">
    // Stuart Thackray
  </div>
  <div id="msg-aa5f7ec" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-aa5f7ec');">
    <pre>
<strong>Author: Stuart Thackray &lt;stuart.thackray@gmail.com&gt;</strong>
<strong>Date:   Tue Jan 12 12:33:06 2016</strong>

remove error_logger call on websockets when sending the keep alive
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d2577b0" target="_blank">d2577b0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d2577b0');">
    Fix #251: catch query parameter errors in www/man.yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d2577b0');">
    // Steve Vinoski
  </div>
  <div id="msg-d2577b0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d2577b0');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Jan 3 16:28:18 2016</strong>

Fix #251: catch query parameter errors in www/man.yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/11a1ac7" target="_blank">11a1ac7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-11a1ac7');">
    Merge pull request #250 from klacke/log-init
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-11a1ac7');">
    // Steve Vinoski
  </div>
  <div id="msg-11a1ac7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-11a1ac7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Dec 18 17:02:18 2015</strong>

Merge pull request #250 from klacke/log-init

Set up log event handler earlier at startup
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/01acebb" target="_blank">01acebb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-01acebb');">
    Set up log event handler earlier at startup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-01acebb');">
    // Steve Vinoski
  </div>
  <div id="msg-01acebb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-01acebb');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Dec 18 14:53:28 2015</strong>

Set up log event handler earlier at startup

In http://sourceforge.net/p/erlyaws/mailman/message/34701725/ a user
reported that with Yaws 2.x, info reports that occur at startup
providing details about config file, control file, and the endpoints
upon which Yaws is listening were not being emitted into report.log,
which is different behavior than that of Yaws 1.x. This commit
reorders the yaws_log setup in yaws_server:init so all but the initial
"Yaws: Using config file..." message makes it into report.log.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6d059d5" target="_blank">6d059d5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6d059d5');">
    Make yaws session idle and long timeouts configurable
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6d059d5');">
    // Steve Vinoski
  </div>
  <div id="msg-6d059d5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6d059d5');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Dec 1 22:36:30 2015</strong>

Make yaws session idle and long timeouts configurable

Add ysession_idle_timeout and ysession_long_timeout config
variables. Their defaults are unchanged. Add documentation for the new
variables.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c8f1918" target="_blank">c8f1918</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c8f1918');">
    Fix 247: src_dir compile error_logger msg levels
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c8f1918');">
    // Steve Vinoski
  </div>
  <div id="msg-c8f1918" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c8f1918');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Dec 1 23:36:40 2015</strong>

Fix 247: src_dir compile error_logger msg levels

When compiling source files from src_dir config paths, emit
error_logger messages as follows:

* for modules that compile with 0 errors and 0 warnings, emit info
  messages

* for modules that compile with 0 errors and 1 or more warnings, emit
  warning messages

* for modules that compile with errors, emit error messages
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/69f720e" target="_blank">69f720e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-69f720e');">
    Fix #244: remove deprecated src/json.erl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-69f720e');">
    // Steve Vinoski
  </div>
  <div id="msg-69f720e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-69f720e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Nov 6 15:36:10 2015</strong>

Fix #244: remove deprecated src/json.erl

The json.erl file has been deprecated for a long time, and since it
clashes with modules of the same name in popular JSON libraries,
remove it.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/74896d1" target="_blank">74896d1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-74896d1');">
    Merge pull request #246 from ostrovsky/gentoo
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-74896d1');">
    // Claes Wikstrom
  </div>
  <div id="msg-74896d1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-74896d1');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Nov 6 15:27:59 2015</strong>

Merge pull request #246 from ostrovsky/gentoo

Adjustments for Gentoo Linux
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/29dc12f" target="_blank">29dc12f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-29dc12f');">
    Adjustments for Gentoo Linux
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-29dc12f');">
    // Karol Ostrovsky
  </div>
  <div id="msg-29dc12f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-29dc12f');">
    <pre>
<strong>Author: Karol Ostrovsky &lt;karol.ostrovsky@gmail.com&gt;</strong>
<strong>Date:   Fri Nov 6 15:12:47 2015</strong>

Adjustments for Gentoo Linux

The init script for Gentoo Linux needed adjustment after Gentoo made
some developments.  Additionally, the ebuild script for Gentoo Linux was
removed at some point, but it is rather helpful for anyone running
Gentoo Linux.
    </pre>
  </div>
</div>

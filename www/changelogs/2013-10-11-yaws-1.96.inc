<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/487ac67" target="_blank">487ac67</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-487ac67');">
    Remove authmod_gssapi from the module list in src/Makefile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-487ac67');">
    // Christopher Faulet
  </div>
  <div id="msg-487ac67" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-487ac67');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Oct 11 11:15:30 2013</strong>

Remove authmod_gssapi from the module list in src/Makefile

This module was moved a long time ago in example/src.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b57e288" target="_blank">b57e288</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b57e288');">
    Fix mime_types.erl generation for a local installation of Yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b57e288');">
    // Christopher Faulet
  </div>
  <div id="msg-b57e288" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b57e288');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Oct 11 09:56:20 2013</strong>

Fix mime_types.erl generation for a local installation of Yaws

Without this fix, at the Yaws startup, the mime_types.erl generation fails
on windows when Yaws is installed using the installer. It also fails for
Yaws releases built with reltool (using rebar or autotools).

Fix issue #161.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ced32bf" target="_blank">ced32bf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ced32bf');">
    Fix typo in yaws_sendfile module
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ced32bf');">
    // Christopher Faulet
  </div>
  <div id="msg-ced32bf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ced32bf');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Oct 11 09:35:12 2013</strong>

Fix typo in yaws_sendfile module
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/18b226d" target="_blank">18b226d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-18b226d');">
    Add README.osx for instructions on installing on OSX using Homebrew
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-18b226d');">
    // Susan Potter
  </div>
  <div id="msg-18b226d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-18b226d');">
    <pre>
<strong>Author: Susan Potter &lt;me@susanpotter.net&gt;</strong>
<strong>Date:   Mon Oct 7 05:27:19 2013</strong>

Add README.osx for instructions on installing on OSX using Homebrew
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/df2f67d" target="_blank">df2f67d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-df2f67d');">
    Add note about running rebar get-deps before rebar compile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-df2f67d');">
    // Corey Haines
  </div>
  <div id="msg-df2f67d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-df2f67d');">
    <pre>
<strong>Author: Corey Haines &lt;coreyhaines@gmail.com&gt;</strong>
<strong>Date:   Sat Oct 5 20:02:35 2013</strong>

Add note about running rebar get-deps before rebar compile

I was trying to install with rebar compile and kept getting the failure
that the ibrowse dependency wasn't available. Once I did a rebar get-deps,
the rebar compile seemed to have gone through.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a8f37f2" target="_blank">a8f37f2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a8f37f2');">
    Adjust line numbers in known_dialyzer_warnings file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a8f37f2');">
    // Christopher Faulet
  </div>
  <div id="msg-a8f37f2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a8f37f2');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Sep 10 21:57:32 2013</strong>

Adjust line numbers in known_dialyzer_warnings file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c887a37" target="_blank">c887a37</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c887a37');">
    Add the way to configure the version of sendfile method to use
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c887a37');">
    // Christopher Faulet
  </div>
  <div id="msg-c887a37" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c887a37');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Sep 10 21:22:17 2013</strong>

Add the way to configure the version of sendfile method to use

Now, at compile time, we only check if sendfile syscall and file:sendfile/5
are supported. Then, dynamically, we can configure which version to use.
This can be done using the directive large_file_sendfile. Supported values
are:

 * erlang: if supported, use file:sendfile/5
 * yaws: if supported, use the Yaws sendfile linked-in driver
 * disable: do not use any sendfile method, but gen_tcp:send/2

If an unsupported method is configured, we fall back on gen_tcp:send/2.

file:sendfile/5 implementation is buggy (in R15 & R16). When async-threads
are enabled, in efile driver, the tcp socket is set in blocking mode and the
sendfile syscall is executed on an async-thread. So an unresponsive client
could block it for a very long time and therefore block the async-thread.
In this way, all async-threads could be easily blocked.
So, by default, the use of sendfile is disable.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8081c35" target="_blank">8081c35</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8081c35');">
    Add support of many options for tcp/ssl sockets
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8081c35');">
    // Christopher Faulet
  </div>
  <div id="msg-8081c35" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8081c35');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Sep 6 21:49:03 2013</strong>

Add support of many options for tcp/ssl sockets

By adding a "&lt;listen_opts&gt; ... &lt;/listen_opts&gt;" block in vhosts
configuration, it is possible to define some options to be set on listen
sockets and, by inheritance, on accepted sockets. Supported options are
(see inet:setopts for details):

  * buffer
  * delay_send
  * linger
  * nodelay
  * priority
  * sndbuf
  * recbuf
  * send_timeout
  * send_timeout_close

send_timeout / send_timeout_close options could be very useful to handle
unresponsive clients and thus avoid some sorts of DoS.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/42aa247" target="_blank">42aa247</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-42aa247');">
    Make the #gconf.large_file_chunk_size value be configurable
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-42aa247');">
    // Christopher Faulet
  </div>
  <div id="msg-42aa247" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-42aa247');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Sep 6 16:17:58 2013</strong>

Make the #gconf.large_file_chunk_size value be configurable
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/849c606" target="_blank">849c606</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-849c606');">
    Fix option to enable/disable sendfile support in the configure script
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-849c606');">
    // Christopher Faulet
  </div>
  <div id="msg-849c606" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-849c606');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Sep 6 09:25:03 2013</strong>

Fix option to enable/disable sendfile support in the configure script

Now it is possible to disable sendfile in yaws passing the option
'--disable-senfile' (or '--enable-sendfile=no') to the configure script.
It is also possible to choose between the erlang implementation (the default)
and the yaws implementation.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c29f7ed" target="_blank">c29f7ed</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c29f7ed');">
    minor yaws.tex edits
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c29f7ed');">
    // Steve Vinoski
  </div>
  <div id="msg-c29f7ed" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c29f7ed');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Sep 3 05:13:29 2013</strong>

minor yaws.tex edits
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/41fcce4" target="_blank">41fcce4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-41fcce4');">
    Merge pull request #148 from tuncer/rel-windows
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-41fcce4');">
    // Steve Vinoski
  </div>
  <div id="msg-41fcce4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-41fcce4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Sep 3 05:05:36 2013</strong>

Merge pull request #148 from tuncer/rel-windows

rel: add Windows scripts
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2652585" target="_blank">2652585</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2652585');">
    Document rebar use
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2652585');">
    // Tuncer Ayaz
  </div>
  <div id="msg-2652585" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2652585');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Mon Aug 26 21:45:14 2013</strong>

Document rebar use
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9e7e3e9" target="_blank">9e7e3e9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9e7e3e9');">
    rel: add Windows scripts
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9e7e3e9');">
    // Tuncer Ayaz
  </div>
  <div id="msg-9e7e3e9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9e7e3e9');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Sun Jun 23 10:28:53 2013</strong>

rel: add Windows scripts
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c370a2e" target="_blank">c370a2e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c370a2e');">
    Merge pull request #158 from ningzhang/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c370a2e');">
    // Steve Vinoski
  </div>
  <div id="msg-c370a2e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c370a2e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Aug 16 15:07:17 2013</strong>

Merge pull request #158 from ningzhang/master

fix a typo: "can be override" =&gt; "can be overridden"
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/45790e2" target="_blank">45790e2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-45790e2');">
    fix a typo: "can be override" => "can be overridden"
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-45790e2');">
    // Ning Zhang
  </div>
  <div id="msg-45790e2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-45790e2');">
    <pre>
<strong>Author: Ning Zhang &lt;ning.m.zhang@gmail.com&gt;</strong>
<strong>Date:   Fri Aug 16 10:09:25 2013</strong>

fix a typo: "can be override" =&gt; "can be overridden"
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0244e95" target="_blank">0244e95</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0244e95');">
    Fix a bug about log rotation. yaws_logger:do_rotate/2 was buggy.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0244e95');">
    // Christopher Faulet
  </div>
  <div id="msg-0244e95" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0244e95');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Jul 18 19:50:24 2013</strong>

Fix a bug about log rotation. yaws_logger:do_rotate/2 was buggy.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/549ac43" target="_blank">549ac43</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-549ac43');">
    Install procedure for FreeBSD wasn't correct
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-549ac43');">
    // Claes Wikstrom
  </div>
  <div id="msg-549ac43" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-549ac43');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Jul 18 20:25:18 2013</strong>

Install procedure for FreeBSD wasn't correct
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/19aed84" target="_blank">19aed84</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-19aed84');">
    Redhat /etc script bug fix by flavio-fernans
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-19aed84');">
    // Claes Wikstrom
  </div>
  <div id="msg-19aed84" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-19aed84');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Jul 16 14:01:45 2013</strong>

Redhat /etc script bug fix by flavio-fernans
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/75e834b" target="_blank">75e834b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-75e834b');">
    Incorrect X-Forwarded-For header
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-75e834b');">
    // Manuel Durán Aguete
  </div>
  <div id="msg-75e834b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-75e834b');">
    <pre>
<strong>Author: Manuel Durán Aguete &lt;manuel@aguete.org&gt;</strong>
<strong>Date:   Mon Jul 15 14:24:50 2013</strong>

Incorrect X-Forwarded-For header

When X-Forwarded-For header appears multiple times in a request only the
last value is supplied by yaws_api:headers_x_forwarded_for/1.

RFC 2616 section 4.2 permits multiple headers with the same name as an
equivalent of one header with a comma-separated list of values.

http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.2

Pound loadbalancer ( http://www.apsis.ch/pound ) repeats the header.

Change yaws to collect all values for this header and present them as a
single comma-separated list. Add a regression test to test/t2.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b1c5a4c" target="_blank">b1c5a4c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b1c5a4c');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b1c5a4c');">
    // Claes Wikstrom
  </div>
  <div id="msg-b1c5a4c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b1c5a4c');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Jul 11 15:35:14 2013</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/982107a" target="_blank">982107a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-982107a');">
    When max connections were reached and we attempt to close the socket, it was assumed we had a TCP socket, crashed on SSL sockets
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-982107a');">
    // Claes Wikstrom
  </div>
  <div id="msg-982107a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-982107a');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Jul 11 15:35:09 2013</strong>

When max connections were reached and we attempt to close the socket, it was assumed we had a TCP socket, crashed on SSL sockets
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8fdb1ec" target="_blank">8fdb1ec</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8fdb1ec');">
    Merge pull request #155 from mdaguete/session_server_fixes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8fdb1ec');">
    // Claes Wikstrom
  </div>
  <div id="msg-8fdb1ec" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8fdb1ec');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Jul 9 10:51:11 2013</strong>

Merge pull request #155 from mdaguete/session_server_fixes

yaws_session_server:print_sessions/0 not working due incorrect message
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/11b3b42" target="_blank">11b3b42</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-11b3b42');">
    yaws_session_server:print_sessions/0 not working due incorrect message
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-11b3b42');">
    // Manuel Durán Aguete
  </div>
  <div id="msg-11b3b42" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-11b3b42');">
    <pre>
<strong>Author: Manuel Durán Aguete &lt;manuel@aguete.org&gt;</strong>
<strong>Date:   Tue Jul 9 08:59:58 2013</strong>

yaws_session_server:print_sessions/0 not working due incorrect message
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e6bb751" target="_blank">e6bb751</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e6bb751');">
    export yaws:setup_sconf/2 for yapp
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e6bb751');">
    // Steve Vinoski
  </div>
  <div id="msg-e6bb751" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e6bb751');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Jul 5 21:30:27 2013</strong>

export yaws:setup_sconf/2 for yapp

The yapp application relies on the yaws:setup_sconf/2 function, but at some
point it was made private to yaws.erl. Export it so yapp works properly
again.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e57249e" target="_blank">e57249e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e57249e');">
    Merge remote-tracking branch 'MrStaticVoid/yaws-1.96/try-to-create...
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e57249e');">
    // Christopher Faulet
  </div>
  <div id="msg-e57249e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e57249e');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Jul 1 20:43:49 2013</strong>

Merge remote-tracking branch 'MrStaticVoid/yaws-1.96/try-to-create...

...-log-dir-if-specified-in-config'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a6320a5" target="_blank">a6320a5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a6320a5');">
    Try to make the log directory if it doesn't exist
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a6320a5');">
    // James Lee
  </div>
  <div id="msg-a6320a5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a6320a5');">
    <pre>
<strong>Author: James Lee &lt;jlee@thestaticvoid.com&gt;</strong>
<strong>Date:   Sun Jun 30 23:33:53 2013</strong>

Try to make the log directory if it doesn't exist

This uses @capflam's patch from https://github.com/klacke/yaws/pull/145
to create the log directory if specified like

    logdir = +path/to/log

in the configuration file.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ba0c733" target="_blank">ba0c733</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ba0c733');">
    Merge pull request #151 from fjoanis/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ba0c733');">
    // Steve Vinoski
  </div>
  <div id="msg-ba0c733" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ba0c733');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Jun 26 15:28:11 2013</strong>

Merge pull request #151 from fjoanis/master

Fix runtime error on OpenBSD 5.3
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/673ac60" target="_blank">673ac60</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-673ac60');">
    Use proper linker command for drivers on OpenBSD
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-673ac60');">
    // Francis Joanis
  </div>
  <div id="msg-673ac60" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-673ac60');">
    <pre>
<strong>Author: Francis Joanis &lt;francis.joanis@gmail.com&gt;</strong>
<strong>Date:   Wed Jun 26 03:56:02 2013</strong>

Use proper linker command for drivers on OpenBSD

This fixes the "Cannot load specified object" / setuid_drv.so: undefined symbol '__guard_local' error that occurs on OpenBSD 5.3. It changes the LD_SHARED variable to use gcc rather than ld. A similar issue is documented here: http://erlang.org/pipermail/erlang-questions/2013-May/073837.html. Tested on OpenBSD 5.3 and FreeBSD 9.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f7a758a" target="_blank">f7a758a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f7a758a');">
    Fix configure checks requiring module loading
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f7a758a');">
    // Johannes Weißl
  </div>
  <div id="msg-f7a758a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f7a758a');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Tue Jun 25 09:38:17 2013</strong>

Fix configure checks requiring module loading

After 98db40b the check outputs:

    $ erl -noshell -eval 'code:load_file(file), io:format("~p~n",[[erlang:function_exported(file,sendfile,5)]]), erlang:halt().'
    [true]

    =ERROR REPORT==== 25-Jun-2013::09:40:51 ===
    Can't load module that resides in sticky dir

Use code:ensure_loaded/1 instead as suggested by Tuncer Ayaz. Fix similar
module loading check in rebar.config.script.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4a4f899" target="_blank">4a4f899</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4a4f899');">
    Merge pull request #147 from tuncer/rel
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4a4f899');">
    // Steve Vinoski
  </div>
  <div id="msg-4a4f899" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4a4f899');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Jun 20 17:36:41 2013</strong>

Merge pull request #147 from tuncer/rel

rel: sync files with upstream templates
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/98db40b" target="_blank">98db40b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-98db40b');">
    changes for R16B01
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-98db40b');">
    // Steve Vinoski
  </div>
  <div id="msg-98db40b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-98db40b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Jun 20 17:32:51 2013</strong>

changes for R16B01

In R16B01, crypto:sha/1 is deprecated in favor of crypto:hash/2, but the
latter is not available prior to R15B02. Change the configure script and
rebar support to look for crypto:hash/2 and use it if found, otherwise fall
back to crypto:sha/1.

Also in R16B01 the public_key application, used by some of the tests, now
requires the asn1 application to be started first. Change the tests to
accommodate that.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5a66aa2" target="_blank">5a66aa2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5a66aa2');">
    rel: sync files with upstream templates
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5a66aa2');">
    // Tuncer Ayaz
  </div>
  <div id="msg-5a66aa2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5a66aa2');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Thu Jun 20 16:48:17 2013</strong>

rel: sync files with upstream templates
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/91d25e0" target="_blank">91d25e0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-91d25e0');">
    Merge pull request #146 from kape1395/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-91d25e0');">
    // Claes Wikstrom
  </div>
  <div id="msg-91d25e0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-91d25e0');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Jun 20 09:56:11 2013</strong>

Merge pull request #146 from kape1395/master

Yaws cannot be compiled with R16B1.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7df6a9b" target="_blank">7df6a9b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7df6a9b');">
    Use crypto:hash instead of crypto:sha to be compatible with R16B1.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7df6a9b');">
    // Karolis Petrauskas
  </div>
  <div id="msg-7df6a9b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7df6a9b');">
    <pre>
<strong>Author: Karolis Petrauskas &lt;karolis.petrauskas@etic.lt&gt;</strong>
<strong>Date:   Thu Jun 20 09:28:16 2013</strong>

Use crypto:hash instead of crypto:sha to be compatible with R16B1.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4aa0061" target="_blank">4aa0061</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4aa0061');">
    Group automatically the server configurations in embedded mode
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4aa0061');">
    // Christopher Faulet
  </div>
  <div id="msg-4aa0061" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4aa0061');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu May 30 14:41:01 2013</strong>

Group automatically the server configurations in embedded mode

Now, Yaws groups the server configurations by itself when it is started
in embedded mode. This is done when yaws_api:setconf/2 is called.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/83add25" target="_blank">83add25</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-83add25');">
    Support message body in DELETE requests
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-83add25');">
    // Christopher Faulet
  </div>
  <div id="msg-83add25" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-83add25');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon May 6 11:01:46 2013</strong>

Support message body in DELETE requests

The HTTP/1.1 specification does not explicitly forbid message body in
DELETE requests.
In fact, it is only forbidden in TRACE requests (rfc2616 - section 9.8).
So GET, HEAD and OPTION requests could contain message body. But it needs
more work to support message body in these requests.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/529b97b" target="_blank">529b97b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-529b97b');">
    Keep the original request in #arg{} and check it against auth rules
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-529b97b');">
    // Christopher Faulet
  </div>
  <div id="msg-529b97b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-529b97b');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon May 6 10:20:51 2013</strong>

Keep the original request in #arg{} and check it against auth rules

Because the request can be rewritten, it could be useful to backup the
original one. Appmods or Yaws scripts could access it by reading the field
orig_reg in #arg{}. For CGI scripts, the variable REQUEST_URI is now built
using the original request (SCRIPT_NAME, PATH_INFO and QUERY_STRING could
be used to know the info about the rewritten request, if needed).

The original request is also checked against the authorization rules. So, if
the request is rewritten, Yaws will check the original request and the
rewritten one. Access must be granted for both urls to accept the request.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e41fec6" target="_blank">e41fec6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e41fec6');">
    optimized ssi code by storing binaries as the data chunks instead of lists of chars
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e41fec6');">
    // Claes Wikstrom
  </div>
  <div id="msg-e41fec6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e41fec6');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Jun 4 07:39:51 2013</strong>

optimized ssi code by storing binaries as the data chunks instead of lists of chars
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5878ac6" target="_blank">5878ac6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5878ac6');">
    Fix handling of multipart boundary markers at input boundaries
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5878ac6');">
    // Rick Reed
  </div>
  <div id="msg-5878ac6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5878ac6');">
    <pre>
<strong>Author: Rick Reed &lt;rr@whatsapp.com&gt;</strong>
<strong>Date:   Thu May 30 17:15:09 2013</strong>

Fix handling of multipart boundary markers at input boundaries
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d702e96" target="_blank">d702e96</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d702e96');">
    pam bug found by Jari Hellman
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d702e96');">
    // Claes Wikstrom
  </div>
  <div id="msg-d702e96" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d702e96');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue May 21 12:25:58 2013</strong>

pam bug found by Jari Hellman
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ed285eb" target="_blank">ed285eb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ed285eb');">
    point to github on main page
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ed285eb');">
    // Claes Wikstrom
  </div>
  <div id="msg-ed285eb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ed285eb');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon May 13 20:17:42 2013</strong>

point to github on main page
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ab92cc8" target="_blank">ab92cc8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ab92cc8');">
    add encoding comments where needed (Nico Kruber)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ab92cc8');">
    // Steve Vinoski
  </div>
  <div id="msg-ab92cc8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ab92cc8');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 11 05:09:04 2013</strong>

add encoding comments where needed (Nico Kruber)

For newer versions of Erlang, modules that contain non-UTF-8 characters
require appropriate encoding comments.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4f5f767" target="_blank">4f5f767</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4f5f767');">
    Patch from constantine.deribin@gooddata.com to allow extra  modules for authentication
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4f5f767');">
    // Claes Wikstrom
  </div>
  <div id="msg-4f5f767" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4f5f767');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sat May 11 00:15:51 2013</strong>

Patch from constantine.deribin@gooddata.com to allow extra  modules for authentication
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8ed725e" target="_blank">8ed725e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8ed725e');">
    fix editing error in yaws.tex
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8ed725e');">
    // Steve Vinoski
  </div>
  <div id="msg-8ed725e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8ed725e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri May 3 23:12:32 2013</strong>

fix editing error in yaws.tex
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/08b12ca" target="_blank">08b12ca</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-08b12ca');">
    Add the global directive 'src_dir' to define source directories
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-08b12ca');">
    // Christopher Faulet
  </div>
  <div id="msg-08b12ca" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-08b12ca');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Apr 16 23:09:47 2013</strong>

Add the global directive 'src_dir' to define source directories

Yaws will compile all erlang modules found in these directories. The
compilation occurs when the configuration is loaded or reloaded. The
include_dir directives are used to search for include files. Multiple
src_dir directives may be used. There is no such directory configured by
default.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e4bbac3" target="_blank">e4bbac3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e4bbac3');">
    Add 'Last-Modified' header only for static files or 304 responses
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e4bbac3');">
    // Christopher Faulet
  </div>
  <div id="msg-e4bbac3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e4bbac3');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Apr 12 11:56:06 2013</strong>

Add 'Last-Modified' header only for static files or 304 responses

It is a partial rollback of the commit 89f3ea22.
The browsers seem to use this header to do aggressive cache (using some
strange heuristics), without doing any request to the server. IMHO, it is
a bug in browsers. Nevertheless, this header is useless for dynamic resources.
So we can skip it. When this header is meaningful for user's application, it
could be added explicitly.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/377f10d" target="_blank">377f10d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-377f10d');">
    Add serveralias directive
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-377f10d');">
    // Christopher Faulet
  </div>
  <div id="msg-377f10d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-377f10d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Apr 2 12:07:01 2013</strong>

Add serveralias directive

Now it is possible to set alternate names for a virtual host. A server alias
may contain wildcards:
 - '*' matches any sequence of zero or more characters
 - '?' matches one character unless that character is a period ('.')

Here is an example:

  &lt;server server.domain.com&gt;
    serveralias = server server2.domain.com server2
    serveralias = *.server.domain.com *.server?.domain.com
    ...
  &lt;/server&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f657faf" target="_blank">f657faf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f657faf');">
    enhance yaws_api:get_listen_port/1 documentation
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f657faf');">
    // Steve Vinoski
  </div>
  <div id="msg-f657faf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f657faf');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 23 04:15:50 2013</strong>

enhance yaws_api:get_listen_port/1 documentation
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e1d65ec" target="_blank">e1d65ec</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e1d65ec');">
    fix spelling error in sconf access function name
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e1d65ec');">
    // Steve Vinoski
  </div>
  <div id="msg-e1d65ec" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e1d65ec');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 21 23:09:50 2013</strong>

fix spelling error in sconf access function name
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9926f8a" target="_blank">9926f8a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9926f8a');">
    add yaws_api:get_listen_port/1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9926f8a');">
    // Steve Vinoski
  </div>
  <div id="msg-9926f8a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9926f8a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 21 22:40:02 2013</strong>

add yaws_api:get_listen_port/1

For testing scenarios it can be useful to configure yaws to run on an
ephemeral port by setting the server port number to 0. Add the
yaws_api:get_listen_port/1 function to enable callers to retrieve the
actual port number of a server from its listen socket. The single argument
to the function is the #sconf{} record for the target server.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9ce4859" target="_blank">9ce4859</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9ce4859');">
    clean up & clarify code in yaws_api:setconf/3
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9ce4859');">
    // Steve Vinoski
  </div>
  <div id="msg-9ce4859" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9ce4859');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 21 21:36:20 2013</strong>

clean up & clarify code in yaws_api:setconf/3
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3489539" target="_blank">3489539</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3489539');">
    General whitespace and indent cleanup, removed trailing whitespace and constrained code to 80 cols
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3489539');">
    // Claes Wikstrom
  </div>
  <div id="msg-3489539" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3489539');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Apr 8 18:07:16 2013</strong>

General whitespace and indent cleanup, removed trailing whitespace and constrained code to 80 cols
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2b2ad1f" target="_blank">2b2ad1f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2b2ad1f');">
    remove obsolete references to removed yaws_dav module
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2b2ad1f');">
    // Steve Vinoski
  </div>
  <div id="msg-2b2ad1f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2b2ad1f');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 7 17:36:33 2013</strong>

remove obsolete references to removed yaws_dav module
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/43f8fe7" target="_blank">43f8fe7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-43f8fe7');">
    fix dialyzer warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-43f8fe7');">
    // Steve Vinoski
  </div>
  <div id="msg-43f8fe7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-43f8fe7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 7 16:01:38 2013</strong>

fix dialyzer warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9434c0c" target="_blank">9434c0c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9434c0c');">
    fix comment heads and some indentation in yaws_appmod_dav
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9434c0c');">
    // Steve Vinoski
  </div>
  <div id="msg-9434c0c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9434c0c');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 7 16:00:43 2013</strong>

fix comment heads and some indentation in yaws_appmod_dav
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/74a5b33" target="_blank">74a5b33</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-74a5b33');">
    increase server stop wait time for tests
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-74a5b33');">
    // Steve Vinoski
  </div>
  <div id="msg-74a5b33" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-74a5b33');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 7 15:41:23 2013</strong>

increase server stop wait time for tests

Increase the server stop wait time in test/support/include.mk.in so that
it's long enough to work with debug builds.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/98ffe2f" target="_blank">98ffe2f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-98ffe2f');">
    fix compiler warnings in Yaws applications
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-98ffe2f');">
    // Steve Vinoski
  </div>
  <div id="msg-98ffe2f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-98ffe2f');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Apr 5 00:18:01 2013</strong>

fix compiler warnings in Yaws applications
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a6706eb" target="_blank">a6706eb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a6706eb');">
    Move WebDAV to an appmod, the locking server to a runmod (tjeerd)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a6706eb');">
    // Steve Vinoski
  </div>
  <div id="msg-a6706eb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a6706eb');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Mar 25 03:06:40 2013</strong>

Move WebDAV to an appmod, the locking server to a runmod (tjeerd)

All WebDAV functionality is moved to an appmod so the extension methods and
additional response headers are not part of yaws_server.erl and yaws.erl
anymore. Also the locking server is now a runmod so it is not started by
the supervisor and can be used only when needed.

Additionally the 100-Continue code is moved so it can be returned to all
requests that contain a body.

The WebDAV functionality is tested against the litmus test. All tests are
passed, except the ones that require unsupported properties. The test shows
this as a fail while returning 404 should be sufficient.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b6b38a6" target="_blank">b6b38a6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b6b38a6');">
    add documentation of exhtml directive to yaws.tex (by tjeerd)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b6b38a6');">
    // Steve Vinoski
  </div>
  <div id="msg-b6b38a6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b6b38a6');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Mar 25 01:54:11 2013</strong>

add documentation of exhtml directive to yaws.tex (by tjeerd)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/19ad254" target="_blank">19ad254</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-19ad254');">
    preparing for yaws 1.96
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-19ad254');">
    // Claes Wikstrom
  </div>
  <div id="msg-19ad254" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-19ad254');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Mar 19 18:02:01 2013</strong>

preparing for yaws 1.96
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9acc88e" target="_blank">9acc88e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9acc88e');">
    Slightly better error messages on failed startup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9acc88e');">
    // Claes Wikstrom
  </div>
  <div id="msg-9acc88e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9acc88e');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Mar 19 13:07:54 2013</strong>

Slightly better error messages on failed startup
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bead34b" target="_blank">bead34b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bead34b');">
    add yaws_api:merge_header/2 and yaws_api:merge_header/3 funs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bead34b');">
    // Steve Vinoski
  </div>
  <div id="msg-bead34b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bead34b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Mar 14 00:26:25 2013</strong>

add yaws_api:merge_header/2 and yaws_api:merge_header/3 funs

The merge_header functions allow HTTP headers and their values to be set
and combined in a #headers{} record.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/80b5767" target="_blank">80b5767</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-80b5767');">
    fix overflowing line in yaws.tex
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-80b5767');">
    // Steve Vinoski
  </div>
  <div id="msg-80b5767" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-80b5767');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Mar 8 12:30:49 2013</strong>

fix overflowing line in yaws.tex
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/729b1e4" target="_blank">729b1e4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-729b1e4');">
    fix issue #140
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-729b1e4');">
    // Steve Vinoski
  </div>
  <div id="msg-729b1e4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-729b1e4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Mar 8 12:27:16 2013</strong>

fix issue #140
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1ec97b0" target="_blank">1ec97b0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1ec97b0');">
    fix deprecated make_default_sconf/0 to return sconf, not gconf
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1ec97b0');">
    // Steve Vinoski
  </div>
  <div id="msg-1ec97b0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1ec97b0');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Mar 3 21:32:34 2013</strong>

fix deprecated make_default_sconf/0 to return sconf, not gconf
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6015755" target="_blank">6015755</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6015755');">
    fix R16B dialyzer issue
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6015755');">
    // Steve Vinoski
  </div>
  <div id="msg-6015755" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6015755');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Feb 27 16:06:50 2013</strong>

fix R16B dialyzer issue
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a7acf19" target="_blank">a7acf19</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a7acf19');">
    spelling err
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a7acf19');">
    // Claes Wikstrom
  </div>
  <div id="msg-a7acf19" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a7acf19');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Feb 15 20:24:50 2013</strong>

spelling err
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9975e7b" target="_blank">9975e7b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9975e7b');">
    Fix typo in documentation
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9975e7b');">
    // Christopher Faulet
  </div>
  <div id="msg-9975e7b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9975e7b');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Feb 12 15:42:54 2013</strong>

Fix typo in documentation
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/29a7989" target="_blank">29a7989</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-29a7989');">
    Refactor WebSockets and add support of optional callback functions
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-29a7989');">
    // Christopher Faulet
  </div>
  <div id="msg-29a7989" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-29a7989');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Dec 10 14:15:38 2012</strong>

Refactor WebSockets and add support of optional callback functions

Main changes:
  * Fix some bugs about UTF-8 encoding and messages fragmentation
  * Add support of optional callback functions
  * Add support of many startup options
  * Add support of outgoing fragmented messages
  * Add a websocket testsuite

                                 - * -
*** bug fixes ***

First of all, an huge part of yaws_websocket.erl was rewritten to fix bugs
about the messages fragmentation and the UTF-8 encoding of incoming text
messages:

  * UTF-8 encoding
    before, when a text message was fragmented, only the first frame was
    checked and partial UTF-8 sequences were not supported. Now, checks
    are done on each message part and a partial UTF-8 sequence at the end
    of a frame is accumulated and checked with the next frame (for basic
    callback only).

  * Messages fragmentation
    for basic callback modules, because of a buggy mapping between frames
    and messages, the messages fragmentation was almost unusable. To fix
    this, the message handling was rewritten.

Now, all tests in the autobahn testsuite[1] pass successfully.

                                 - * -
*** Optional callback functions ***

Then, from an idea of François de Metz[2], yaws_websocket module was
extended to support optional callback functions. See the documentation for
details (www/websockets.yaws).

Quickly, optional callback functions are:

  * Module:init/1           (for basic and advanced callback modules)
  * Module:terminate/2      (for basic and advanced callback modules)
  * Module:handle_open/2    (for basic and advanced callback modules)
  * Module:handle_info/2    (for basic and advanced callback modules)
  * Module:handle_message/2 (for basic callback modules only, used in place
                             of Module:handle_message/1)

Thanks to Pablo Vieytes[3] which added handle_info to optional callback
functions.
                                 - * -
*** Startup options ***

To start a websocket process a script must return the following term from
its out/1 function:

  {websocket, CallbackMod, Options}

where 'Options' is a (possibly empty) proplist. Following parameters are
supported:

  * {origin, Orig}
  * {callback, Type}
  * {keepalive, Boolean}
  * {keepalive_timeout, Tout}
  * {keepalive_grace_period, Time}
  * {drop_on_timeout, Boolean}
  * {close_timeout, Tout}
  * {close_if_unmasked, Boolean}
  * {max_frame_size, Int}
  * {max_message_size, Int}
  * {auto_fragment_message, Boolean}
  * {auto_fragment_threshold, Int}

See the documentation for details (www/websockets.yaws).

                                 - * -
*** Outgoing fragmented messages ***

A callback module can now send fragmented messages to clients using the
record #ws_frame{}:

 #ws_frame{fin     = true,  %% true | false
           rsv     = 0,
           opcode,          %% text | binary | continuation...
           payload = &lt;&lt;&gt;&gt;}. %% binary(), unmasked data

--
[1] http://autobahn.ws/testsuite
[2] https://github.com/klacke/yaws/pull/99
[3] https://github.com/pvieytes
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c341c31" target="_blank">c341c31</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c341c31');">
    Adjust line numbers in known_dialyzer_warnings file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c341c31');">
    // Christopher Faulet
  </div>
  <div id="msg-c341c31" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c341c31');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Jan 3 11:53:04 2013</strong>

Adjust line numbers in known_dialyzer_warnings file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/48c0510" target="_blank">48c0510</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-48c0510');">
    Add access functions for #gconf{} and #sconf{} records
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-48c0510');">
    // Christopher Faulet
  </div>
  <div id="msg-48c0510" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-48c0510');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 9 10:26:48 2012</strong>

Add access functions for #gconf{} and #sconf{} records

Applications may want to retrieve information in it. yaws:gconf_*/1 and
yaws:sconf_*/1 should be used in preference to a direct access to reduce
the dependence of your code on these records. So internal modifications
could be done avoiding the need to update and recompile your application.

Update www/internals.yaws accordingly and remove dependency on 'yaws.hrl'
in examples and applications. Add unit tests to check #gconf{} and #sconf{}
setup.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7da7481" target="_blank">7da7481</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7da7481');">
    Add support for deactivation of cache in reentrant calls
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7da7481');">
    // Christopher Faulet
  </div>
  <div id="msg-7da7481" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7da7481');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Nov 27 14:32:51 2012</strong>

Add support for deactivation of cache in reentrant calls
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d748f92" target="_blank">d748f92</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d748f92');">
    Add support of the 'Vary' header in response
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d748f92');">
    // Christopher Faulet
  </div>
  <div id="msg-d748f92" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d748f92');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 30 12:26:29 2012</strong>

Add support of the 'Vary' header in response

With this patch, Yaws will add 'Accept-Encoding' in 'Vary' header if the
support of gzip compression is enabled or if the response is compressed.
The 'Vary' header can be set using 'yaws:outh_set_vary(Fields)' or by
returning '{header, {vary, Fields}}' from scripts (where Fields is a list
of header names).
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f5f62b6" target="_blank">f5f62b6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f5f62b6');">
    Fix checks on the response content type when a charset is present
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f5f62b6');">
    // Christopher Faulet
  </div>
  <div id="msg-f5f62b6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f5f62b6');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Nov 29 17:10:14 2012</strong>

Fix checks on the response content type when a charset is present

When we test if a response is compressible or not, or when 'Expires' and
'Cache-Control' headers are set, the charset must be removed from the
response content-type to only keep the response mime-type.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/89f3ea2" target="_blank">89f3ea2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-89f3ea2');">
    Fix when 'Expires' and 'Cache-Control' headers are set
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-89f3ea2');">
    // Christopher Faulet
  </div>
  <div id="msg-89f3ea2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-89f3ea2');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 9 17:35:01 2012</strong>

Fix when 'Expires' and 'Cache-Control' headers are set

Because the content-type of the response may change many times during the
request handling process, we must wait the #outh{} serialization to set it.

Note: 'Last-modified' header is also managed in a same way because there is
no good reason to set it before.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fdc06fd" target="_blank">fdc06fd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fdc06fd');">
    Update docs & testsuite accordingly
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fdc06fd');">
    // Christopher Faulet
  </div>
  <div id="msg-fdc06fd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fdc06fd');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 23 15:20:03 2012</strong>

Update docs & testsuite accordingly
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f5343a3" target="_blank">f5343a3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f5343a3');">
    Flush remaining data in case of 3xx redirect
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f5343a3');">
    // Christopher Faulet
  </div>
  <div id="msg-f5343a3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f5343a3');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 23 14:51:50 2012</strong>

Flush remaining data in case of 3xx redirect

When Yaws returns a 3xx response, if the client connection is kept alive,
the remaining data must be flushed. Else the next request on the same
connection cannot be handled.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4fed987" target="_blank">4fed987</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4fed987');">
    Fix a bug: redirect URL must be url-encoded
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4fed987');">
    // Christopher Faulet
  </div>
  <div id="msg-4fed987" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4fed987');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 23 14:39:53 2012</strong>

Fix a bug: redirect URL must be url-encoded
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e7740bc" target="_blank">e7740bc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e7740bc');">
    Keep info about the original request during a reentrant call
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e7740bc');">
    // Christopher Faulet
  </div>
  <div id="msg-e7740bc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e7740bc');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 23 14:26:59 2012</strong>

Keep info about the original request during a reentrant call

Some information must be preserved during a reentrant call. From the original
request, we keep:
  * buffered data (Arg#arg.clidata) and the number of bytes read
  * the request state (Arg#arg.cont & Arg#arg.state)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0a2e565" target="_blank">0a2e565</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0a2e565');">
    Add a clause to yaws_api:url_encode to handle deep lists
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0a2e565');">
    // Christopher Faulet
  </div>
  <div id="msg-0a2e565" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0a2e565');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 23 09:59:06 2012</strong>

Add a clause to yaws_api:url_encode to handle deep lists
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bb5124b" target="_blank">bb5124b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bb5124b');">
    Make be possible to chain appmods
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bb5124b');">
    // Christopher Faulet
  </div>
  <div id="msg-bb5124b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bb5124b');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed Nov 21 11:18:28 2012</strong>

Make be possible to chain appmods

Before, all appmods were disabled during a reentrent call. So it was
impossible to chain appmods. Now, it is possible. Only the current appmod
is disabled for the next recursion (if we are inside an appmod of course).

So if we have this configuration for appmods:

  appmods = &lt;/foo, foo&gt; &lt;/bar, bar&gt;

When "/foo" is requested, the appmon "foo" is called. At this step, it is
possible to call the appmod "bar" by returning {page, "/bar"}.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ebe66b6" target="_blank">ebe66b6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ebe66b6');">
    Support multipart/form-data "name" with backslash at the end.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ebe66b6');">
    // Johannes Weißl
  </div>
  <div id="msg-ebe66b6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ebe66b6');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Sat Dec 1 17:22:55 2012</strong>

Support multipart/form-data "name" with backslash at the end.

Also add multipart/form-data tests ("name" with escaped characters).
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/25b8e4c" target="_blank">25b8e4c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-25b8e4c');">
    Merge pull request #133 from olgeni/tilde_injection_fix
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-25b8e4c');">
    // Christopher Faulet
  </div>
  <div id="msg-25b8e4c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-25b8e4c');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Nov 29 15:20:11 2012</strong>

Merge pull request #133 from olgeni/tilde_injection_fix

Fix io:format call in yaws_compile.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1cf4eeb" target="_blank">1cf4eeb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1cf4eeb');">
    Fix io:format call in yaws_compile.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1cf4eeb');">
    // olgeni
  </div>
  <div id="msg-1cf4eeb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1cf4eeb');">
    <pre>
<strong>Author: olgeni &lt;olgeni@olgeni.com&gt;</strong>
<strong>Date:   Thu Nov 29 12:47:45 2012</strong>

Fix io:format call in yaws_compile.

Basically, yaws_compile was unable to process items in some "unlucky"
paths on Win32.

NTFS may generate filenames in an MS-DOS compatible format if the
original filename is not compatible with the legacy 8.3 notation;
these filenames often contain tilde characters, which are inserted
(unquoted) in the Format parameter of io:format by the string
concatenation.

These unquoted tilde characters are processed as usual, leading to
a badarg error as the Data parameter is always an empty list.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7061375" target="_blank">7061375</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7061375');">
    Fix compilation of mime_types.erl when working directory is not 'yaws'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7061375');">
    // Christopher Faulet
  </div>
  <div id="msg-7061375" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7061375');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Nov 29 11:56:40 2012</strong>

Fix compilation of mime_types.erl when working directory is not 'yaws'

When mime_types.erl is generated, we must check if it is done during a Yaws
compilation or a Yaws startup. In the first case, we must use a relative
path for includes. Else include_lib must be used.
This issue was reported by Andrei Zavada and avoid a compile error when the
working directory is not 'yaws'.

The mechanisms to generate and compile this file have also changed.

Then, to centralize calls to yaws_generated:is_local_install/0, following
functions was added in yaws.erl:
  get_app_dir/0, get_src_dir/0, get_ebin_dir/0, get_priv_dir/0, get_inc_dir/0
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/dc424c8" target="_blank">dc424c8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-dc424c8');">
    skip sslaccept_tout_test for R15B03
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-dc424c8');">
    // Steve Vinoski
  </div>
  <div id="msg-dc424c8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-dc424c8');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Nov 29 00:20:56 2012</strong>

skip sslaccept_tout_test for R15B03

An SSL bug introduced in R15B03 makes the sslaccept_tout_test fail. The bug
is explained here:

http://erlang.org/pipermail/erlang-bugs/2012-November/003208.html
http://erlang.org/pipermail/erlang-bugs/2012-November/003209.html

Check the Erlang version and skip that test for R15B03.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0f6226d" target="_blank">0f6226d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0f6226d');">
    Take care to preserve the query-string in yaws_server:deliver_302/4
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0f6226d');">
    // Christopher Faulet
  </div>
  <div id="msg-0f6226d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0f6226d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Nov 20 15:34:42 2012</strong>

Take care to preserve the query-string in yaws_server:deliver_302/4

It is an old bug on the redirect made when the trailing slash is missing.
The query-string was always missing from the redirect URL. So now, we add
it explicitly.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/26e7ed2" target="_blank">26e7ed2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-26e7ed2');">
    Extend syntax of redirect block to allow an optional status code
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-26e7ed2');">
    // Christopher Faulet
  </div>
  <div id="msg-26e7ed2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-26e7ed2');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Nov 13 17:11:18 2012</strong>

Extend syntax of redirect block to allow an optional status code

Now, it is possible to change the HTTP status code used in redirect rules.
Supported formats are:

   Path = URL
   Path = code
   Path = code URL

* '==' syntax instead of '=' is always valid.
* If no status code is defined, Yaws will perform a 302 redirect.
* For 3xx status codes, the URL parameter must be present and will be used
  to build the new location.
* For other status codes (1xx, 2xx, 4xx and 5xx), it can be omitted. In the
  absence of URL, Yaws will return a generic response with the specified
  status code. Otherwise, the URL parameter must be a relative URL and will
  be used to customize the response.

For example:

  &lt;redirect&gt;
    /foo = http://yaws.hyber.org  # (1)
    /bar = 301 /foo               # (2)
    /baz = 404                    # (3)
    /qux = 403 /forbidden.yaws    # (4)
  &lt;/redirect&gt;

(1) do a 302 redirect on 'http://yaws.hyber.org/foo/...'
(2) do a 301 redirect on 'scheme:host:port/bar/...'
(3) return a generic 404 error
(4) do a reentrant call to '/forbidden.yaws' by setting the status code
    to 403.

Note: this new feature is fully compatible with older one.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/05be6e6" target="_blank">05be6e6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-05be6e6');">
    handle case with DOS attack towards pam login with embedded NUL chars in username or password. The protocol cannot handle NUL chars and Yaws dies
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-05be6e6');">
    // Claes Wikstrom
  </div>
  <div id="msg-05be6e6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-05be6e6');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Nov 9 12:06:48 2012</strong>

handle case with DOS attack towards pam login with embedded NUL chars in username or password. The protocol cannot handle NUL chars and Yaws dies
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cda2a27" target="_blank">cda2a27</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cda2a27');">
    Remove invalid macro in yaws.hrl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cda2a27');">
    // Christopher Faulet
  </div>
  <div id="msg-cda2a27" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cda2a27');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 9 09:38:18 2012</strong>

Remove invalid macro in yaws.hrl

?sc_set_ssl is never used and rely on the undefined macro ?SC_SSL.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a8077fd" target="_blank">a8077fd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a8077fd');">
    Fix doc errors
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a8077fd');">
    // Christopher Faulet
  </div>
  <div id="msg-a8077fd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a8077fd');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Nov 9 09:37:33 2012</strong>

Fix doc errors
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b416f25" target="_blank">b416f25</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b416f25');">
    start crypto app for yaws_session_server_test for R13
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b416f25');">
    // Steve Vinoski
  </div>
  <div id="msg-b416f25" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b416f25');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 25 00:23:41 2012</strong>

start crypto app for yaws_session_server_test for R13
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7e5e673" target="_blank">7e5e673</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7e5e673');">
    add Ahti Legonkov to contributors file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7e5e673');">
    // Steve Vinoski
  </div>
  <div id="msg-7e5e673" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7e5e673');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Oct 23 22:06:36 2012</strong>

add Ahti Legonkov to contributors file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f496811" target="_blank">f496811</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f496811');">
    adjust line numbers in known_dialyzer_warnings file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f496811');">
    // Steve Vinoski
  </div>
  <div id="msg-f496811" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f496811');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Oct 23 22:02:03 2012</strong>

adjust line numbers in known_dialyzer_warnings file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0281736" target="_blank">0281736</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0281736');">
    Fix bug #128 and add a new test case for it
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0281736');">
    // Ahti Legonkov
  </div>
  <div id="msg-0281736" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0281736');">
    <pre>
<strong>Author: Ahti Legonkov &lt;ahti.legonkov@gmail.com&gt;</strong>
<strong>Date:   Mon Oct 22 17:23:39 2012</strong>

Fix bug #128 and add a new test case for it
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/954ccb1" target="_blank">954ccb1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-954ccb1');">
    ensure compatibility with Erlang R13B04
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-954ccb1');">
    // Steve Vinoski
  </div>
  <div id="msg-954ccb1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-954ccb1');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Oct 23 17:05:19 2012</strong>

ensure compatibility with Erlang R13B04

For R13B04 compatibility, fix bare calls to integer_to_list and
list_to_integer, and in yaws_davlock provide an alternative for pre-R14 for
finding hwaddrs.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0f41b7b" target="_blank">0f41b7b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0f41b7b');">
    fix dialyzer issue in yaws_dav and update known_dialyzer_warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0f41b7b');">
    // Steve Vinoski
  </div>
  <div id="msg-0f41b7b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0f41b7b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Oct 23 03:29:04 2012</strong>

fix dialyzer issue in yaws_dav and update known_dialyzer_warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/06a2b0e" target="_blank">06a2b0e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-06a2b0e');">
    fix call to file:read_file_info/1 in yaws_dav
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-06a2b0e');">
    // Steve Vinoski
  </div>
  <div id="msg-06a2b0e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-06a2b0e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Oct 23 03:23:47 2012</strong>

fix call to file:read_file_info/1 in yaws_dav
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2a131ff" target="_blank">2a131ff</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2a131ff');">
    keep websocket errors from crashing gen_server
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2a131ff');">
    // Steve Vinoski
  </div>
  <div id="msg-2a131ff" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2a131ff');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Oct 22 23:45:07 2012</strong>

keep websocket errors from crashing gen_server

Change websocket implementation to not stop abnormally or crash with noisy
logs if errors occur.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fb4df1f" target="_blank">fb4df1f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fb4df1f');">
    add comment to websocket example explaining {origin, any}
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fb4df1f');">
    // Steve Vinoski
  </div>
  <div id="msg-fb4df1f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fb4df1f');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Oct 22 02:09:13 2012</strong>

add comment to websocket example explaining {origin, any}
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b3bc43c" target="_blank">b3bc43c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b3bc43c');">
    Handle relative paths when Yaws is started in embedded mode
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b3bc43c');">
    // Christopher Faulet
  </div>
  <div id="msg-b3bc43c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b3bc43c');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Oct 19 10:35:44 2012</strong>

Handle relative paths when Yaws is started in embedded mode

Now, following functions will throw an error if the docroot
directory does not exist:

  * yaws_api:embedded_start_conf/1,2,3,4
  * yaws:start_embedded/1,2,3,4
  * yaws:add_server/2
  * yaws:create_sconf/2
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1939b38" target="_blank">1939b38</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1939b38');">
    make error reports for stats:sent() go away nd be ignored
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1939b38');">
    // Claes Wikstrom
  </div>
  <div id="msg-1939b38" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1939b38');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Oct 17 21:03:53 2012</strong>

make error reports for stats:sent() go away nd be ignored
    </pre>
  </div>
</div>

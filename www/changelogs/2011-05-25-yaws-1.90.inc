<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cd50d66" target="_blank">cd50d66</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cd50d66');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cd50d66');">
    // Claes Wikstrom
  </div>
  <div id="msg-cd50d66" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cd50d66');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed May 25 23:33:26 2011</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fda6d26" target="_blank">fda6d26</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fda6d26');">
    FreeBSD patch by Jimmy Olgeni, add quotes around vsn number in .app file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fda6d26');">
    // Claes Wikstrom
  </div>
  <div id="msg-fda6d26" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fda6d26');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed May 25 23:32:50 2011</strong>

FreeBSD patch by Jimmy Olgeni, add quotes around vsn number in .app file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/16ede48" target="_blank">16ede48</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-16ede48');">
    increase default maximum wait-started time
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-16ede48');">
    // Steve Vinoski
  </div>
  <div id="msg-16ede48" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-16ede48');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed May 25 17:13:31 2011</strong>

increase default maximum wait-started time

Increase the default maximum time for the yaws --wait-started option
from 6 seconds to 30 seconds. The test suite was seeing random
failures due to 6 second wait not being enough. (The option doesn't
mean it waits for 30 seconds every time, just that it will wait for a
maximum of 30 seconds for yaws with the specified id to start.)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fcae3cd" target="_blank">fcae3cd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fcae3cd');">
    quote Install arguments in regular-install (Christopher Faulet)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fcae3cd');">
    // Steve Vinoski
  </div>
  <div id="msg-fcae3cd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fcae3cd');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue May 17 05:32:54 2011</strong>

quote Install arguments in regular-install (Christopher Faulet)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4c2ae80" target="_blank">4c2ae80</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4c2ae80');">
    minor corrections to news file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4c2ae80');">
    // Steve Vinoski
  </div>
  <div id="msg-4c2ae80" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4c2ae80');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed May 25 16:01:11 2011</strong>

minor corrections to news file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3d09de8" target="_blank">3d09de8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3d09de8');">
    correct handling of delayed requests (Christopher Faulet)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3d09de8');">
    // Steve Vinoski
  </div>
  <div id="msg-3d09de8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3d09de8');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed May 25 15:59:10 2011</strong>

correct handling of delayed requests (Christopher Faulet)

Correction for one of the capflam patches that wasn't fully applied.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/66a0c5e" target="_blank">66a0c5e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-66a0c5e');">
    contributor list additions, corrections, and sorting
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-66a0c5e');">
    // Steve Vinoski
  </div>
  <div id="msg-66a0c5e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-66a0c5e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed May 25 06:37:17 2011</strong>

contributor list additions, corrections, and sorting

Fix some misspellings in the www/contributors.txt list, add missing
contributors mostly gleaned from eyeballing the old sourceforge svn
repository, eliminate duplicates, and sort it alphabetically by
surname it to make it easier to track.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/17df5ce" target="_blank">17df5ce</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-17df5ce');">
    starting to prepare for 1.90
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-17df5ce');">
    // Claes Wikstrom
  </div>
  <div id="msg-17df5ce" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-17df5ce');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue May 24 22:28:44 2011</strong>

starting to prepare for 1.90
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9dd0990" target="_blank">9dd0990</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9dd0990');">
    publish the contributors list on frontpage
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9dd0990');">
    // Claes Wikstrom
  </div>
  <div id="msg-9dd0990" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9dd0990');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue May 24 21:59:03 2011</strong>

publish the contributors list on frontpage
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1540c17" target="_blank">1540c17</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1540c17');">
    starting to prepare for 1.90
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1540c17');">
    // Claes Wikstrom
  </div>
  <div id="msg-1540c17" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1540c17');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue May 24 21:32:20 2011</strong>

starting to prepare for 1.90
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4582480" target="_blank">4582480</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4582480');">
    starting to prepare for 1.90
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4582480');">
    // Claes Wikstrom
  </div>
  <div id="msg-4582480" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4582480');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue May 24 21:31:48 2011</strong>

starting to prepare for 1.90
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c48a0e8" target="_blank">c48a0e8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c48a0e8');">
    dups
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c48a0e8');">
    // Claes Wikstrom
  </div>
  <div id="msg-c48a0e8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c48a0e8');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue May 24 20:30:07 2011</strong>

dups
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/db7b2af" target="_blank">db7b2af</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-db7b2af');">
    fix server listen config initialization
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-db7b2af');">
    // Steve Vinoski
  </div>
  <div id="msg-db7b2af" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-db7b2af');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue May 24 06:52:15 2011</strong>

fix server listen config initialization

One of the capflam contributions added the ability for a server config
to have multiple listen directives. Part of the patch was to
initialize the #sconf.listen field to the empty list when starting to
parse a server config, but that was erroneously left out. Fixed.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/db3ec48" target="_blank">db3ec48</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-db3ec48');">
    always store a tuple in #headers.authorization (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-db3ec48');">
    // Jean-Sébastien Pédron
  </div>
  <div id="msg-db3ec48" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-db3ec48');">
    <pre>
<strong>Author: Jean-Sébastien Pédron &lt;jean-sebastien@yakaz.com&gt;</strong>
<strong>Date:   Fri May 20 16:56:05 2011</strong>

always store a tuple in #headers.authorization (capflam)

The function yaws:parse_auth/1 returned "undefined" if it couldn't
parse the value of the "Authorization" header. In this case, we lost
the original value of this header.

Always store a tuple of the form "{User, Pass, Orig}" even if the
value can't be parsed. In this case, "User" and "Pass" are set to
"undefined".

Also, export yaws:parse_auth/1 so that 3rd-party modules can set
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/049d4b1" target="_blank">049d4b1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-049d4b1');">
    fix include of yaws_debug.hrl (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-049d4b1');">
    // Jean-Sébastien Pédron
  </div>
  <div id="msg-049d4b1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-049d4b1');">
    <pre>
<strong>Author: Jean-Sébastien Pédron &lt;jean-sebastien@yakaz.com&gt;</strong>
<strong>Date:   Fri May 20 12:40:22 2011</strong>

fix include of yaws_debug.hrl (capflam)

A couple files included yaws_debug.hrl with the following path:

  ../../yaws/src/yaws_debug.hrl

This assumes the root directory is named "yaws". Also, the directory
paths used are pointless since yaws_debug.hrl lives in the same
directory as the source files including it, so they can just include
it without specifying any preceding directory paths.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f64b24d" target="_blank">f64b24d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f64b24d');">
    improve daemon status output (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f64b24d');">
    // Christopher Faulet
  </div>
  <div id="msg-f64b24d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f64b24d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 12:50:37 2011</strong>

improve daemon status output (capflam)

Improve the output of the "yaws --status" command by adding session
and connection counters for each group of virtual servers.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e014400" target="_blank">e014400</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e014400');">
    allow Server header customization (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e014400');">
    // Christopher Faulet
  </div>
  <div id="msg-e014400" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e014400');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 12:15:53 2011</strong>

allow Server header customization (capflam)

Add the server_signature directive in the global part of configuration
to allow users to customize the HTTP Server header.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c650d77" target="_blank">c650d77</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c650d77');">
    add shaper directive to control access (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c650d77');">
    // Christopher Faulet
  </div>
  <div id="msg-c650d77" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c650d77');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed May 4 16:43:43 2011</strong>

add shaper directive to control access (capflam)

inspired by the mod_bwshare module of Apache, add the shaper directive
to control access to virtual servers. Access can be controlled based
on the client's IP address. It is also possible to throttle HTTP
requests based on the client's download rate. External modules used to
shape the traffic must implement the new behaviour yaws_shaper.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fd3ca71" target="_blank">fd3ca71</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fd3ca71');">
    allow conf file pathname config with app env var (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fd3ca71');">
    // Christopher Faulet
  </div>
  <div id="msg-fd3ca71" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fd3ca71');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 12:15:15 2011</strong>

allow conf file pathname config with app env var (capflam)

Prior to this change you could customize the path to the configuration
file with the --conf command line argument, otherwise Yaws would use
the default configuration file. With this patch, you can also use the
application environment variable "config" to set the path of the
default configuration file.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9bb7178" target="_blank">9bb7178</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9bb7178');">
    init SSL sockets same as regular TCP sockets (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9bb7178');">
    // Christopher Faulet
  </div>
  <div id="msg-9bb7178" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9bb7178');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 12:14:15 2011</strong>

init SSL sockets same as regular TCP sockets (capflam)

Give ssl:listen the same options as gen_tcp:listen, especially the
reuseaddr flag.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c075573" target="_blank">c075573</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c075573');">
    soft yaws shutdown (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c075573');">
    // Christopher Faulet
  </div>
  <div id="msg-c075573" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c075573');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 15:48:29 2011</strong>

soft yaws shutdown (capflam)

Close listening sockets and stop accepting new connections, and then
try to shutdown all open connections. If, after 60 seconds, some
connections remain alive, kill them. So, shutdown takes longer, but
every process handling requests has a chance to send a response before
stopping.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f983a1b" target="_blank">f983a1b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f983a1b');">
    arg_rewrite mods can now return HTTP responses (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f983a1b');">
    // Christopher Faulet
  </div>
  <div id="msg-f983a1b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f983a1b');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 12:00:27 2011</strong>

arg_rewrite mods can now return HTTP responses (capflam)

Module used to rewrite arg records can now return any HTTP
response. This can be used to redirect requests or to return an
error. To do so, such a module must set the element #arg.state using
the record #rewrite_response{}. The record #rewrite_response{}
contains 3 elements:

  status: any valid HTTP status code
  headers: a list of {header, H}
  content: an iolist

For example, to do an unconditional redirect to http://www.yakaz.com,
you can use the module simple_redir_mod.erl:

  -module(simple_redir_mod.erl).
  -export([arg_rewrite/1]).
  -include_lib("yaws/include/yaws_api.hrl").

  arg_rewrite(Arg) -&gt;
    L = "http://www.yakaz.com",
    H = [{header, {location, L}}],
    RwResp = #rewrite_response{status=301, headers=H}
    Arg#arg{state=RwResp}.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3ad1703" target="_blank">3ad1703</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3ad1703');">
    support external handler to interpret php scripts (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3ad1703');">
    // Christopher Faulet
  </div>
  <div id="msg-3ad1703" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3ad1703');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 16:30:12 2011</strong>

support external handler to interpret php scripts (capflam)

phpfcgi is now deprecated. Instead, you must use php_handler. This
directive is used to customize the handler to interpret .php files. It
can be one of the following definitions:

  - php_handler = &lt;cgi, Filename&gt;
  - php_handler = &lt;fcgi, Host:Port&gt;
  - php_handler = &lt;extern, Module:Function | Node:Module:Function&gt;

If there is no php_handler or deprecated phpfcgi directive, we use the
value of php_exe_path. Its default value is "/usr/bin/php-cgi" but it
can be customized.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7036143" target="_blank">7036143</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7036143');">
    renew sconf record when {page, P} is handled (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7036143');">
    // Christopher Faulet
  </div>
  <div id="msg-7036143" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7036143');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 14:55:05 2011</strong>

renew sconf record when {page, P} is handled (capflam)

Renew sconf record when Yaws return a different page than the one
being requested. This is particularly useful to restart checks on all
docroots.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b65bdb8" target="_blank">b65bdb8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b65bdb8');">
    add status option to page options (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b65bdb8');">
    // Christopher Faulet
  </div>
  <div id="msg-b65bdb8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b65bdb8');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 14:41:48 2011</strong>

add status option to page options (capflam)

Scripts can return {page, {Options, Page}} to make Yaws return a
different page than the one being requested, where Options is a deep
list of options. Previously the only type of option was {header, H}.
This patch adds the type {status, Code} to set the HTTP status code of
the response.

It also delays the accumulation of these headers to preserve it during
the handling of the request on Page. Both 'delayed_regular' and
'delayed_directory' need fewer checks than other requests because
they're used on the second pass and many tests have already been done
during the first pass.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/985491a" target="_blank">985491a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-985491a');">
    allow multiple listen directives per server (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-985491a');">
    // Christopher Faulet
  </div>
  <div id="msg-985491a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-985491a');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed May 11 13:42:44 2011</strong>

allow multiple listen directives per server (capflam)

Sometimes, a virtual server must be bound to several IP
addresses. Previously you had to duplicate virtual server
configuration and change the address to listen on.

Now, multiple Listen directives are allowed without any
duplication. For example, if your server must listen on loopback
address on both IPv4 and IPv6, your server part may be:

  &lt;server www.yakaz.com &gt;
    port = 80
    listen = 127.0.0.1
    listen = ::1
    ...
  &lt;/server&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d9342f8" target="_blank">d9342f8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d9342f8');">
    server-specific logger_mod and auth_mod (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d9342f8');">
    // Christopher Faulet
  </div>
  <div id="msg-d9342f8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d9342f8');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed May 4 15:49:18 2011</strong>

server-specific logger_mod and auth_mod (capflam)

Make logger_mod and auth_mod configuration variables local to each
virtual server. The logger_mod variable can be used to customize
access and auth messages. The auth_log variable previously was global,
but it now lives in the server configuration and the global variable
is deprecated.

The new behaviour yaws_logger must be used to define external modules
to log messages.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4a7c1b1" target="_blank">4a7c1b1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4a7c1b1');">
    support parsing of literal IPv6 addresses (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4a7c1b1');">
    // Christopher Faulet
  </div>
  <div id="msg-4a7c1b1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4a7c1b1');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed May 11 09:25:52 2011</strong>

support parsing of literal IPv6 addresses (capflam)

Support parsing of URLs with literal IPv6 addresses in function
yaws_api:parse_url, like http://[::1]:8080.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a2805a9" target="_blank">a2805a9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a2805a9');">
    allow hard reload of conf without stopping (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a2805a9');">
    // Christopher Faulet
  </div>
  <div id="msg-a2805a9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a2805a9');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 15:02:38 2011</strong>

allow hard reload of conf without stopping (capflam)

Allow a hard reload of the configuration when the server is running,
without stopping the application. This happens when the configuration
has changed too much (e.g. the IP address of virtual server). This is
done by stopping all gserv processes and restarting it with the new
configuration, so it is pretty brutal.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6764e5d" target="_blank">6764e5d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6764e5d');">
    fix gserv counters (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6764e5d');">
    // Christopher Faulet
  </div>
  <div id="msg-6764e5d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6764e5d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 12:35:52 2011</strong>

fix gserv counters (capflam)

Each group of virtual servers (same IP, same port) is represented by
an Erlang process that maintains some counters, but these counters
were not updated correctly. Fix it so they are.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c60fc5f" target="_blank">c60fc5f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c60fc5f');">
    add control for "Expires" and "Cache-Control" response headers (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c60fc5f');">
    // Christopher Faulet
  </div>
  <div id="msg-c60fc5f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c60fc5f');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 16:02:28 2011</strong>

add control for "Expires" and "Cache-Control" response headers (capflam)

Allow an expires list in the server configuration and add "Expires"
and "Cache-Control" headers in responses if the mime type of the
document matches an element of this list. Like the Apache mod_expires
module, the "expires" configuration directive controls the setting of
the Expires HTTP header and the max-age directive of the Cache-Control
HTTP header in server responses. The expiration date can be relative
to either the time the source file was last modified or to the time of
the client access. It is possible to have multiple expires directives
for a virtual server.

Here is an example:

  &lt;server www.yakaz.com &gt;
    ...
    expires = &lt;image/gif, access+2592000&gt; &lt;image/png, access+2592000&gt;
    expires = &lt;image/jpeg, access+2592000&gt; &lt;text/css, access+2592000&gt;
    expires = &lt;application/javascript, modify+2592000&gt;
    ...
  &lt;/server&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2b603fc" target="_blank">2b603fc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2b603fc');">
    add "application/javascript" as a compressible mime type (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2b603fc');">
    // Christopher Faulet
  </div>
  <div id="msg-2b603fc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2b603fc');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 12:13:24 2011</strong>

add "application/javascript" as a compressible mime type (capflam)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b955bca" target="_blank">b955bca</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b955bca');">
    use "partial_post_size" for chunked requests (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b955bca');">
    // Christopher Faulet
  </div>
  <div id="msg-b955bca" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b955bca');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 12:10:49 2011</strong>

use "partial_post_size" for chunked requests (capflam)

Chunked requests were read entirely into memory, so for very large
POSTs Erlang could run out of memory. Use the config value
"partial_post_size" to read these requests in blocks, same as for
non-chunked requests.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ab09643" target="_blank">ab09643</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ab09643');">
    authorization improvements (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ab09643');">
    // Christopher Faulet
  </div>
  <div id="msg-ab09643" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ab09643');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed May 4 08:58:32 2011</strong>

authorization improvements (capflam)

There are two changes here. The first improves the implementation of
authentication mechanisms, like authentication inside all docroots and
recursion through subdirectories (if you define an authentication
policy for a directory, then it is automatically applied to any
subdirectories, except those with their own authentication policy). It
also adds the docroot directive in auth structures (or .yaws_auth
files) to restrict it to a specific docroot (no docroot configured
means all docroots). We can now also configure authentication for
specific files instead of just directories.

The second one adds ACLs (Access Control List), like Apache's
mod_access module, to protect data. You can add directives in auth
structures (or .yaws_auth files) to control access to particular parts
of the server based on the client IP address. The allow and deny
directives are used to specify which clients are or are not allowed to
access to the server, while the order directive sets the default
access state and configures how the allow and deny directives interact
with each other. Use "/" as default directory in auth structure;
yaws.conf manpage has been updated accordingly.

Here is an example:

  &lt;server www.yakaz.com &gt;
    ...
    &lt;auth&gt;
      docroot = /var/www/yakaz/admin
      deny    = all
      allow   = 127.0.0.1, 192.168.0.0/24
      order   = deny,allow
    &lt;/auth&gt;
    ...
  &lt;/server&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bc1a16b" target="_blank">bc1a16b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bc1a16b');">
    add yaws.appup.src template file (capflam)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bc1a16b');">
    // Christopher Faulet
  </div>
  <div id="msg-bc1a16b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bc1a16b');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue May 3 12:23:27 2011</strong>

add yaws.appup.src template file (capflam)

Add yaws.appup.src template file used to build the final yaws.appup
file. Also add ebin/yaws.appup to .gitignore.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/739fcd6" target="_blank">739fcd6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-739fcd6');">
    added contributors.txt
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-739fcd6');">
    // Claes Wikstrom
  </div>
  <div id="msg-739fcd6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-739fcd6');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sat May 21 13:39:31 2011</strong>

added contributors.txt
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3385b0a" target="_blank">3385b0a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3385b0a');">
    minor edits in yaws.tex
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3385b0a');">
    // Steve Vinoski
  </div>
  <div id="msg-3385b0a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3385b0a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon May 16 00:20:30 2011</strong>

minor edits in yaws.tex
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1aeaccd" target="_blank">1aeaccd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1aeaccd');">
    check for install dirs before calling mkdir
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1aeaccd');">
    // Steve Vinoski
  </div>
  <div id="msg-1aeaccd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1aeaccd');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue May 10 04:12:56 2011</strong>

check for install dirs before calling mkdir

In the new scripts/regular-install script, check for directory
existence before calling mkdir.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bccfd7f" target="_blank">bccfd7f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bccfd7f');">
    fix rebar-related installation errors
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bccfd7f');">
    // Steve Vinoski
  </div>
  <div id="msg-bccfd7f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bccfd7f');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun May 8 22:41:38 2011</strong>

fix rebar-related installation errors

Fix install-related errors caused by refactoring for rebar. One error
was that the new gen-yaws-conf script was trying to create directories
needed for the local-install script, and the other was that the
regular-install script was attempting to create a directory named
$DESTDIR, which is typically an empty string.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/61a1116" target="_blank">61a1116</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-61a1116');">
    more build avoidance for rebar build
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-61a1116');">
    // Steve Vinoski
  </div>
  <div id="msg-61a1116" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-61a1116');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun May 8 05:20:54 2011</strong>

more build avoidance for rebar build

Modify scripts/rebar-pre-scripts and scripts/local-install to avoid
rebuilding generated files that haven't changed since the last
build. Modify scripts/Makefile to account for local-install changes.

Also apply minor fix to rebar.config indentation.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/21d0299" target="_blank">21d0299</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-21d0299');">
    epam: removed unused variable
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-21d0299');">
    // Tuncer Ayaz
  </div>
  <div id="msg-21d0299" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-21d0299');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Wed May 4 23:47:11 2011</strong>

epam: removed unused variable
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bedbc05" target="_blank">bedbc05</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bedbc05');">
    add rebar support (Tuncer Ayaz and Steve Vinoski)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bedbc05');">
    // Tuncer Ayaz
  </div>
  <div id="msg-bedbc05" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bedbc05');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Sat May 7 15:09:03 2011</strong>

add rebar support (Tuncer Ayaz and Steve Vinoski)

Add support for building yaws with rebar. The original configure and
make support is kept intact.

If you build with rebar you get a local install. The rebar approach
does not support a regular install, which defaults into /usr/local. If
you want a regular install, use configure and make.

Create a new top-level contrib directory and move unused src files
there. Also move src/benchmarks and src/contrib contents there as
well. Remove the obsolete src/patches directory. This is all to keep
rebar from compiling this unused code (since by default it compiles
everything under the src dir).

Move a number of build rules out of Makefiles into separate scripts so
they can be used by both rebar and make.

Modify yaws version specifier and handling to be amenable to rebar.

Clean up trailing whitespace in a number of scripts and Makefiles.

Use the following environment variables to customize the rebar build
defaults:

DEFAULT_CHARSET: used in mime type table (default: "")
ERLBINDIR: e.g. /usr/local/bin
ETCDIR: etc directory (default: ./etc)
VARDIR: var directory (default: ./var)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d8cf062" target="_blank">d8cf062</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d8cf062');">
    augment index.yaws and news
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d8cf062');">
    // Steve Vinoski
  </div>
  <div id="msg-d8cf062" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d8cf062');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 7 14:28:50 2011</strong>

augment index.yaws and news

Add a link to the Yaws github repo to the Yaws front page.

Add a note about JSON-RPC 2.0 changes to the news page.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5ca422e" target="_blank">5ca422e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5ca422e');">
    fix object lookup problem in jsonrpc
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5ca422e');">
    // Steve Vinoski
  </div>
  <div id="msg-5ca422e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5ca422e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 7 06:26:30 2011</strong>

fix object lookup problem in jsonrpc

Fix a matching problem causing lookups to fail in JSON objects used
for JSON-RPC calls.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b16f002" target="_blank">b16f002</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b16f002');">
    fix warning introduced in JSON-RPC 2.0 changes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b16f002');">
    // Steve Vinoski
  </div>
  <div id="msg-b16f002" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b16f002');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue May 3 01:10:49 2011</strong>

fix warning introduced in JSON-RPC 2.0 changes
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/15454bc" target="_blank">15454bc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-15454bc');">
    JSON-RPC 2.0 support
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-15454bc');">
    // Steve Vinoski
  </div>
  <div id="msg-15454bc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-15454bc');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon May 2 02:14:33 2011</strong>

JSON-RPC 2.0 support

Convert existing JSON-RPC support to version 2.0, supporting all
features mentioned here:

http://groups.google.com/group/json-rpc/web/json-rpc-2-0

Add all the examples from that webpage as new tests.

Modify documentation to note that version 2.0 of JSON-RPC is now
supported.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/96534b2" target="_blank">96534b2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-96534b2');">
    replace json.erl with json2.erl to fix issue 50
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-96534b2');">
    // Steve Vinoski
  </div>
  <div id="msg-96534b2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-96534b2');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 24 06:25:48 2011</strong>

replace json.erl with json2.erl to fix issue 50

Yaws issue 50 (https://github.com/klacke/yaws/issues/50) notes that
the json.erl module uses list_to_atom and is therefore dangerous due
to the possibility of filling the atom table. Also, the test function
in json.erl has been broken for quite some time. Deprecate json.erl
and add json2.erl, which avoids list_to_atom. Repair the json2:test/0
function inherited from json.erl and add it to the t1 test
suite. Change www/json_intro.yaws to refer to json2.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5d8fd4a" target="_blank">5d8fd4a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5d8fd4a');">
    improve error message for setting process ulimit
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5d8fd4a');">
    // Steve Vinoski
  </div>
  <div id="msg-5d8fd4a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5d8fd4a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 20 03:22:20 2011</strong>

improve error message for setting process ulimit

The test/t1/runtest script attempts to raise the process ulimit if
it's too low to run its load test. Improve the error message it emits
if it can't set the ulimit to the desired level.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/455578a" target="_blank">455578a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-455578a');">
    major trailing whitespace cleanup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-455578a');">
    // Steve Vinoski
  </div>
  <div id="msg-455578a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-455578a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 20 02:49:18 2011</strong>

major trailing whitespace cleanup

Remove trailing whitespace in all .erl and .hrl files in the
repository.

If you're an emacs user, you can easily see trailing whitespace using
settings like these in your ~/.emacs file:

(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "slate gray")

You can also delete trailing whitespace automatically when you save
your Erlang sources by setting the emacs before-save-hook in your
~/.emacs file like this:

(add-hook 'before-save-hook
          '(lambda () (if (eq major-mode 'erlang-mode)
                          (delete-trailing-whitespace))))
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f74f67a" target="_blank">f74f67a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f74f67a');">
    set HTTP_HOST properly for CGI
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f74f67a');">
    // Steve Vinoski
  </div>
  <div id="msg-f74f67a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f74f67a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Apr 18 16:58:22 2011</strong>

set HTTP_HOST properly for CGI

Set HTTP_HOST to the value of the HTTP Host header, rather than just
setting it to the server hostname. Thanks to Roman Arefiev for
tracking down the fact that HTTP_HOST was set incorrectly.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4aa146b" target="_blank">4aa146b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4aa146b');">
    fix dialyzer problems in yaws_sendfile changes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4aa146b');">
    // Steve Vinoski
  </div>
  <div id="msg-4aa146b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4aa146b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 17 06:04:06 2011</strong>

fix dialyzer problems in yaws_sendfile changes
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4872cd7" target="_blank">4872cd7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4872cd7');">
    delete ets table in terminate
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4872cd7');">
    // Steve Vinoski
  </div>
  <div id="msg-4872cd7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4872cd7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 17 04:43:06 2011</strong>

delete ets table in terminate

Delete the caller table in terminate, and fix the terminate return
value.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d75acaf" target="_blank">d75acaf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d75acaf');">
    restore overflow handling dropped in previous commit
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d75acaf');">
    // Steve Vinoski
  </div>
  <div id="msg-d75acaf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d75acaf');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Apr 16 23:56:05 2011</strong>

restore overflow handling dropped in previous commit

Restore handling of eoverflow errors in sendfile, which was
accidentally left out of the previous commit.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6a4c4e7" target="_blank">6a4c4e7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6a4c4e7');">
    change sendfile driver handler to a gen_server
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6a4c4e7');">
    // Steve Vinoski
  </div>
  <div id="msg-6a4c4e7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6a4c4e7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Apr 16 23:26:36 2011</strong>

change sendfile driver handler to a gen_server

Rather than use a custom message handling loop, make the yaws_sendfile
driver handler a gen_server. Collapse the yaws_sendfile_compat
functionality directly into yaws_sendfile and remove
yaws_sendfile_compat, and change all references to
yaws_sendfile_compat to refer to yaws_sendfile instead.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a425e71" target="_blank">a425e71</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a425e71');">
    fully qualify call to erlang:max
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a425e71');">
    // Steve Vinoski
  </div>
  <div id="msg-a425e71" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a425e71');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 13 12:57:19 2011</strong>

fully qualify call to erlang:max
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7add0cf" target="_blank">7add0cf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7add0cf');">
    update mime.types from recent Apache web server sources
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7add0cf');">
    // Steve Vinoski
  </div>
  <div id="msg-7add0cf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7add0cf');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 13 06:06:48 2011</strong>

update mime.types from recent Apache web server sources

Also eliminate trailing whitespace in the file.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6eee2aa" target="_blank">6eee2aa</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6eee2aa');">
    remove leftover debug code in yaws_api.erl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6eee2aa');">
    // Steve Vinoski
  </div>
  <div id="msg-6eee2aa" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6eee2aa');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 13 05:26:00 2011</strong>

remove leftover debug code in yaws_api.erl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d9a795a" target="_blank">d9a795a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d9a795a');">
    add binary option to multipart/form-data parsing
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d9a795a');">
    // Steve Vinoski
  </div>
  <div id="msg-d9a795a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d9a795a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 12 21:58:11 2011</strong>

add binary option to multipart/form-data parsing

Add optional arguments list and binary (passed as atoms) to specify
whether the uploaded file data should be returned in list form or
binary form, respectively. By default
yaws_multipart:read_multipart_form returns uploaded file data in list
form to preserve backwards compatibility. But using the binary option
can result in yaws requiring less memory for handling file uploads.

The same options can now also be passed to
yaws_api:parse_multipart_post.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b6f774f" target="_blank">b6f774f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b6f774f');">
    Removed the contribs stuff from the front webpage, it was lame and inaccurate
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b6f774f');">
    // Claes Wikstrom
  </div>
  <div id="msg-b6f774f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b6f774f');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Apr 12 15:03:01 2011</strong>

Removed the contribs stuff from the front webpage, it was lame and inaccurate
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/501ad5d" target="_blank">501ad5d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-501ad5d');">
    rewrite multipart/form-data POST handling
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-501ad5d');">
    // Steve Vinoski
  </div>
  <div id="msg-501ad5d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-501ad5d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 10 22:06:28 2011</strong>

rewrite multipart/form-data POST handling

Issue 38 (https://github.com/klacke/yaws/issues/38) states that
POSTing large files crashes Yaws due to out-of-memory (OOM). I found
that POSTing just a half meg file will drive Yaws to OOM on my 3 GB
Macbook Pro.

Rewrite the multipart/form-data handling (which is almost 10 years old
at this point). Avoid converting the data to lists for processing, and
avoid processing the data byte-by-byte. Use Boyer-Moore searching to
find data boundaries, and use erlang:decode_packet to handle part
headers.

Unfortunately binaries are still converted to lists after all
processing is completed. This is required for backward compatibility,
but it means a lot of memory still gets used to handle large file
uploads, even though it's not as much memory as was previously
required.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f922e85" target="_blank">f922e85</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f922e85');">
    add applications/wiki/scripts files to .gitignore
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f922e85');">
    // Steve Vinoski
  </div>
  <div id="msg-f922e85" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f922e85');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 10 22:04:48 2011</strong>

add applications/wiki/scripts files to .gitignore
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1edce78" target="_blank">1edce78</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1edce78');">
    no code changes, just cleanup trailing whitespace
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1edce78');">
    // Steve Vinoski
  </div>
  <div id="msg-1edce78" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1edce78');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 6 23:14:08 2011</strong>

no code changes, just cleanup trailing whitespace
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4539905" target="_blank">4539905</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4539905');">
    Adding mime type for WOFF font files.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4539905');">
    // Torbjorn Tornkvist
  </div>
  <div id="msg-4539905" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4539905');">
    <pre>
<strong>Author: Torbjorn Tornkvist &lt;tobbe@tornkvist.org&gt;</strong>
<strong>Date:   Mon Apr 4 12:36:07 2011</strong>

Adding mime type for WOFF font files.

This according to: http://www.w3.org/TR/WOFF/#appendix-b
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3f08480" target="_blank">3f08480</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3f08480');">
    add configurable access logging
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3f08480');">
    // Steve Vinoski
  </div>
  <div id="msg-3f08480" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3f08480');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 3 20:59:33 2011</strong>

add configurable access logging

Add the global logger_mod config variable to allow for customized
access logging. Document logger_mod in yaws.conf.5 and yaws.tex.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/799f0c7" target="_blank">799f0c7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-799f0c7');">
    make RUNTEST_CURLS reflect actual number of curl test clients
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-799f0c7');">
    // Steve Vinoski
  </div>
  <div id="msg-799f0c7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-799f0c7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Mar 25 16:32:35 2011</strong>

make RUNTEST_CURLS reflect actual number of curl test clients
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/870ef05" target="_blank">870ef05</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-870ef05');">
    fix yaws_server dialyzer issues
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-870ef05');">
    // Steve Vinoski
  </div>
  <div id="msg-870ef05" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-870ef05');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Mar 19 01:50:40 2011</strong>

fix yaws_server dialyzer issues

In yaws_server the old function ut_open used to return either {bin,
Bin} or an open file handle depending on how it was called. A number
of other areas of the yaws_server code also handled either a file
handle or {bin, Bin}. The ut_open function was changed in June 2002 to
deal only with binaries, and dialyzer correctly pointed out that a
number of areas of the code were obsolete as a result. Change
yaws_server to get rid of ut_read and ut_close, then rename ut_open to
ut_read and have it just return a plain binary. Remove all vestiges of
code relying on the old behavior related to open file handles.

This clears up all known dialyzer issues in yaws_server.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f43aa7d" target="_blank">f43aa7d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f43aa7d');">
    correction
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f43aa7d');">
    // Claes Wikstrom
  </div>
  <div id="msg-f43aa7d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f43aa7d');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sun Mar 6 22:26:58 2011</strong>

correction
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/525846e" target="_blank">525846e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-525846e');">
    Some dialyzer fixes - no real bugs, just enhancements, removed the deprecated feature of fdsrv, and fixed some bugs recently introduced by Tobbe and yaws_session_server
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-525846e');">
    // Claes Wikstrom
  </div>
  <div id="msg-525846e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-525846e');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sat Mar 5 13:13:04 2011</strong>

Some dialyzer fixes - no real bugs, just enhancements, removed the deprecated feature of fdsrv, and fixed some bugs recently introduced by Tobbe and yaws_session_server
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/190ea50" target="_blank">190ea50</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-190ea50');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-190ea50');">
    // Claes Wikstrom
  </div>
  <div id="msg-190ea50" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-190ea50');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sat Mar 5 00:24:50 2011</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b1e886f" target="_blank">b1e886f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b1e886f');">
    compiler warning
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b1e886f');">
    // Claes Wikstrom
  </div>
  <div id="msg-b1e886f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b1e886f');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sat Mar 5 00:24:33 2011</strong>

compiler warning
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ce93c84" target="_blank">ce93c84</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ce93c84');">
    Adding access functions for various Yaws records.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ce93c84');">
    // Torbjorn Tornkvist
  </div>
  <div id="msg-ce93c84" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ce93c84');">
    <pre>
<strong>Author: Torbjorn Tornkvist &lt;tobbe@klarna.com&gt;</strong>
<strong>Date:   Sat Mar 5 00:18:50 2011</strong>

Adding access functions for various Yaws records.

By adding these access functions for extracting data out from
some of the Yaws records (e.g the #arg{} record, it is possible
for other code to avoid having a dependency to any Yaws .hrl files.

For example: SimpleBridge can now implement its wrappers without
having to include yaws_api.hrl.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d2d461c" target="_blank">d2d461c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d2d461c');">
    Figuring out proper yaws_session_server backend.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d2d461c');">
    // Torbjorn Tornkvist
  </div>
  <div id="msg-d2d461c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d2d461c');">
    <pre>
<strong>Author: Torbjorn Tornkvist &lt;tobbe@tornkvist.org&gt;</strong>
<strong>Date:   Fri Mar 4 09:38:24 2011</strong>

Figuring out proper yaws_session_server backend.

This fixes the problem with getting hold of the yaws_session_server
backend when running in embedded mode. In this mode, no #gconf{}
record may be available when the yaws_session_server want to figure
out what backend to use. The fix stores the 'pending' configuration
as a yaws appplication environment variable 'embedded_conf', which
makes it possible for the yaws_session_server to get hold of the
backend when starting up. If still no #gconf{} can be obtained,
we will fallback to the default yaws_session_server backend.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b0514f5" target="_blank">b0514f5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b0514f5');">
    Fixing up the start_embedded code.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b0514f5');">
    // Torbjorn Tornkvist
  </div>
  <div id="msg-b0514f5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b0514f5');">
    <pre>
<strong>Author: Torbjorn Tornkvist &lt;tobbe@tornkvist.org&gt;</strong>
<strong>Date:   Fri Mar 4 09:34:07 2011</strong>

Fixing up the start_embedded code.

Add an explicit check for successfully starting Yaws.
Adding proper setup of authdirs in start_embedded.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9791223" target="_blank">9791223</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9791223');">
    indent
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9791223');">
    // Claes Wikstrom
  </div>
  <div id="msg-9791223" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9791223');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Mar 3 23:41:54 2011</strong>

indent
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6e96a14" target="_blank">6e96a14</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6e96a14');">
    indendation cleanup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6e96a14');">
    // Claes Wikstrom
  </div>
  <div id="msg-6e96a14" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6e96a14');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Mar 3 16:06:39 2011</strong>

indendation cleanup
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c90c603" target="_blank">c90c603</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c90c603');">
    ugly indent, plus a dialyzer warning for dead code
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c90c603');">
    // Claes Wikstrom
  </div>
  <div id="msg-c90c603" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c90c603');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Mar 3 12:54:18 2011</strong>

ugly indent, plus a dialyzer warning for dead code
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3d1c7df" target="_blank">3d1c7df</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3d1c7df');">
    avoid keeping our own copy of ibrowse
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3d1c7df');">
    // Steve Vinoski
  </div>
  <div id="msg-3d1c7df" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3d1c7df');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Mar 3 04:54:47 2011</strong>

avoid keeping our own copy of ibrowse

Remove our copy of ibrowse in our test directory from git
control. Instead, download ibrowse master from github if we don't have
a fresh copy. Track master HEAD to make sure we have the latest.

Change test/Makefile to fetch ibrowse if needed as part of building
the "all" target.

Add test/ibrowse to .gitignore.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/28cb08d" target="_blank">28cb08d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-28cb08d');">
    make "make clean" also clean the test directory
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-28cb08d');">
    // Steve Vinoski
  </div>
  <div id="msg-28cb08d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-28cb08d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Mar 3 02:58:48 2011</strong>

make "make clean" also clean the test directory
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/65bf36a" target="_blank">65bf36a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-65bf36a');">
    remove support for SOAP DIME attachments
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-65bf36a');">
    // Steve Vinoski
  </div>
  <div id="msg-65bf36a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-65bf36a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Mar 3 02:49:41 2011</strong>

remove support for SOAP DIME attachments

Remove DIME support. We're cleaning up dialyzer errors in Yaws and the
DIME code had some problems, so we decided to drop it rather than try
to fix it since we don't believe anyone's using it. Microsoft has
dropped support for DIME anyway, recommending MTOM instead.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6c982a7" target="_blank">6c982a7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6c982a7');">
    do not use ERL_DRV_USE when de-selecting sendfile socket
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6c982a7');">
    // Steve Vinoski
  </div>
  <div id="msg-6c982a7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6c982a7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Mar 2 20:38:48 2011</strong>

do not use ERL_DRV_USE when de-selecting sendfile socket

The sendfile driver does not own the HTTP sockets to which it sends
data, and so it never closes them. The previous commit erroneously set
ERL_DRV_USE on the driver_select(..., 0) call, which causes erts to
justifiably complain about the driver not implementing the stop_select
call. Remove ERL_DRV_USE for this case. Thanks to Tuncer Ayaz.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3a5b906" target="_blank">3a5b906</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3a5b906');">
    fix EINTR handling in sendfile driver
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3a5b906');">
    // Steve Vinoski
  </div>
  <div id="msg-3a5b906" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3a5b906');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Mar 2 17:36:12 2011</strong>

fix EINTR handling in sendfile driver

Tuncer Ayaz pointed out that OS X sendfile will return bytes sent for
the case when EINTR occurs. Make sendfile driver handle this case by
treating EINTR the same as EAGAIN for all supported platforms.

Also update driver to use ERL_DRV_* flags in driver_select calls if
they're defined, rather than the old DO_* flags. This is considered
correct practice since R13B.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/db682d3" target="_blank">db682d3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-db682d3');">
    add test target to top-level Makefile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-db682d3');">
    // Steve Vinoski
  </div>
  <div id="msg-db682d3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-db682d3');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Mar 2 17:31:51 2011</strong>

add test target to top-level Makefile

Add test target to top-level Makefile to allow tests to be built and
run from that level.

Also fix the start target in test/support/include.mk.in to only try to
start a yaws instance if the current working directory contains a
yaws.conf file. The lack of this check was preventing half the tests
from running under "make test".
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f0c19f6" target="_blank">f0c19f6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f0c19f6');">
    change the previous dialyzer fix (thanks to Kostis)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f0c19f6');">
    // Steve Vinoski
  </div>
  <div id="msg-f0c19f6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f0c19f6');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Mar 2 15:19:41 2011</strong>

change the previous dialyzer fix (thanks to Kostis)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c833e54" target="_blank">c833e54</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c833e54');">
    comment out unused ssl functions
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c833e54');">
    // Steve Vinoski
  </div>
  <div id="msg-c833e54" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c833e54');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Mar 1 23:34:20 2011</strong>

comment out unused ssl functions

Comment out the ssl_fail_if_no_peer_cert functions to eliminate
warnings about them being unused.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b6ffe23" target="_blank">b6ffe23</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b6ffe23');">
    fix more dialyzer warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b6ffe23');">
    // Steve Vinoski
  </div>
  <div id="msg-b6ffe23" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b6ffe23');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Mar 1 22:41:07 2011</strong>

fix more dialyzer warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1fcdfd7" target="_blank">1fcdfd7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1fcdfd7');">
    removed unused post/query parsing code
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1fcdfd7');">
    // Claes Wikstrom
  </div>
  <div id="msg-1fcdfd7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1fcdfd7');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Mar 1 22:01:38 2011</strong>

removed unused post/query parsing code
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3a55037" target="_blank">3a55037</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3a55037');">
    yaws_cgi dialyzer cleanup (thanks to Kostis Sagonas)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3a55037');">
    // Steve Vinoski
  </div>
  <div id="msg-3a55037" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3a55037');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Feb 28 21:25:48 2011</strong>

yaws_cgi dialyzer cleanup (thanks to Kostis Sagonas)

Clean up dialyzer warnings. Make sure fcgi_receive_binary return value
used consistently everywhere. Comment out code used for
as-yet-unsupported FCGI features to avoid dialyzer warnings.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8493ded" target="_blank">8493ded</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8493ded');">
    Use a binary comprehension instead of god old list_to_binary as per suggestion from Kostis
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8493ded');">
    // Claes Wikstrom
  </div>
  <div id="msg-8493ded" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8493ded');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Feb 28 08:45:33 2011</strong>

Use a binary comprehension instead of god old list_to_binary as per suggestion from Kostis
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d744060" target="_blank">d744060</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d744060');">
    Fix Dialyzer warning: remove dead code
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d744060');">
    // Tuncer Ayaz
  </div>
  <div id="msg-d744060" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d744060');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Thu Feb 24 12:48:48 2011</strong>

Fix Dialyzer warning: remove dead code
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2f8a8c6" target="_blank">2f8a8c6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2f8a8c6');">
    Fix unused clause Dialyzer warnings in yaws_rss
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2f8a8c6');">
    // Tuncer Ayaz
  </div>
  <div id="msg-2f8a8c6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2f8a8c6');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Wed Feb 23 03:28:25 2011</strong>

Fix unused clause Dialyzer warnings in yaws_rss
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6f61f74" target="_blank">6f61f74</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6f61f74');">
    Fix zlib:deflate/3 Dialyzer warning
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6f61f74');">
    // Tuncer Ayaz
  </div>
  <div id="msg-6f61f74" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6f61f74');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Wed Feb 23 03:17:25 2011</strong>

Fix zlib:deflate/3 Dialyzer warning
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9e516ba" target="_blank">9e516ba</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9e516ba');">
    Fix calls to undefined functions
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9e516ba');">
    // Tuncer Ayaz
  </div>
  <div id="msg-9e516ba" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9e516ba');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Sun Feb 20 14:50:28 2011</strong>

Fix calls to undefined functions
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/438cfb1" target="_blank">438cfb1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-438cfb1');">
    Fix deprecated guard warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-438cfb1');">
    // Tuncer Ayaz
  </div>
  <div id="msg-438cfb1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-438cfb1');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Sun Feb 20 14:32:31 2011</strong>

Fix deprecated guard warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4637975" target="_blank">4637975</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4637975');">
    dialyzer
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4637975');">
    // Claes Wikstrom
  </div>
  <div id="msg-4637975" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4637975');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Feb 25 23:50:32 2011</strong>

dialyzer
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/dad71b7" target="_blank">dad71b7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-dad71b7');">
    more dialyzer bugs, soap_srv startup code that could never run
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-dad71b7');">
    // Claes Wikstrom
  </div>
  <div id="msg-dad71b7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-dad71b7');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Feb 25 23:45:32 2011</strong>

more dialyzer bugs, soap_srv startup code that could never run
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8e64d3b" target="_blank">8e64d3b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8e64d3b');">
    Some minor bugs, and some dead code removed, all found by Tuncers run of dialyzer
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8e64d3b');">
    // Claes Wikstrom
  </div>
  <div id="msg-8e64d3b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8e64d3b');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Feb 25 17:14:40 2011</strong>

Some minor bugs, and some dead code removed, all found by Tuncers run of dialyzer
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/df47d9c" target="_blank">df47d9c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-df47d9c');">
    compile bug
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-df47d9c');">
    // Claes Wikstrom
  </div>
  <div id="msg-df47d9c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-df47d9c');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Feb 25 16:49:33 2011</strong>

compile bug
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/88915e6" target="_blank">88915e6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-88915e6');">
    io:format leftover
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-88915e6');">
    // Claes Wikstrom
  </div>
  <div id="msg-88915e6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-88915e6');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Feb 24 12:32:28 2011</strong>

io:format leftover
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f73d23d" target="_blank">f73d23d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f73d23d');">
    Fixed dialyzer bugs found by Tuncer, Issue #54
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f73d23d');">
    // Claes Wikstrom
  </div>
  <div id="msg-f73d23d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f73d23d');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Feb 23 22:30:02 2011</strong>

Fixed dialyzer bugs found by Tuncer, Issue #54
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5ea40d4" target="_blank">5ea40d4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5ea40d4');">
    eliminate redundant check for erl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5ea40d4');">
    // Steve Vinoski
  </div>
  <div id="msg-5ea40d4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5ea40d4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Feb 17 01:28:54 2011</strong>

eliminate redundant check for erl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e7e6404" target="_blank">e7e6404</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e7e6404');">
    configure ERLBINDIR relative to ERLDIR
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e7e6404');">
    // James Lee
  </div>
  <div id="msg-e7e6404" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e7e6404');">
    <pre>
<strong>Author: James Lee &lt;jameslee@gwu.edu&gt;</strong>
<strong>Date:   Wed Feb 16 20:14:29 2011</strong>

configure ERLBINDIR relative to ERLDIR

Rather than configuring ERLBINDIR relative to ERL, which doesn't work
when ERL is in a strange location such as /usr/bin/amd64/erl,
configure it relative to ERLDIR.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/98f8db9" target="_blank">98f8db9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-98f8db9');">
    Fix configure failing to determine word size on Linux platform.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-98f8db9');">
    // Yurii Rashkovskii
  </div>
  <div id="msg-98f8db9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-98f8db9');">
    <pre>
<strong>Author: Yurii Rashkovskii &lt;yrashk@gmail.com&gt;</strong>
<strong>Date:   Mon Feb 14 09:45:47 2011</strong>

Fix configure failing to determine word size on Linux platform.

It was not explicitly specifying that it needs Erlang to run the test so autoconf was trying to
compile that halt(...) code test as a C program, which obviously leads to a failure
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6b56677" target="_blank">6b56677</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6b56677');">
    filter erl output for setting ERLDIR
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6b56677');">
    // Steve Vinoski
  </div>
  <div id="msg-6b56677" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6b56677');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Jan 26 21:54:04 2011</strong>

filter erl output for setting ERLDIR

A user reported a problem with yaws-1.89 where ERLDIR was getting set
incorrectly because his R14B01 Erlang installation was emitting the
string "Running Erlang" as part of the output used to set the ERLDIR
variable. (I don't know why, since that string doesn't appear anywhere
in the sources as far as I can see.) Change the command used to set
ERLDIR so that only the last line of output is kept.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3b52160" target="_blank">3b52160</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3b52160');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3b52160');">
    // Steve Vinoski
  </div>
  <div id="msg-3b52160" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3b52160');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Jan 18 04:40:27 2011</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3cdb645" target="_blank">3cdb645</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3cdb645');">
    teach linux build to distinguish 32- and 64-bit erlang
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3cdb645');">
    // Steve Vinoski
  </div>
  <div id="msg-3cdb645" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3cdb645');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Jan 18 04:16:20 2011</strong>

teach linux build to distinguish 32- and 64-bit erlang

Change Linux build to check whether the Erlang system available is
32-bit or 64-bit and use the right build flags for c_src to
match. This support is mostly the same as changes added some time ago
to distinguish 32- and 64-bit builds on OS X.

Also change configure script to use "host" where "target" used to be
used, since according to autotools documentation "host" is what was
really meant.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2d74b84" target="_blank">2d74b84</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2d74b84');">
    Added a comment in yaws.hrl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2d74b84');">
    // Nicolas Thauvin
  </div>
  <div id="msg-2d74b84" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2d74b84');">
    <pre>
<strong>Author: Nicolas Thauvin &lt;qux@free.fr&gt;</strong>
<strong>Date:   Thu Jan 6 01:25:44 2011</strong>

Added a comment in yaws.hrl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c6f26aa" target="_blank">c6f26aa</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c6f26aa');">
    Pluggable ysession storage with mnesia sample in src/contrib, part 2
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c6f26aa');">
    // Nicolas Thauvin
  </div>
  <div id="msg-c6f26aa" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c6f26aa');">
    <pre>
<strong>Author: Nicolas Thauvin &lt;qux@free.fr&gt;</strong>
<strong>Date:   Thu Jan 6 01:23:08 2011</strong>

Pluggable ysession storage with mnesia sample in src/contrib, part 2
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0761bb2" target="_blank">0761bb2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0761bb2');">
    Pluggable ysession storage with mnesia sample in src/contrib
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0761bb2');">
    // Nicolas Thauvin
  </div>
  <div id="msg-0761bb2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0761bb2');">
    <pre>
<strong>Author: Nicolas Thauvin &lt;qux@free.fr&gt;</strong>
<strong>Date:   Thu Jan 6 01:15:26 2011</strong>

Pluggable ysession storage with mnesia sample in src/contrib
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3aac349" target="_blank">3aac349</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3aac349');">
    openbsd port
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3aac349');">
    // Claes Wikstrom
  </div>
  <div id="msg-3aac349" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3aac349');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Dec 29 13:16:25 2010</strong>

openbsd port
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2e97498" target="_blank">2e97498</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2e97498');">
    add manpage entry for x_forwarded_for_log_proxy_whitelist
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2e97498');">
    // Fabian Linzberger
  </div>
  <div id="msg-2e97498" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2e97498');">
    <pre>
<strong>Author: Fabian Linzberger &lt;e@lefant.net&gt;</strong>
<strong>Date:   Tue Nov 9 17:11:18 2010</strong>

add manpage entry for x_forwarded_for_log_proxy_whitelist

(As part of pulling from Fabian's branch to get this change I also
repaired some formatting for another entry and amended the
commit. --steve)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1c3afb1" target="_blank">1c3afb1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1c3afb1');">
    Fix 'implicit declaration of function' warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1c3afb1');">
    // Tuncer Ayaz
  </div>
  <div id="msg-1c3afb1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1c3afb1');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Thu Nov 25 07:37:53 2010</strong>

Fix 'implicit declaration of function' warnings

c_src/yaws_sendfile_drv.c: In function 'set_error_buffer':
c_src/yaws_sendfile_drv.c:142:9: warning: implicit declaration of
                                          function 'tolower'
c_src/yaws_sendfile_drv.c: In function
                              'yaws_sendfile_drv_ready_output':
c_src/yaws_sendfile_drv.c:259:9: warning: implicit declaration of
                                          function 'close'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/35a5c1a" target="_blank">35a5c1a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-35a5c1a');">
    add configure detection of erts bits support
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-35a5c1a');">
    // Steve Vinoski
  </div>
  <div id="msg-35a5c1a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-35a5c1a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Dec 8 05:41:34 2010</strong>

add configure detection of erts bits support

The websockets support relies on bits support in Erlang. The
src/Makefile used to contain a gmake shell command for checking the
erts version to see if bits support was present. This change moves
that test into the configure scripts and adds a new make variable to
include.mk that src/Makefile can use instead. This keeps configuration
checks out of the makefiles and in the configure script where they
belong.

Also fixed the test for bits support, which would have been incorrect
once the erts major version number increases above 5 because it also
unconditionally wanted the erts minor number to be greater than 6, so
a major/minor version such as 6.0 would have failed.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7d10e6f" target="_blank">7d10e6f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7d10e6f');">
    Typo fix in previous commit
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7d10e6f');">
    // Peter Lemenkov
  </div>
  <div id="msg-7d10e6f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7d10e6f');">
    <pre>
<strong>Author: Peter Lemenkov &lt;lemenkov@gmail.com&gt;</strong>
<strong>Date:   Thu Nov 18 10:53:34 2010</strong>

Typo fix in previous commit

Module yaws_dime should be listed w/o extension.

Signed-off-by: Peter Lemenkov &lt;lemenkov@gmail.com&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5708168" target="_blank">5708168</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5708168');">
    Add yaws_dime.erl to the list of modules to compile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5708168');">
    // Peter Lemenkov
  </div>
  <div id="msg-5708168" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5708168');">
    <pre>
<strong>Author: Peter Lemenkov &lt;lemenkov@gmail.com&gt;</strong>
<strong>Date:   Wed Nov 17 16:20:23 2010</strong>

Add yaws_dime.erl to the list of modules to compile

This module is used in yaws_rpc, yaws_soap_lib and
yaws_soap_srv modules but it wasn't compiled. So it's
quite likely that application will fails with 'undef'
error.

Signed-off-by: Peter Lemenkov &lt;lemenkov@gmail.com&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/29fb4cd" target="_blank">29fb4cd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-29fb4cd');">
    security vulnerability for win32 reported at http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2010-4181
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-29fb4cd');">
    // Claes Wikstrom
  </div>
  <div id="msg-29fb4cd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-29fb4cd');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Nov 17 12:59:32 2010</strong>

security vulnerability for win32 reported at http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2010-4181
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d2225ed" target="_blank">d2225ed</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d2225ed');">
    bug in upload to file code found by Mojito Sorbet
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d2225ed');">
    // Claes Wikstrom
  </div>
  <div id="msg-d2225ed" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d2225ed');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Nov 8 09:41:23 2010</strong>

bug in upload to file code found by Mojito Sorbet
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/68111e1" target="_blank">68111e1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-68111e1');">
    added webm mime type
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-68111e1');">
    // François de Metz
  </div>
  <div id="msg-68111e1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-68111e1');">
    <pre>
<strong>Author: François de Metz &lt;fdemetz@af83.com&gt;</strong>
<strong>Date:   Fri Oct 29 14:33:16 2010</strong>

added webm mime type
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c25ed57" target="_blank">c25ed57</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c25ed57');">
    fix SSL Websocket support (Dan Willemsen)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c25ed57');">
    // Dan Willemsen
  </div>
  <div id="msg-c25ed57" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c25ed57');">
    <pre>
<strong>Author: Dan Willemsen &lt;dan@danw.org&gt;</strong>
<strong>Date:   Sun Oct 24 02:06:32 2010</strong>

fix SSL Websocket support (Dan Willemsen)

fixed a case where a gen_tcp call was erroneously being used on an SSL
socket
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/40fac1e" target="_blank">40fac1e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-40fac1e');">
    add eunit tests to top-level test Makefile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-40fac1e');">
    // Steve Vinoski
  </div>
  <div id="msg-40fac1e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-40fac1e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 21 22:10:18 2010</strong>

add eunit tests to top-level test Makefile

Make sure eunit tests get executed when test/Makefile is used to run
all the tests.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/31995fe" target="_blank">31995fe</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-31995fe');">
    let fdsrv return ignore for normal connection
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-31995fe');">
    // Steve Vinoski
  </div>
  <div id="msg-31995fe" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-31995fe');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 21 22:00:40 2010</strong>

let fdsrv return ignore for normal connection

Allow fdsrv to return the atom ignore, which tells yaws_server to just
listen normally. This is useful because fdsrv is global, but the user
might want to use it for just one virtual server while having listen
sockets for other virtual servers set up normally.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2f234c7" target="_blank">2f234c7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2f234c7');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2f234c7');">
    // Steve Vinoski
  </div>
  <div id="msg-2f234c7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2f234c7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 21 21:40:08 2010</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a1e2e74" target="_blank">a1e2e74</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a1e2e74');">
    add implementation of X-Forwarded-For header (Fabian Linzberger)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a1e2e74');">
    // Fabian Linzberger
  </div>
  <div id="msg-a1e2e74" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a1e2e74');">
    <pre>
<strong>Author: Fabian Linzberger &lt;e@lefant.net&gt;</strong>
<strong>Date:   Wed Oct 6 16:49:40 2010</strong>

add implementation of X-Forwarded-For header (Fabian Linzberger)

* Add x_forwarded_for_log_proxy_whitelist global config parameter and
  parsing for it.

* Add x_forwarded_for header in record definition and collection
  (based on code by Chris Double he mentions on
  http://www.bluishcoder.co.nz/2006/04/handling-x-forwarded-for-in-yaws.html)

* For yaws_server:maybe_access_log/3: if originating IP is present in
  x_forwarded_for_log_proxy_whitelist, log last IP found in
  x-forwarded-for header instead of originating IP
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c54054e" target="_blank">c54054e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c54054e');">
    Ahhrg...minor text modification of previous commit.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c54054e');">
    // Torbjorn Tornkvist
  </div>
  <div id="msg-c54054e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c54054e');">
    <pre>
<strong>Author: Torbjorn Tornkvist &lt;tobbe@klarna.com&gt;</strong>
<strong>Date:   Mon Oct 11 15:00:27 2010</strong>

Ahhrg...minor text modification of previous commit.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9b4dc47" target="_blank">9b4dc47</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9b4dc47');">
    Adding some more text to the arg_rewrite_mod paragraph to highlight the use of the yaws_vdir module.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9b4dc47');">
    // Torbjorn Tornkvist
  </div>
  <div id="msg-9b4dc47" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9b4dc47');">
    <pre>
<strong>Author: Torbjorn Tornkvist &lt;tobbe@klarna.com&gt;</strong>
<strong>Date:   Mon Oct 11 14:58:16 2010</strong>

Adding some more text to the arg_rewrite_mod paragraph to highlight the use of the yaws_vdir module.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/43d6620" target="_blank">43d6620</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-43d6620');">
    Adding yaws_vdir.erl to the Makefile.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-43d6620');">
    // Torbjorn Tornkvist
  </div>
  <div id="msg-43d6620" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-43d6620');">
    <pre>
<strong>Author: Torbjorn Tornkvist &lt;tobbe@klarna.com&gt;</strong>
<strong>Date:   Thu Oct 7 16:21:52 2010</strong>

Adding yaws_vdir.erl to the Makefile.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/20f58c8" target="_blank">20f58c8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-20f58c8');">
    Merge branch 'master' of git://github.com/klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-20f58c8');">
    // Per Andersson
  </div>
  <div id="msg-20f58c8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-20f58c8');">
    <pre>
<strong>Author: Per Andersson &lt;avtobiff@gmail.com&gt;</strong>
<strong>Date:   Wed Oct 6 09:52:34 2010</strong>

Merge branch 'master' of git://github.com/klacke/yaws

Conflicts:
	man/yaws.conf.5
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/713e35b" target="_blank">713e35b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-713e35b');">
    --running-config flag to query a running yaws for its config
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-713e35b');">
    // Claes Wikstrom
  </div>
  <div id="msg-713e35b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-713e35b');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Oct 5 21:37:02 2010</strong>

--running-config flag to query a running yaws for its config
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/035303b" target="_blank">035303b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-035303b');">
    bad docs for ssl verify
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-035303b');">
    // Claes Wikstrom
  </div>
  <div id="msg-035303b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-035303b');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Oct 5 21:15:32 2010</strong>

bad docs for ssl verify
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/891a972" target="_blank">891a972</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-891a972');">
    updated mime.types
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-891a972');">
    // Steve Vinoski
  </div>
  <div id="msg-891a972" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-891a972');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Oct 5 17:18:53 2010</strong>

updated mime.types

Updated mime.types based on Apache's mime.types. See the top of
src/mime.types for details.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9bcdaf6" target="_blank">9bcdaf6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9bcdaf6');">
    fix typos on soap intro page (reported by Wes James)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9bcdaf6');">
    // Steve Vinoski
  </div>
  <div id="msg-9bcdaf6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9bcdaf6');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Oct 4 18:19:23 2010</strong>

fix typos on soap intro page (reported by Wes James)

Fix typos, and also update the instructions for retrieving erlsom to
use github.com instead of SourceForge.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6d5b980" target="_blank">6d5b980</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6d5b980');">
    Added handling of SSL option.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6d5b980');">
    // Per Andersson
  </div>
  <div id="msg-6d5b980" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6d5b980');">
    <pre>
<strong>Author: Per Andersson &lt;avtobiff@gmail.com&gt;</strong>
<strong>Date:   Fri Oct 1 18:55:26 2010</strong>

Added handling of SSL option.

* Added access functions for fail_if_no_peer_cert.
* Handle new SSL verify and fail_if_no_peer_cert options in config.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b6cbbe5" target="_blank">b6cbbe5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b6cbbe5');">
    Fixed and updated SSL verify options.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b6cbbe5');">
    // Per Andersson
  </div>
  <div id="msg-b6cbbe5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b6cbbe5');">
    <pre>
<strong>Author: Per Andersson &lt;avtobiff@gmail.com&gt;</strong>
<strong>Date:   Fri Oct 1 17:56:23 2010</strong>

Fixed and updated SSL verify options.

* Fixed documentation for verify values to correspond to Erlang's SSL
  implementation.
* Updated documentation to include new SSL implementation options.
* Updated #ssl{} and yaws:ssl_listen_opts/2 to include
  fail_if_no_peer_cert.
    </pre>
  </div>
</div>

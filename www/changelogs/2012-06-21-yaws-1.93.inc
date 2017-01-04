<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/92a1a27" target="_blank">92a1a27</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-92a1a27');">
    sloppy bug that sneaked into the 1.93 release, call to undef function in yaws_ls
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-92a1a27');">
    // Claes Wikstrom
  </div>
  <div id="msg-92a1a27" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-92a1a27');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Jun 21 09:56:00 2012</strong>

sloppy bug that sneaked into the 1.93 release, call to undef function in yaws_ls
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/645c644" target="_blank">645c644</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-645c644');">
    preparing for 1.93
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-645c644');">
    // Claes Wikstrom
  </div>
  <div id="msg-645c644" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-645c644');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Jun 20 22:34:53 2012</strong>

preparing for 1.93
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b49b6e8" target="_blank">b49b6e8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b49b6e8');">
    preparing for 1.93
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b49b6e8');">
    // Claes Wikstrom
  </div>
  <div id="msg-b49b6e8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b49b6e8');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Jun 20 22:30:52 2012</strong>

preparing for 1.93
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/290b982" target="_blank">290b982</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-290b982');">
    preparing for 1.93
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-290b982');">
    // Claes Wikstrom
  </div>
  <div id="msg-290b982" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-290b982');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Jun 20 20:57:24 2012</strong>

preparing for 1.93
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8a1401b" target="_blank">8a1401b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8a1401b');">
    preparing for 1.93
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8a1401b');">
    // Claes Wikstrom
  </div>
  <div id="msg-8a1401b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8a1401b');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Jun 20 20:47:28 2012</strong>

preparing for 1.93
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b44aba6" target="_blank">b44aba6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b44aba6');">
    Use crypto:rand_bytes() instead of the cryptogrphicalli weak random module. Swedish security consultant and cryptographer Kalle Zetterlund discovered a way to - given a sequence of cokkies produced by yaws_session_server - predict the next session id. Thus providing a gaping security hole into yaws servers that yse the yaws_session_server to maintain cookie based HTTP sessions
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b44aba6');">
    // Claes Wikstrom
  </div>
  <div id="msg-b44aba6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b44aba6');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Jun 20 20:21:21 2012</strong>

Use crypto:rand_bytes() instead of the cryptogrphicalli weak random module. Swedish security consultant and cryptographer Kalle Zetterlund discovered a way to - given a sequence of cokkies produced by yaws_session_server - predict the next session id. Thus providing a gaping security hole into yaws servers that yse the yaws_session_server to maintain cookie based HTTP sessions
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/38df2eb" target="_blank">38df2eb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-38df2eb');">
    Merge pull request #110 from tuncer/reltool
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-38df2eb');">
    // Steve Vinoski
  </div>
  <div id="msg-38df2eb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-38df2eb');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Jun 19 06:08:48 2012</strong>

Merge pull request #110 from tuncer/reltool

Simplify reltool.config
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/73741c6" target="_blank">73741c6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-73741c6');">
    Simplify reltool.config
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-73741c6');">
    // Tuncer Ayaz
  </div>
  <div id="msg-73741c6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-73741c6');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Tue Jun 19 00:59:44 2012</strong>

Simplify reltool.config
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5545f49" target="_blank">5545f49</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5545f49');">
    minor fixes to yaws.tex
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5545f49');">
    // Steve Vinoski
  </div>
  <div id="msg-5545f49" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5545f49');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Jun 16 20:30:56 2012</strong>

minor fixes to yaws.tex
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8904c51" target="_blank">8904c51</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8904c51');">
    make sure to always send proper strings to file:write() while logging
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8904c51');">
    // Nicolas Adiba
  </div>
  <div id="msg-8904c51" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8904c51');">
    <pre>
<strong>Author: Nicolas Adiba &lt;adiba@whatsapp.com&gt;</strong>
<strong>Date:   Fri Jun 15 00:18:45 2012</strong>

make sure to always send proper strings to file:write() while logging

Failure to send proper string to file:write() on FreeBSD could result in a
emulator Bad value on output port 'efile' if bad data makes it there, say
an atom.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c90a997" target="_blank">c90a997</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c90a997');">
    default to a tcp queue backlog of 1024 (Nicolas Adiba)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c90a997');">
    // Steve Vinoski
  </div>
  <div id="msg-c90a997" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c90a997');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Jun 15 20:27:41 2012</strong>

default to a tcp queue backlog of 1024 (Nicolas Adiba)

Default to a backlog of 1024 rather than the default 5 that TCP provides.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0b3f29e" target="_blank">0b3f29e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0b3f29e');">
    log debug messages to the error_logger for embedded mode (Nicolas Adiba)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0b3f29e');">
    // Steve Vinoski
  </div>
  <div id="msg-0b3f29e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0b3f29e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Jun 15 19:50:30 2012</strong>

log debug messages to the error_logger for embedded mode (Nicolas Adiba)

Log debug messages to the error_logger given that stdout is not available
in embedded mode.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8e0ac54" target="_blank">8e0ac54</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8e0ac54');">
    Add support of the 'OPTIONS' method when WebDav is enabled
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8e0ac54');">
    // Christopher Faulet
  </div>
  <div id="msg-8e0ac54" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8e0ac54');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed Jun 13 12:18:18 2012</strong>

Add support of the 'OPTIONS' method when WebDav is enabled
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/05aa4f0" target="_blank">05aa4f0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-05aa4f0');">
    fix overfull hbox warnings in yaws.tex
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-05aa4f0');">
    // Steve Vinoski
  </div>
  <div id="msg-05aa4f0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-05aa4f0');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Jun 12 01:21:24 2012</strong>

fix overfull hbox warnings in yaws.tex

Fix overfull hboxes and also update some of the record definitions (like
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9a95602" target="_blank">9a95602</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9a95602');">
    Handle non-chunked responses without content-length in yaws_revproxy
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9a95602');">
    // Christopher Faulet
  </div>
  <div id="msg-9a95602" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9a95602');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jun 11 17:59:06 2012</strong>

Handle non-chunked responses without content-length in yaws_revproxy

In that case, the revproxy must read until the backend closes.
Add a test for this new functionality
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/32bdfb0" target="_blank">32bdfb0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-32bdfb0');">
    Refactor yaws_revproxy when large non-chunked content is received
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-32bdfb0');">
    // Christopher Faulet
  </div>
  <div id="msg-32bdfb0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-32bdfb0');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jun 11 17:43:11 2012</strong>

Refactor yaws_revproxy when large non-chunked content is received

Fix: srvconn_status must not be set to keep-alive
Refactoring: prepare the handling of non-chunked responses with no
content-length.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/98b6f00" target="_blank">98b6f00</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-98b6f00');">
    Fix conditions to close the backend socket when an error occurs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-98b6f00');">
    // Christopher Faulet
  </div>
  <div id="msg-98b6f00" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-98b6f00');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jun 11 17:28:38 2012</strong>

Fix conditions to close the backend socket when an error occurs

We must close the socket when an error _OTHER_THAN_ 'closed' occurs.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b10e492" target="_blank">b10e492</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b10e492');">
    handle large non-chunked content in yaws_revproxy
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b10e492');">
    // Steve Vinoski
  </div>
  <div id="msg-b10e492" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b10e492');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Jun 11 04:33:48 2012</strong>

handle large non-chunked content in yaws_revproxy

Use partial_post_size as a block size to handle large non-chunked content
from backend servers in yaws_revproxy. Read the content block by block,
returning each block to the client as it arrives. This avoids having to
read the entire backend response into memory before replying to the
client. Add a test for this new functionality and fix .gitignore to ignore
the new content file used for the test.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f1f858c" target="_blank">f1f858c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f1f858c');">
    add a do_recv variant taking a timeout arg
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f1f858c');">
    // Steve Vinoski
  </div>
  <div id="msg-f1f858c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f1f858c');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Jun 6 16:05:54 2012</strong>

add a do_recv variant taking a timeout arg
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/69b1215" target="_blank">69b1215</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-69b1215');">
    use request content type for SOAP responses
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-69b1215');">
    // Steve Vinoski
  </div>
  <div id="msg-69b1215" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-69b1215');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Jun 4 02:54:22 2012</strong>

use request content type for SOAP responses

Rather than hard-coding text/xml or application/xml as the content type for
SOAP responses, extract the content type from the SOAP request and use it
in the response. This will avoid problems with clients that don't properly
handle certain MIME types with SOAP.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b80121d" target="_blank">b80121d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b80121d');">
    fix websockets reply status codes for client close messages
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b80121d');">
    // Steve Vinoski
  </div>
  <div id="msg-b80121d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b80121d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Jun 2 21:33:24 2012</strong>

fix websockets reply status codes for client close messages

A number of autobahn WebSockets tests were failing due to replies
containing incorrect status codes. Check client status codes and reply with
correct codes.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fd690a6" target="_blank">fd690a6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fd690a6');">
    refactor yaws_websockets as a gen_server
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fd690a6');">
    // Steve Vinoski
  </div>
  <div id="msg-fd690a6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fd690a6');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Jun 1 04:53:26 2012</strong>

refactor yaws_websockets as a gen_server

The yaws_websockets module used an explicit recursive loop. Refactor it to
use a gen_server instead.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e2e6f66" target="_blank">e2e6f66</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e2e6f66');">
    assert return value of filelib:ensure_dir for $HOME/yaws dir
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e2e6f66');">
    // Steve Vinoski
  </div>
  <div id="msg-e2e6f66" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e2e6f66');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Jun 1 04:25:59 2012</strong>

assert return value of filelib:ensure_dir for $HOME/yaws dir

A user reported an installation-related error because of a missing
$HOME/.yaws dir where the CTL file is stored. Assert that
filelib:ensure_dir returns ok for that directory.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9e6c9e1" target="_blank">9e6c9e1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9e6c9e1');">
    revert SOAP response MIME type to "text/xml"
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9e6c9e1');">
    // Steve Vinoski
  </div>
  <div id="msg-9e6c9e1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9e6c9e1');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Jun 1 02:37:10 2012</strong>

revert SOAP response MIME type to "text/xml"

Some Microsoft tools don't handle "application/xml" as the MIME type for
SOAP responses. Revert the change from "text/xml" made in commit 15454bcd.

Also perform some minor cleanup in yaws_soap_lib and yaws_soap12_lib.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ea6c5b1" target="_blank">ea6c5b1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ea6c5b1');">
    use already-downloaded ibrowse for tests if network down
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ea6c5b1');">
    // Steve Vinoski
  </div>
  <div id="msg-ea6c5b1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ea6c5b1');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Jun 1 00:14:50 2012</strong>

use already-downloaded ibrowse for tests if network down

"make test" failed for disconnected operation due to trying to check the
latest version of ibrowse, which we use for a test client. Change it to
just use any existing ibrowse if it can't check github for the ibrowse
version.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5ecef77" target="_blank">5ecef77</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5ecef77');">
    Merge pull request #107 from nicad/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5ecef77');">
    // Steve Vinoski
  </div>
  <div id="msg-5ecef77" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5ecef77');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 26 03:35:56 2012</strong>

Merge pull request #107 from nicad/master

typo in WWW-Authenticate handling leading to infinite recursion
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/369ce33" target="_blank">369ce33</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-369ce33');">
    fix typo for WWW-Authenticate error leading to infinite recursion
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-369ce33');">
    // Nicolas Adiba
  </div>
  <div id="msg-369ce33" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-369ce33');">
    <pre>
<strong>Author: Nicolas Adiba &lt;adiba@whatsapp.com&gt;</strong>
<strong>Date:   Sat May 26 02:17:22 2012</strong>

fix typo for WWW-Authenticate error leading to infinite recursion
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/563c7ef" target="_blank">563c7ef</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-563c7ef');">
    add new HTTP status codes from RFC 6585
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-563c7ef');">
    // Steve Vinoski
  </div>
  <div id="msg-563c7ef" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-563c7ef');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 12 05:48:21 2012</strong>

add new HTTP status codes from RFC 6585

RFC 6585 added new HTTP status codes, so add them here.

Also change the code that limits the number of incoming headers to return
new status code 431 (Request Header Fields Too Large).
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8981ce2" target="_blank">8981ce2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8981ce2');">
    Add support for precompressed static files
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8981ce2');">
    // Christopher Faulet
  </div>
  <div id="msg-8981ce2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8981ce2');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 11 16:46:35 2012</strong>

Add support for precompressed static files

By setting use_gzip_static to true in deflate options, in a vhost
configuration, It is possible to serve precompressed versions of
static files. Yaws will look for precompressed files in the same
location as original files that end in ".gz".

Only files that do not fit in the cache are concerned and the mtime
of a precompressed file must be higher than the one of original file.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ea529f1" target="_blank">ea529f1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ea529f1');">
    Fix compat_send/3 to return the number of bytes sent
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ea529f1');">
    // Christopher Faulet
  </div>
  <div id="msg-ea529f1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ea529f1');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 11 16:09:18 2012</strong>

Fix compat_send/3 to return the number of bytes sent

So all versions of the send function into yaws_sendfile will return
{ok, BytesSent} if successful, otherwise {error, Reason}.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cab8a28" target="_blank">cab8a28</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cab8a28');">
    Improve how the responses compression is handled
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cab8a28');">
    // Christopher Faulet
  </div>
  <div id="msg-cab8a28" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cab8a28');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 11 09:07:29 2012</strong>

Improve how the responses compression is handled

Among other things, we can now disable the compression in yaws scripts by
returning '{header, {content_encoding, "identity"}}'.
A bug was also fixed when part of a large file is requested. The response
content length was calculated using the file size and not the part size.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/48b73a4" target="_blank">48b73a4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-48b73a4');">
    Do cosmetic changes in yaws.erl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-48b73a4');">
    // Christopher Faulet
  </div>
  <div id="msg-48b73a4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-48b73a4');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 4 15:40:06 2012</strong>

Do cosmetic changes in yaws.erl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/93a2fd4" target="_blank">93a2fd4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-93a2fd4');">
    fix issue #106: commit 6b5266 broke websockets
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-93a2fd4');">
    // Steve Vinoski
  </div>
  <div id="msg-93a2fd4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-93a2fd4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun May 6 04:10:59 2012</strong>

fix issue #106: commit 6b5266 broke websockets

The fix for uncaught exceptions as implemented in commit 6b5266
unfortunately included too much within a try/catch block and ended up
reporting intentional exit(normal) code in websockets as uncaught
exceptions. Rework the try/catch to protect only the call needing
protection.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5f11152" target="_blank">5f11152</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5f11152');">
    issue #105: don't create default libdir if configure --libdir option used
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5f11152');">
    // Steve Vinoski
  </div>
  <div id="msg-5f11152" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5f11152');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 5 19:27:42 2012</strong>

issue #105: don't create default libdir if configure --libdir option used
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c008278" target="_blank">c008278</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c008278');">
    fix yaws issue #105: configure ignores --libdir
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c008278');">
    // Steve Vinoski
  </div>
  <div id="msg-c008278" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c008278');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 5 17:41:19 2012</strong>

fix yaws issue #105: configure ignores --libdir
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6b52665" target="_blank">6b52665</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6b52665');">
    report uncaught exception as server error 500
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6b52665');">
    // Steve Vinoski
  </div>
  <div id="msg-6b52665" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6b52665');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 5 00:41:32 2012</strong>

report uncaught exception as server error 500

In yaws_server we used to catch exceptions thrown by application code and
ignore them. Now, catch them and report a 500 error back to the client, and
include an error message similar to what occurs for unexpected exits in
application code. This makes debugging applications that unexpectedly throw
exceptions much easier. Thanks to Richard Carlsson and Samuel Rivas for
reporting this.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/824e6db" target="_blank">824e6db</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-824e6db');">
    Move hostname resolution from yaws_server to yaws_log
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-824e6db');">
    // Christopher Faulet
  </div>
  <div id="msg-824e6db" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-824e6db');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu May 3 18:28:42 2012</strong>

Move hostname resolution from yaws_server to yaws_log

It is useful to keep ip addresses unchanged when a specific logger is
used.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f31e4b8" target="_blank">f31e4b8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f31e4b8');">
    Make the parameter "x_forwarded_for_log_proxy_whitelist" obsolete
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f31e4b8');">
    // Christopher Faulet
  </div>
  <div id="msg-f31e4b8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f31e4b8');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu May 3 14:56:55 2012</strong>

Make the parameter "x_forwarded_for_log_proxy_whitelist" obsolete

This parameter is now ignored and throws a warning when yaws starts.
If necessary, it can be replaced by a simple logger_mod on top of
yaws_log. To explain how to write such module, an example was added
into the www directory (www/logger_mod.yaws).
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4dc3b76" target="_blank">4dc3b76</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4dc3b76');">
    Remove safe_decode_path/1 from yaws_log and use...
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4dc3b76');">
    // Christopher Faulet
  </div>
  <div id="msg-4dc3b76" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4dc3b76');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu May 3 14:15:53 2012</strong>

Remove safe_decode_path/1 from yaws_log and use...

... yaws_server:safe_decode_path/1 instead.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/02930c9" target="_blank">02930c9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-02930c9');">
    Complete yaws:setup_gconf/2 and yaws:setup_sconf/3
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-02930c9');">
    // Christopher Faulet
  </div>
  <div id="msg-02930c9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-02930c9');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed May 2 17:01:02 2012</strong>

Complete yaws:setup_gconf/2 and yaws:setup_sconf/3

In embedded mode, these functions are used to populate #gconf and #sconf
records. Now, all fields can be setup.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/457d941" target="_blank">457d941</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-457d941');">
    doc update
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-457d941');">
    // Claes Wikstrom
  </div>
  <div id="msg-457d941" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-457d941');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed May 2 16:16:46 2012</strong>

doc update
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/592c765" target="_blank">592c765</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-592c765');">
    fix yaws issue #103: fix yapp exclude_dir paths (Mikael Karlsson)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-592c765');">
    // Steve Vinoski
  </div>
  <div id="msg-592c765" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-592c765');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Apr 12 20:46:41 2012</strong>

fix yaws issue #103: fix yapp exclude_dir paths (Mikael Karlsson)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/af3e2ca" target="_blank">af3e2ca</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-af3e2ca');">
    Fix bugs in yaws_api:parse_multipart_post/1,2 for chunked requests
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-af3e2ca');">
    // Christopher Faulet
  </div>
  <div id="msg-af3e2ca" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-af3e2ca');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Apr 27 09:54:41 2012</strong>

Fix bugs in yaws_api:parse_multipart_post/1,2 for chunked requests

For chunked requests, when several parts are parsed, only the first one was
processed properly. For the others, all the content of each part was needed
to be parsed. So when an huge file was uploaded, this bug could lead to a
memory exhaustion.

Note: Now, yaws_api:parse_multipart_post/1,2 can return '{error, Reason}'
if an error occurred during the parsing.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/02e9baf" target="_blank">02e9baf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-02e9baf');">
    Check the Content-Length before reading the request's content
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-02e9baf');">
    // Christopher Faulet
  </div>
  <div id="msg-02e9baf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-02e9baf');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Apr 26 16:09:11 2012</strong>

Check the Content-Length before reading the request's content

Some browsers, like Firefox, do not support files that are larger than 2GB.
Uploading a &gt;2GB file causes an integer overflow and the Content-Length
header is set to a negative value.

To prevent any crash in this case, we check the Content-Length value before
reading the request's content. When the value is negative, an error 400 is
returned.

A bug about this problem is openned for Firefox:
    - https://bugzilla.mozilla.org/show_bug.cgi?id=215450
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c74a92e" target="_blank">c74a92e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c74a92e');">
    Add options to configure deflate compression behaviour
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c74a92e');">
    // Christopher Faulet
  </div>
  <div id="msg-c74a92e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c74a92e');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed Apr 25 08:38:16 2012</strong>

Add options to configure deflate compression behaviour

By adding "&lt;deflate&gt; ... &lt;/deflate&gt;" structure in vhosts configuration,
it is possible to configure how deflate compression will be applied
and when it will come into effect. Now we can:

 * define the smallest response size that will be compressed
 * define the compression level to be used
 * specify the zlib compression window size
 * specify how much memory should be allocated for the internal
   compression state
 * choose the strategy used to tune the compression algorithm

All these parameters are used when a zlib stream is initialized for
compression.

It is also possible to define all compressible mime types.
Here is an example:

&lt;server localhost&gt;
  ...
  deflate = true
  &lt;deflate&gt;
    min_compress_size = 4096
    compression_level = best_compression
    mime_types        = defaults, image/*
    mime_types        = application/xml, application/xhtml+xml, application/rss+xml
    mem_level         = 9
    strategy          = default
    window_size       = 15
  &lt;/deflate&gt;
  ...
&lt;/server&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/172b8b4" target="_blank">172b8b4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-172b8b4');">
    Update yaws.tex to have the same description of the config file...
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-172b8b4');">
    // Christopher Faulet
  </div>
  <div id="msg-172b8b4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-172b8b4');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Apr 24 18:27:44 2012</strong>

Update yaws.tex to have the same description of the config file...

...than in the manpage yaws.conf.5
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2f9f664" target="_blank">2f9f664</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2f9f664');">
    Make the target test dependent on the target all
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2f9f664');">
    // Christopher Faulet
  </div>
  <div id="msg-2f9f664" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2f9f664');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Apr 24 16:04:23 2012</strong>

Make the target test dependent on the target all

So, when we run 'make test', Yaws sources are recompiled, if needed.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3683b2e" target="_blank">3683b2e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3683b2e');">
    Fix syntax and layout errors in yaws.conf.5
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3683b2e');">
    // Christopher Faulet
  </div>
  <div id="msg-3683b2e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3683b2e');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Apr 24 14:56:07 2012</strong>

Fix syntax and layout errors in yaws.conf.5
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c1bdfc7" target="_blank">c1bdfc7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c1bdfc7');">
    cosmetic comment and whitespace cleanup in yaws.hrl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c1bdfc7');">
    // Christopher Faulet
  </div>
  <div id="msg-c1bdfc7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c1bdfc7');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Apr 17 12:35:50 2012</strong>

cosmetic comment and whitespace cleanup in yaws.hrl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/af9ab82" target="_blank">af9ab82</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-af9ab82');">
    make handling of cookie names case insensitive
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-af9ab82');">
    // Steve Vinoski
  </div>
  <div id="msg-af9ab82" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-af9ab82');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 25 02:27:08 2012</strong>

make handling of cookie names case insensitive

According to RFC 2109, handling of cookie names is specified as being case
insensitive.

Also, add a couple cookie unit tests. More are needed though.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9334c88" target="_blank">9334c88</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9334c88');">
    add rebar dependencies needed for SOAP applications
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9334c88');">
    // Steve Vinoski
  </div>
  <div id="msg-9334c88" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9334c88');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 24 23:29:42 2012</strong>

add rebar dependencies needed for SOAP applications

Use rebar.config.script to dynamically add SOAP dependencies if the shell
environment variable YAWS_SOAP is set. This way, Yaws users not interested
in SOAP do not need these dependencies.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cbed101" target="_blank">cbed101</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cbed101');">
    change dialyzer Makefile targets to handle known warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cbed101');">
    // Steve Vinoski
  </div>
  <div id="msg-cbed101" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cbed101');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 24 22:10:41 2012</strong>

change dialyzer Makefile targets to handle known warnings

At the suggestion of Tuncer Ayaz, modify the dialyzer targets to use the
new known_dialyzer_warnings file to ignore all known dialyzer warnings.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/705cd0a" target="_blank">705cd0a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-705cd0a');">
    fix compilation error in previous commit (thanks again Kostis)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-705cd0a');">
    // Steve Vinoski
  </div>
  <div id="msg-705cd0a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-705cd0a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 22 17:31:30 2012</strong>

fix compilation error in previous commit (thanks again Kostis)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ed11020" target="_blank">ed11020</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ed11020');">
    fix broken change to yaws_trace.erl from previous commit (thanks Kostis Sagonas)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ed11020');">
    // Steve Vinoski
  </div>
  <div id="msg-ed11020" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ed11020');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 22 17:09:04 2012</strong>

fix broken change to yaws_trace.erl from previous commit (thanks Kostis Sagonas)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f3622d2" target="_blank">f3622d2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f3622d2');">
    dialyzer fixes and comments
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f3622d2');">
    // Steve Vinoski
  </div>
  <div id="msg-f3622d2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f3622d2');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 22 15:50:51 2012</strong>

dialyzer fixes and comments
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/02c9022" target="_blank">02c9022</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-02c9022');">
    fix misspelled call to yaws_api:headers_transfer_encoding
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-02c9022');">
    // Steve Vinoski
  </div>
  <div id="msg-02c9022" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-02c9022');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 22 06:28:09 2012</strong>

fix misspelled call to yaws_api:headers_transfer_encoding
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c0af90a" target="_blank">c0af90a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c0af90a');">
    add callback for abnormal websocket close
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c0af90a');">
    // Steve Vinoski
  </div>
  <div id="msg-c0af90a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c0af90a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 22 05:15:18 2012</strong>

add callback for abnormal websocket close

If a websocket client closes the connection abnormally, issue a callback to
the application callback module indicating the closure.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/27e5786" target="_blank">27e5786</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-27e5786');">
    Merge pull request #104 from abs/patch-1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-27e5786');">
    // Steve Vinoski
  </div>
  <div id="msg-27e5786" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-27e5786');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Apr 21 00:33:20 2012</strong>

Merge pull request #104 from abs/patch-1

Update www/websockets.yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6e10bf2" target="_blank">6e10bf2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6e10bf2');">
    Update www/websockets.yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6e10bf2');">
    // Andrei Soroker
  </div>
  <div id="msg-6e10bf2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6e10bf2');">
    <pre>
<strong>Author: Andrei Soroker &lt;soroker@gmail.com&gt;</strong>
<strong>Date:   Sat Apr 21 00:21:31 2012</strong>

Update www/websockets.yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/88996d8" target="_blank">88996d8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-88996d8');">
    cosmetic comment and whitespace cleanup in yaws_api.hrl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-88996d8');">
    // Steve Vinoski
  </div>
  <div id="msg-88996d8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-88996d8');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Apr 12 01:58:41 2012</strong>

cosmetic comment and whitespace cleanup in yaws_api.hrl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/32ad09a" target="_blank">32ad09a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-32ad09a');">
    note IPv4 or IPv6 as appropriate in munin statistics (Olivier Girondel)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-32ad09a');">
    // Steve Vinoski
  </div>
  <div id="msg-32ad09a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-32ad09a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 11 02:23:34 2012</strong>

note IPv4 or IPv6 as appropriate in munin statistics (Olivier Girondel)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/43bd8ab" target="_blank">43bd8ab</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-43bd8ab');">
    Merge pull request #102 from tuncer/noderunner
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-43bd8ab');">
    // Steve Vinoski
  </div>
  <div id="msg-43bd8ab" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-43bd8ab');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 4 20:07:28 2012</strong>

Merge pull request #102 from tuncer/noderunner

Sync noderunner script with rebar template
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ced8e45" target="_blank">ced8e45</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ced8e45');">
    Sync noderunner script with rebar template
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ced8e45');">
    // Tuncer Ayaz
  </div>
  <div id="msg-ced8e45" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ced8e45');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Wed Apr 4 17:49:03 2012</strong>

Sync noderunner script with rebar template
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1e103e9" target="_blank">1e103e9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1e103e9');">
    Merge pull request #100 from tuncer/reltool
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1e103e9');">
    // Steve Vinoski
  </div>
  <div id="msg-1e103e9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1e103e9');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 3 21:39:54 2012</strong>

Merge pull request #100 from tuncer/reltool

reltool: use app-level lib_dir setting
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1a888aa" target="_blank">1a888aa</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1a888aa');">
    fix configure's ERTS version checking for file:sendfile/5
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1a888aa');">
    // Steve Vinoski
  </div>
  <div id="msg-1a888aa" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1a888aa');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 3 21:34:46 2012</strong>

fix configure's ERTS version checking for file:sendfile/5

R15B01 is ERTS version 5.9.1. In commit ddfa25, made before R15B01 was
released, it was erroneously assumed to be 5.10. This prevented
file:sendfile/5 from being configured properly for R15B01.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fef5fcd" target="_blank">fef5fcd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fef5fcd');">
    reltool: use app-level lib_dir setting
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fef5fcd');">
    // Tuncer Ayaz
  </div>
  <div id="msg-fef5fcd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fef5fcd');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Mon Mar 26 19:37:57 2012</strong>

reltool: use app-level lib_dir setting
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/46aa94c" target="_blank">46aa94c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-46aa94c');">
    Merge pull request #101 from tuncer/port_env
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-46aa94c');">
    // Steve Vinoski
  </div>
  <div id="msg-46aa94c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-46aa94c');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 1 17:32:35 2012</strong>

Merge pull request #101 from tuncer/port_env

Update rebar.config: port_envs -&gt; port_env
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3020ed1" target="_blank">3020ed1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3020ed1');">
    Update rebar.config: port_envs -> port_env
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3020ed1');">
    // Tuncer Ayaz
  </div>
  <div id="msg-3020ed1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3020ed1');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Sat Mar 31 13:47:25 2012</strong>

Update rebar.config: port_envs -&gt; port_env
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ddfa257" target="_blank">ddfa257</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ddfa257');">
    rebar: use file:sendfile/5 if erts version > R15B
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ddfa257');">
    // Steve Vinoski
  </div>
  <div id="msg-ddfa257" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ddfa257');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 1 16:52:29 2012</strong>

rebar: use file:sendfile/5 if erts version &gt; R15B
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2150708" target="_blank">2150708</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2150708');">
    Use file:sendfile/5 if available
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2150708');">
    // Tuncer Ayaz
  </div>
  <div id="msg-2150708" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2150708');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Thu Dec 29 20:43:30 2011</strong>

Use file:sendfile/5 if available

Due to a bug in file:sendfile/5 in R15B, the yaws configure script enables
the use of file:sendfile/5 only for versions R15B01 or greater.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ad390f7" target="_blank">ad390f7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ad390f7');">
    fix tests due to ibrowse include file path change
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ad390f7');">
    // Steve Vinoski
  </div>
  <div id="msg-ad390f7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ad390f7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 1 04:03:50 2012</strong>

fix tests due to ibrowse include file path change

In the ibrowse sources ibrowse.hrl was moved from the src dir to the
include dir. Change yaws tests where necessary to account for this change.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3fcf9a7" target="_blank">3fcf9a7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3fcf9a7');">
    fix yaws issue #97: mail application config file read failure
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3fcf9a7');">
    // Steve Vinoski
  </div>
  <div id="msg-3fcf9a7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3fcf9a7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Mar 24 18:08:03 2012</strong>

fix yaws issue #97: mail application config file read failure

Change yaws_config to export the toks/2 function being called by the mail
application.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a323a3f" target="_blank">a323a3f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a323a3f');">
    fix compilation warnings in mail application
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a323a3f');">
    // Steve Vinoski
  </div>
  <div id="msg-a323a3f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a323a3f');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Mar 24 17:25:33 2012</strong>

fix compilation warnings in mail application
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/137f5c7" target="_blank">137f5c7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-137f5c7');">
    fix multipart tests for commit 470004
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-137f5c7');">
    // Steve Vinoski
  </div>
  <div id="msg-137f5c7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-137f5c7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Mar 17 18:12:29 2012</strong>

fix multipart tests for commit 470004
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/08cee94" target="_blank">08cee94</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-08cee94');">
    added soap12 capability
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-08cee94');">
    // Kaloyan Dimitrov
  </div>
  <div id="msg-08cee94" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-08cee94');">
    <pre>
<strong>Author: Kaloyan Dimitrov &lt;kalski@gmail.com&gt;</strong>
<strong>Date:   Fri Mar 9 20:27:01 2012</strong>

added soap12 capability
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a2c17ec" target="_blank">a2c17ec</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a2c17ec');">
    Added facility for specifying an #auth record when starting embedded
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a2c17ec');">
    // Ulf Wiger
  </div>
  <div id="msg-a2c17ec" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a2c17ec');">
    <pre>
<strong>Author: Ulf Wiger &lt;ulf@feuerlabs.com&gt;</strong>
<strong>Date:   Thu Mar 15 22:58:50 2012</strong>

Added facility for specifying an #auth record when starting embedded

When giving a proplist for SC, the option {auth, Opts} can be
given. Opts is a {Key,Value} list where Key shall match the fields
in the #auth{} record. All other options ignored.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cf60207" target="_blank">cf60207</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cf60207');">
    make embedded start use docroot from sconf
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cf60207');">
    // Steve Vinoski
  </div>
  <div id="msg-cf60207" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cf60207');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Mar 17 16:34:37 2012</strong>

make embedded start use docroot from sconf

Embedded startup previously used the docroot passed as the first
argument into yaws_api:embedded_start_conf even if the sconf records
passed in specified their own docroot settings. Fix the code to use
the docroot first argument only for sconf records that don't specify
their own docroots.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1d78da8" target="_blank">1d78da8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1d78da8');">
    fix overfull vboxes in yaws.tex
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1d78da8');">
    // Steve Vinoski
  </div>
  <div id="msg-1d78da8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1d78da8');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Mar 15 15:26:08 2012</strong>

fix overfull vboxes in yaws.tex
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a7f41ba" target="_blank">a7f41ba</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a7f41ba');">
    add description of "yaws --running-config"
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a7f41ba');">
    // Steve Vinoski
  </div>
  <div id="msg-a7f41ba" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a7f41ba');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Mar 15 15:15:10 2012</strong>

add description of "yaws --running-config"

Add description of "yaws --running-config" to the Embedded Mode
chapter of yaws.tex.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/470004c" target="_blank">470004c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-470004c');">
    A denial of service bug has been corrected. Multippart POST processing on the yaws server side contained a list_to_atom/1 call which potentially makes it possible for an attacker to craft a continous list of POSTs, each potentially generating a new atom. This is a backwards incompatible fix since the upload code on the server side is user code. That code now needs to look for strings instead of atoms. For example the upload.taws code in the www examples is changed. It searches now for the field "filename" instead of 'filename'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-470004c');">
    // Claes Wikstrom
  </div>
  <div id="msg-470004c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-470004c');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Mar 16 14:38:47 2012</strong>

A denial of service bug has been corrected. Multippart POST processing on the yaws server side contained a list_to_atom/1 call which potentially makes it possible for an attacker to craft a continous list of POSTs, each potentially generating a new atom. This is a backwards incompatible fix since the upload code on the server side is user code. That code now needs to look for strings instead of atoms. For example the upload.taws code in the www examples is changed. It searches now for the field "filename" instead of 'filename'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/de1f46a" target="_blank">de1f46a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-de1f46a');">
    Update gitignore file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-de1f46a');">
    // Christopher Faulet
  </div>
  <div id="msg-de1f46a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-de1f46a');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Mar 6 11:33:08 2012</strong>

Update gitignore file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f727b9bd" target="_blank">f727b9bd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f727b9bd');">
    Fix bug when reentrant requests are used to return regular files
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f727b9bd');">
    // Christopher Faulet
  </div>
  <div id="msg-f727b9bd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f727b9bd');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Mar 6 10:55:47 2012</strong>

Fix bug when reentrant requests are used to return regular files

Response headers were not correctly set, in particular the Content-Length
header. This bug was fixed by refactoring the way reentrant requests are
handled.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/acfd860" target="_blank">acfd860</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-acfd860');">
    Merge pull request #94 from karlsson/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-acfd860');">
    // Steve Vinoski
  </div>
  <div id="msg-acfd860" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-acfd860');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Mar 5 12:08:26 2012</strong>

Merge pull request #94 from karlsson/master

Adapted yapp.app.src to new yaws include.mk
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8beb7a9" target="_blank">8beb7a9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8beb7a9');">
    Adapt yapp.app.src to new yaws include.mk
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8beb7a9');">
    // Mikael Karlsson
  </div>
  <div id="msg-8beb7a9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8beb7a9');">
    <pre>
<strong>Author: Mikael Karlsson &lt;karlsson.rm@gmail.com&gt;</strong>
<strong>Date:   Sun Mar 4 14:57:09 2012</strong>

Adapt yapp.app.src to new yaws include.mk
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b1b4e2b" target="_blank">b1b4e2b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b1b4e2b');">
    Improve management of cached connections in the revproxy
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b1b4e2b');">
    // Christopher Faulet
  </div>
  <div id="msg-b1b4e2b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b1b4e2b');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Feb 28 22:32:50 2012</strong>

Improve management of cached connections in the revproxy
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7464efc" target="_blank">7464efc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7464efc');">
    Adapt rebar.config to rebar changes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7464efc');">
    // Tuncer Ayaz
  </div>
  <div id="msg-7464efc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7464efc');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Mon Feb 27 13:48:25 2012</strong>

Adapt rebar.config to rebar changes
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/262cac7" target="_blank">262cac7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-262cac7');">
    Add the revproxy testsuite
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-262cac7');">
    // Christopher Faulet
  </div>
  <div id="msg-262cac7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-262cac7');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Feb 24 12:57:51 2012</strong>

Add the revproxy testsuite
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fcba27d" target="_blank">fcba27d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fcba27d');">
    Refactor the reverse proxy to manage it as a "special" appmod
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fcba27d');">
    // Christopher Faulet
  </div>
  <div id="msg-fcba27d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fcba27d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Feb 21 15:32:08 2012</strong>

Refactor the reverse proxy to manage it as a "special" appmod

Now, the reverse proxy is written like an appmod and follows the same
workflow. We forge the #urltype{} record and use Arg#arg.state to pass
the revproxy's rule before calling yaws_revproxy:out/1.

For now, the support of persistent connections with the backend servers
is disabled. But it does not affect the status of client connections.
Chunked-transfer encoded requests and responses are supported.

Despite this refactoring, yaws_revproxy is always experimental and need
more tests to be safely used in production environments.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e61b0e3" target="_blank">e61b0e3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e61b0e3');">
    Manage all 'special' headers of #headers{} and #outh{} records
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e61b0e3');">
    // Christopher Faulet
  </div>
  <div id="msg-e61b0e3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e61b0e3');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Feb 21 14:49:54 2012</strong>

Manage all 'special' headers of #headers{} and #outh{} records

Some headers, like 'Content-Encoding' or 'Transfer-Encoding' were
not correctly handled. These changes can be useful for everyone who
writes yaws scripts or appmods. In particular, the next version of
the reverse proxy depends on it.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/54acbb6" target="_blank">54acbb6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-54acbb6');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-54acbb6');">
    // Steve Vinoski
  </div>
  <div id="msg-54acbb6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-54acbb6');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Feb 17 21:50:07 2012</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8ec7125" target="_blank">8ec7125</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8ec7125');">
    clean up rebar build problems with examples/src
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8ec7125');">
    // Steve Vinoski
  </div>
  <div id="msg-8ec7125" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8ec7125');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Feb 17 21:48:23 2012</strong>

clean up rebar build problems with examples/src

Clean up some duplicate options and extra whitespace in
examples/src/Makefile, and add examples/src to rebar.config to make
sure it gets built under rebar.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f163227" target="_blank">f163227</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f163227');">
    Fix issue #88
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f163227');">
    // Christopher Faulet
  </div>
  <div id="msg-f163227" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f163227');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed Feb 15 12:26:02 2012</strong>

Fix issue #88

2 changes here:

 * Revert "Flush remaining data when dynamic content is delivered"

This reverts commit d09ed3dc622be8d2b17efcd1cda552ee777fe464.
Systematically flushing data is not a good idea.

 * Add "flush" as possible return value of the out/1 function

Some clients (eg. Firefox and google-chome...) do not handle connection
close gracefully when the connection is closed during sending data; they
do not read the server response. So, when the response is important, to
warn the end-user that an error occured for example, we must be fair by
flushing data before closing the connection.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/280e38e" target="_blank">280e38e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-280e38e');">
    Fixed issue 87
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-280e38e');">
    // Claes Wikstrom
  </div>
  <div id="msg-280e38e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-280e38e');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Feb 13 15:19:51 2012</strong>

Fixed issue 87
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/16834ce" target="_blank">16834ce</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-16834ce');">
    add close callback for websockets
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-16834ce');">
    // Steve Vinoski
  </div>
  <div id="msg-16834ce" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-16834ce');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Feb 11 00:24:19 2012</strong>

add close callback for websockets

WebSocket clients can close their end of the connection, and RFC 6455
requires servers to handle that message appropriately. This change
adds a new callback message for basic WebSocket callback modules. The
server now also replies to client "close" messages with a "close"
reply, as RFC 6455 requires. WebSockets documentation updated as well.

Also included a message ordering fix from Jan Bothma.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0944464" target="_blank">0944464</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0944464');">
    minor cleanup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0944464');">
    // Steve Vinoski
  </div>
  <div id="msg-0944464" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0944464');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Feb 11 00:20:36 2012</strong>

minor cleanup
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ac369a3" target="_blank">ac369a3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ac369a3');">
    Allow the server signature to be defined per virtual server
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ac369a3');">
    // Christopher Faulet
  </div>
  <div id="msg-ac369a3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ac369a3');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Feb 2 17:42:48 2012</strong>

Allow the server signature to be defined per virtual server

Now, we can use the 'server_signature' directive into the server part
to overload the global one for specific virtual server.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0128031" target="_blank">0128031</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0128031');">
    Merge remote-tracking branch 'garret-smith/win_log_rotate'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0128031');">
    // Claes Wikstrom
  </div>
  <div id="msg-0128031" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0128031');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Jan 24 08:55:41 2012</strong>

Merge remote-tracking branch 'garret-smith/win_log_rotate'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2411b2f" target="_blank">2411b2f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2411b2f');">
    rotate log file regardless of copy_errlog parameter
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2411b2f');">
    // Garret Smith
  </div>
  <div id="msg-2411b2f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2411b2f');">
    <pre>
<strong>Author: Garret Smith &lt;garret.smith@gmail.com&gt;</strong>
<strong>Date:   Wed Dec 7 18:08:50 2011</strong>

rotate log file regardless of copy_errlog parameter

Signed-off-by: Garret Smith &lt;garret.smith@gmail.com&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/317008a" target="_blank">317008a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-317008a');">
    fix log rotation on Windows, where fsync() is required to get the actual file size
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-317008a');">
    // Garret Smith
  </div>
  <div id="msg-317008a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-317008a');">
    <pre>
<strong>Author: Garret Smith &lt;garret.smith@gmail.com&gt;</strong>
<strong>Date:   Wed Dec 7 18:08:03 2011</strong>

fix log rotation on Windows, where fsync() is required to get the actual file size

Signed-off-by: Garret Smith &lt;garret.smith@gmail.com&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/51c5e03" target="_blank">51c5e03</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-51c5e03');">
    Simplify excl_sys_filters
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-51c5e03');">
    // Tuncer Ayaz
  </div>
  <div id="msg-51c5e03" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-51c5e03');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Fri Jan 20 17:20:13 2012</strong>

Simplify excl_sys_filters
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/eab8584" target="_blank">eab8584</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-eab8584');">
    fixed wiki app XSS vulnerabilities
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-eab8584');">
    // Sergei Golovan
  </div>
  <div id="msg-eab8584" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-eab8584');">
    <pre>
<strong>Author: Sergei Golovan &lt;sgolovan@gmail.com&gt;</strong>
<strong>Date:   Mon Jan 16 19:29:30 2012</strong>

fixed wiki app XSS vulnerabilities

In applications/wiki/src/wiki.erl,
applications/wiki/src/wiki_format_txt.erl,
applications/wiki/src/wiki_to_html.erl: Fixed CVE-2011-5025 (multiple
cross-site scripting (XSS) vulnerabilities in the wiki) by accepting
only letters and digits in node names, guarding a few calls to
list_to_integer/1, and escaping all HTML which goes to a user's
browser.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5c0cc97" target="_blank">5c0cc97</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5c0cc97');">
    R15B compatibility fixes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5c0cc97');">
    // Sergei Golovan
  </div>
  <div id="msg-5c0cc97" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5c0cc97');">
    <pre>
<strong>Author: Sergei Golovan &lt;sgolovan@gmail.com&gt;</strong>
<strong>Date:   Mon Jan 16 19:11:59 2012</strong>

R15B compatibility fixes

In applications/mail/src/mail.erl, applications/mail/src/smtp.erl,
applications/wiki/src/utils.erl, applications/wiki/src/wiki.erl,
applications/wiki/src/wiki_format_txt.erl,
applications/wiki/src/wiki_yaws.erl: Fixed compatibility with Erlang
R15B. Replaced atom/1, list/1, integer/1 by is_atom/1, is_list/1,
is_integer/1 and erlang:fault/1 by erlang:error/1. In the wiki app,
replaced regexp module by re one, renamed error/1 by html_error/1 to
avoid clashing with global error/1 function.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/404aec1" target="_blank">404aec1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-404aec1');">
    fix syntax and spelling errors in man pages
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-404aec1');">
    // Sergei Golovan
  </div>
  <div id="msg-404aec1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-404aec1');">
    <pre>
<strong>Author: Sergei Golovan &lt;sgolovan@gmail.com&gt;</strong>
<strong>Date:   Mon Jan 16 19:09:09 2012</strong>

fix syntax and spelling errors in man pages

In man/yaws.conf.5, man/yaws_api.5: Fixed a few syntax and spelling
errors in manpages.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/86dc31d" target="_blank">86dc31d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-86dc31d');">
    handle parseable but invalid ctlfile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-86dc31d');">
    // Sergei Golovan
  </div>
  <div id="msg-86dc31d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-86dc31d');">
    <pre>
<strong>Author: Sergei Golovan &lt;sgolovan@gmail.com&gt;</strong>
<strong>Date:   Mon Jan 16 19:03:58 2012</strong>

handle parseable but invalid ctlfile

src/yaws_ctl.erl: Process the case when ctlfile is parseable but its
content is invalid.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a40036a" target="_blank">a40036a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a40036a');">
    fixes for make clean in applications and doc dirs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a40036a');">
    // Sergei Golovan
  </div>
  <div id="msg-a40036a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a40036a');">
    <pre>
<strong>Author: Sergei Golovan &lt;sgolovan@gmail.com&gt;</strong>
<strong>Date:   Mon Jan 16 18:59:52 2012</strong>

fixes for make clean in applications and doc dirs

* applications/wiki/scripts/Makefile: Remove all scripts on make
  clean.
* applications/yapp/Makefile: Remove forgotten PNG image on make
  clean.
* doc/Makefile: Remove generated www/yaws.ps and www/yaws.pdf on
  make clean.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d677d20" target="_blank">d677d20</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d677d20');">
    Merge remote-tracking branch 'capflam/multi_docroots'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d677d20');">
    // Claes Wikstrom
  </div>
  <div id="msg-d677d20" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d677d20');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jan 9 22:03:55 2012</strong>

Merge remote-tracking branch 'capflam/multi_docroots'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9b220fe" target="_blank">9b220fe</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9b220fe');">
    Merge remote-tracking branch 'capflam/infinite_keepalive_timeout'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9b220fe');">
    // Claes Wikstrom
  </div>
  <div id="msg-9b220fe" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9b220fe');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jan 9 21:53:59 2012</strong>

Merge remote-tracking branch 'capflam/infinite_keepalive_timeout'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9cde5d0" target="_blank">9cde5d0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9cde5d0');">
    Merge remote-tracking branch 'capflam/ssl_accept_timeout'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9cde5d0');">
    // Claes Wikstrom
  </div>
  <div id="msg-9cde5d0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9cde5d0');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jan 9 21:53:14 2012</strong>

Merge remote-tracking branch 'capflam/ssl_accept_timeout'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a343807" target="_blank">a343807</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a343807');">
    Merge remote-tracking branch 'capflam/trace_improvements'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a343807');">
    // Claes Wikstrom
  </div>
  <div id="msg-a343807" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a343807');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jan 9 21:51:49 2012</strong>

Merge remote-tracking branch 'capflam/trace_improvements'

Conflicts:
	src/yaws_server.erl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/22806cd" target="_blank">22806cd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-22806cd');">
    Merge remote-tracking branch 'capflam/set_outh_in_rw_resp'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-22806cd');">
    // Claes Wikstrom
  </div>
  <div id="msg-22806cd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-22806cd');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jan 9 21:18:54 2012</strong>

Merge remote-tracking branch 'capflam/set_outh_in_rw_resp'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3cfc219" target="_blank">3cfc219</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3cfc219');">
    Merge remote-tracking branch 'capflam/out_fixes'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3cfc219');">
    // Claes Wikstrom
  </div>
  <div id="msg-3cfc219" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3cfc219');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jan 9 21:00:26 2012</strong>

Merge remote-tracking branch 'capflam/out_fixes'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d6b27e4" target="_blank">d6b27e4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d6b27e4');">
    Merge pull request #80 from richcarl/fix-some-warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d6b27e4');">
    // Claes Wikstrom
  </div>
  <div id="msg-d6b27e4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d6b27e4');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jan 9 15:46:07 2012</strong>

Merge pull request #80 from richcarl/fix-some-warnings

fixed warnings about unused imports and export_all
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e1896ee" target="_blank">e1896ee</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e1896ee');">
    Merge pull request #79 from richcarl/check-host-undefined
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e1896ee');">
    // Claes Wikstrom
  </div>
  <div id="msg-e1896ee" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e1896ee');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jan 9 15:45:20 2012</strong>

Merge pull request #79 from richcarl/check-host-undefined

check for Host = undefined in pick_host()
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a77d2e9" target="_blank">a77d2e9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a77d2e9');">
    Set default output headers when a rewrite response is returned
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a77d2e9');">
    // Christopher Faulet
  </div>
  <div id="msg-a77d2e9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a77d2e9');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 17:47:51 2012</strong>

Set default output headers when a rewrite response is returned

Because a response can be returned by the module used to rewrite arg records,
we must take care to set default output headers like 'date' and 'server'.
When the response has a content, if the content type is not defined, then
we set it to "text/plain".
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9730337" target="_blank">9730337</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9730337');">
    Add tests for flush mechanisms
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9730337');">
    // Christopher Faulet
  </div>
  <div id="msg-9730337" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9730337');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 17:44:12 2012</strong>

Add tests for flush mechanisms
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/96da4a1" target="_blank">96da4a1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-96da4a1');">
    Allow gconf keepalive_timeout to be set to infinity
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-96da4a1');">
    // Christopher Faulet
  </div>
  <div id="msg-96da4a1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-96da4a1');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jan 9 12:27:35 2012</strong>

Allow gconf keepalive_timeout to be set to infinity

The manpage of yaws.conf says that 'infinity' is a legal but not recommended
value for keepalive_timeout. But it was not really supported.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6b56d41" target="_blank">6b56d41</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6b56d41');">
    Update the testsuite accordingly
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6b56d41');">
    // Christopher Faulet
  </div>
  <div id="msg-6b56d41" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6b56d41');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jan 9 11:49:27 2012</strong>

Update the testsuite accordingly
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6a0379d" target="_blank">6a0379d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6a0379d');">
    Use gconf keepalive_timeout when the SSL connection is established
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6a0379d');">
    // Christopher Faulet
  </div>
  <div id="msg-6a0379d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6a0379d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jan 9 10:35:52 2012</strong>

Use gconf keepalive_timeout when the SSL connection is established

If a client open an SSL connection on Yaws without starting the SSL
negotiation phase, the server hangs until the client closes the connection.
By using a timeout during the call to ssl_accept, we solve this problem.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/eacdbbd" target="_blank">eacdbbd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-eacdbbd');">
    fixed warnings about unused imports and export_all
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-eacdbbd');">
    // Richard Carlsson
  </div>
  <div id="msg-eacdbbd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-eacdbbd');">
    <pre>
<strong>Author: Richard Carlsson &lt;richardc@klarna.com&gt;</strong>
<strong>Date:   Mon Jan 9 09:51:45 2012</strong>

fixed warnings about unused imports and export_all
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2cee2df" target="_blank">2cee2df</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2cee2df');">
    check for Host = undefined in pick_host()
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2cee2df');">
    // Richard Carlsson
  </div>
  <div id="msg-2cee2df" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2cee2df');">
    <pre>
<strong>Author: Richard Carlsson &lt;richardc@klarna.com&gt;</strong>
<strong>Date:   Mon Jan 9 09:23:56 2012</strong>

check for Host = undefined in pick_host()
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4a8d7ba" target="_blank">4a8d7ba</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4a8d7ba');">
    Update documentation accordingly
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4a8d7ba');">
    // Christopher Faulet
  </div>
  <div id="msg-4a8d7ba" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4a8d7ba');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Sep 9 14:20:15 2011</strong>

Update documentation accordingly
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/55f72f9" target="_blank">55f72f9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-55f72f9');">
    Use filter to reduce the amount of trace messages logged by Yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-55f72f9');">
    // Christopher Faulet
  </div>
  <div id="msg-55f72f9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-55f72f9');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Aug 29 17:20:34 2011</strong>

Use filter to reduce the amount of trace messages logged by Yaws

When traffic or http tracing is enabled, a filter can be defined to
disable trace logging for specific requests. When Yaws receive a request,
if tracing is enabled and the filter is evaluated to true, trace messages
are logged. Else tracing is disable for this request.

Filter is defined using a syntax similar to the condition part of match
specifications. We can match on the client's ip address or request headers.

For example, to trace requests coming from 127.0.0.1 on all gif images,
we can do:

  1&gt; yaws_trace:set_filter({'and',
                            {equal, ip, "127.0.0.1"},
                            {match, {request, path}, "^.*\.gif$"}}).
  ok
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c363cfe" target="_blank">c363cfe</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c363cfe');">
    Typo
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c363cfe');">
    // Christopher Faulet
  </div>
  <div id="msg-c363cfe" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c363cfe');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Aug 12 15:50:04 2011</strong>

Typo
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d9e3392" target="_blank">d9e3392</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d9e3392');">
    Speed up stop/quite_stop target in test suites when yaws is not started
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d9e3392');">
    // Christopher Faulet
  </div>
  <div id="msg-d9e3392" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d9e3392');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Aug 11 17:24:14 2011</strong>

Speed up stop/quite_stop target in test suites when yaws is not started
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1462128" target="_blank">1462128</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1462128');">
    Trace parameter is dynamically reconfigurable
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1462128');">
    // Christopher Faulet
  </div>
  <div id="msg-1462128" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1462128');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Aug 11 17:19:51 2011</strong>

Trace parameter is dynamically reconfigurable

So yaws_trace:enable_trace/disable_trace are also dynamic.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/61cadfe" target="_blank">61cadfe</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-61cadfe');">
    Remove useless code about trace handling in yaws_log
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-61cadfe');">
    // Christopher Faulet
  </div>
  <div id="msg-61cadfe" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-61cadfe');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Aug 11 16:48:17 2011</strong>

Remove useless code about trace handling in yaws_log
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/eb25bc7" target="_blank">eb25bc7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-eb25bc7');">
    Use yaws_trace to handle trace files
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-eb25bc7');">
    // Christopher Faulet
  </div>
  <div id="msg-eb25bc7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-eb25bc7');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Aug 11 16:45:59 2011</strong>

Use yaws_trace to handle trace files
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/082d55c" target="_blank">082d55c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-082d55c');">
    Add new module to deal with trace files
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-082d55c');">
    // Christopher Faulet
  </div>
  <div id="msg-082d55c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-082d55c');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Aug 11 16:28:00 2011</strong>

Add new module to deal with trace files

This module is a replacement of what we find in yaws_log.erl. This is the
first step to improve traces handling.

Changes with yaws_log.erl:

  * Trace files are created in a subdirectory suffixed with the creation
    date. So this subdirectory changed every time yaws restarts.

  * There is 1 trace file by connection handler. So traces cannot be
    interwined with each other

  * We can enable/disable trace for a period of time (must be improved
    to be fully dynamic)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/16d46fc" target="_blank">16d46fc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-16d46fc');">
    Retrieve the address and port number of the peer only once
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-16d46fc');">
    // Christopher Faulet
  </div>
  <div id="msg-16d46fc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-16d46fc');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Aug 11 16:25:12 2011</strong>

Retrieve the address and port number of the peer only once
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3dee1a5" target="_blank">3dee1a5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3dee1a5');">
    Update documentation and manpages accordingly
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3dee1a5');">
    // Christopher Faulet
  </div>
  <div id="msg-3dee1a5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3dee1a5');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 17:40:17 2012</strong>

Update documentation and manpages accordingly
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d09ed3d" target="_blank">d09ed3d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d09ed3d');">
    Flush remaining data when dynamic content is delivered
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d09ed3d');">
    // Christopher Faulet
  </div>
  <div id="msg-d09ed3d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d09ed3d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 17:25:04 2012</strong>

Flush remaining data when dynamic content is delivered

So, in a script, we can return values without taking care of remaining data
on the socket. This is very usefull to return result early even if the
request content is chunked.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0e1a7c3" target="_blank">0e1a7c3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0e1a7c3');">
    Fix handle_out_reply/5 function
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0e1a7c3');">
    // Christopher Faulet
  </div>
  <div id="msg-0e1a7c3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0e1a7c3');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 17:21:21 2012</strong>

Fix handle_out_reply/5 function
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2d4fba9" target="_blank">2d4fba9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2d4fba9');">
    Handle_crash/2 function must always return break
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2d4fba9');">
    // Christopher Faulet
  </div>
  <div id="msg-2d4fba9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2d4fba9');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 16:35:19 2012</strong>

Handle_crash/2 function must always return break

This is useful when a crash is reported during evaluation of return values
from Mod:out/1 function.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/92899a8" target="_blank">92899a8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-92899a8');">
    Fix get_chunked_client_data/2 function
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-92899a8');">
    // Christopher Faulet
  </div>
  <div id="msg-92899a8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-92899a8');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 16:34:30 2012</strong>

Fix get_chunked_client_data/2 function

If this function is called when all data are already read, it hangs.
Now, this state is caught.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1245ce4" target="_blank">1245ce4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1245ce4');">
    Refactor flush/1 function to prevent DoS attack
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1245ce4');">
    // Christopher Faulet
  </div>
  <div id="msg-1245ce4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1245ce4');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 16:32:27 2012</strong>

Refactor flush/1 function to prevent DoS attack

When Yaws receives a request with the method 'GET', 'HEAD', 'OPTIONS'..., its
content is ignored. Because the connection may be persistent, we must flush
the socket.

Today, this is done by reading all the data at one time. If there is an huge
amount of data, Erlang can run out of memory. To fix this issue, we flush
data by blocks of maximum 'partial_post_size' bytes.

There is another problem with the current implementation of the flush/1
function. It does not handle chunked encoded requests. So, now, we also
catch this case.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ad267c4" target="_blank">ad267c4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ad267c4');">
    Update documentation about docroot directive
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ad267c4');">
    // Christopher Faulet
  </div>
  <div id="msg-ad267c4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ad267c4');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 16:26:46 2012</strong>

Update documentation about docroot directive
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4ec8c19" target="_blank">4ec8c19</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4ec8c19');">
    Improve 'docroot' directive definition
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4ec8c19');">
    // Christopher Faulet
  </div>
  <div id="msg-4ec8c19" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4ec8c19');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri Jan 6 16:24:21 2012</strong>

Improve 'docroot' directive definition

Now, we just need at least one valid docroot in a server part to start yaws;
other invalid docroots are skipped with their associated auth structures.
Moreover, multiple docroot directives can be used.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/990bf47" target="_blank">990bf47</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-990bf47');">
    moved the websockets example cb modules into the  example dir
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-990bf47');">
    // Claes Wikstrom
  </div>
  <div id="msg-990bf47" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-990bf47');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jan 2 14:53:17 2012</strong>

moved the websockets example cb modules into the  example dir
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/53f02fc" target="_blank">53f02fc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-53f02fc');">
    added yaws.eps high resolution logo , created by Tomas Selander
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-53f02fc');">
    // Claes Wikstrom
  </div>
  <div id="msg-53f02fc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-53f02fc');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Dec 29 16:44:11 2011</strong>

added yaws.eps high resolution logo , created by Tomas Selander
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ee32d2c" target="_blank">ee32d2c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ee32d2c');">
    Merge pull request #76 from tuncer/release-vsn
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ee32d2c');">
    // Steve Vinoski
  </div>
  <div id="msg-ee32d2c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ee32d2c');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Dec 28 15:35:09 2011</strong>

Merge pull request #76 from tuncer/release-vsn

Use version from vsn.mk for reltool release
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/167c7bc" target="_blank">167c7bc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-167c7bc');">
    Use version from vsn.mk for reltool release
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-167c7bc');">
    // Tuncer Ayaz
  </div>
  <div id="msg-167c7bc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-167c7bc');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Wed Dec 28 13:29:56 2011</strong>

Use version from vsn.mk for reltool release
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/04970fa" target="_blank">04970fa</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-04970fa');">
    exhtml cleanup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-04970fa');">
    // Claes Wikstrom
  </div>
  <div id="msg-04970fa" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-04970fa');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@iron.hyber.org&gt;</strong>
<strong>Date:   Sun Dec 25 21:27:47 2011</strong>

exhtml cleanup
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5c9e8dc" target="_blank">5c9e8dc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5c9e8dc');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5c9e8dc');">
    // Steve Vinoski
  </div>
  <div id="msg-5c9e8dc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5c9e8dc');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Dec 24 23:26:36 2011</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a056469" target="_blank">a056469</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a056469');">
    set 2.61 as minimum required autoconf version
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a056469');">
    // Steve Vinoski
  </div>
  <div id="msg-a056469" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a056469');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Dec 24 23:26:10 2011</strong>

set 2.61 as minimum required autoconf version
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e119da6" target="_blank">e119da6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e119da6');">
    rotate log file regardless of copy_errlog parameter
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e119da6');">
    // Garret Smith
  </div>
  <div id="msg-e119da6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e119da6');">
    <pre>
<strong>Author: Garret Smith &lt;garret.smith@gmail.com&gt;</strong>
<strong>Date:   Wed Dec 7 18:08:50 2011</strong>

rotate log file regardless of copy_errlog parameter
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8025d25" target="_blank">8025d25</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8025d25');">
    fix log rotation on Windows, where fsync() is required to get the actual file size
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8025d25');">
    // Garret Smith
  </div>
  <div id="msg-8025d25" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8025d25');">
    <pre>
<strong>Author: Garret Smith &lt;garret.smith@gmail.com&gt;</strong>
<strong>Date:   Wed Dec 7 18:08:03 2011</strong>

fix log rotation on Windows, where fsync() is required to get the actual file size
    </pre>
  </div>
</div>

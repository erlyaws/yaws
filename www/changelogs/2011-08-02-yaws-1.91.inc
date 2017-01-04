<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/467a675" target="_blank">467a675</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-467a675');">
    prepare for 1.91
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-467a675');">
    // Claes Wikstrom
  </div>
  <div id="msg-467a675" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-467a675');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Aug 2 15:43:09 2011</strong>

prepare for 1.91
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/23c0c13" target="_blank">23c0c13</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-23c0c13');">
    Added support to generate and check strict xhtml output
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-23c0c13');">
    // Claes Wikstrom
  </div>
  <div id="msg-23c0c13" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-23c0c13');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Aug 2 14:47:00 2011</strong>

Added support to generate and check strict xhtml output
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3bdc203" target="_blank">3bdc203</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3bdc203');">
    Added possibility to turn off yaws log wrapping. This is useful for applications that embedd yaws and do not want that functionality at all.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3bdc203');">
    // Claes Wikstrom
  </div>
  <div id="msg-3bdc203" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3bdc203');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Aug 2 14:02:46 2011</strong>

Added possibility to turn off yaws log wrapping. This is useful for applications that embedd yaws and do not want that functionality at all.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b6e60f0" target="_blank">b6e60f0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b6e60f0');">
    revert "remove support for SOAP DIME attachments"
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b6e60f0');">
    // Steve Vinoski
  </div>
  <div id="msg-b6e60f0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b6e60f0');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Jul 7 03:30:24 2011</strong>

revert "remove support for SOAP DIME attachments"

This reverts commit 65bf36a597ac26559d179059a98bfcf5ebe8e9cc.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4d7440f" target="_blank">4d7440f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4d7440f');">
    add proxy autoconfig to MIME types (Kenji Rikitake)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4d7440f');">
    // Steve Vinoski
  </div>
  <div id="msg-4d7440f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4d7440f');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Jul 3 21:09:50 2011</strong>

add proxy autoconfig to MIME types (Kenji Rikitake)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a922ccb" target="_blank">a922ccb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a922ccb');">
    catch exit signals sent by gserv process
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a922ccb');">
    // Christopher Faulet
  </div>
  <div id="msg-a922ccb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a922ccb');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Jun 21 09:44:07 2011</strong>

catch exit signals sent by gserv process

In commit 5cc39b7, the temporary fix for issue #60, all exit signals
found when erasing transient messages are ignored. This commit
completes that fix. Exit signals coming specifically from the gserv
process to shutdown the connection are also ignored.  So, only signals
from gserv are now processed and all others are flushed.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5cc39b7" target="_blank">5cc39b7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5cc39b7');">
    fix reverse proxy problem (issue #60)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5cc39b7');">
    // Steve Vinoski
  </div>
  <div id="msg-5cc39b7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5cc39b7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Jun 21 04:29:42 2011</strong>

fix reverse proxy problem (issue #60)

Change yaws_server.erl to not exit on {'EXIT', From, Reason} messages
found when erasing transient messages, a change originally introduced
in commit c0755739. Doing so was causing reverse proxy to no longer
work, as reported in issue #60 on github and as experienced by several
users. Note however that this might be a temporary fix, since it might
be better to figure out the source of the EXIT message, but I'm
committing and pushing this fix now for the benefit of those waiting
to use the reverse proxy feature.

Thanks to Manuel Durán Aguete for providing a test case making it easy
to reproduce the problem and test the fix.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2961b81" target="_blank">2961b81</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2961b81');">
    fix yaws_rpc session handler (Christian Adams)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2961b81');">
    // Steve Vinoski
  </div>
  <div id="msg-2961b81" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2961b81');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Jun 19 03:48:30 2011</strong>

fix yaws_rpc session handler (Christian Adams)

Fix yaws_rpc:handler_session funs to remember sessions. The problem
resulted in the www/json_sample.html example no longer counting.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b4aab16" target="_blank">b4aab16</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b4aab16');">
    let configure handle erlang built from git clone
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b4aab16');">
    // Steve Vinoski
  </div>
  <div id="msg-b4aab16" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b4aab16');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Jun 19 02:59:32 2011</strong>

let configure handle erlang built from git clone

A normal Erlang installation has versioned directory names for
applications such as sasl, kernel, and ssl. The yaws configure script
checks for these. But an Erlang built from a git clone of the OTP
repository, the versions for these applications do not exist. Trying
to build yaws using erl built in such a git clone would fail due to
this lack of versioned applications.

Change configure.in to look for normal application versions first and
if not found, look for application dirs containing vsn.mk files. If
found, use those versions instead. Also change configure.in to get the
erts version from running the emulator instead of from the erts
versioned directory name.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b7b8b7d" target="_blank">b7b8b7d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b7b8b7d');">
    fix missing whitespace in yaws_sendfile error message
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b7b8b7d');">
    // Tuncer Ayaz
  </div>
  <div id="msg-b7b8b7d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b7b8b7d');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Fri Jun 3 20:11:46 2011</strong>

fix missing whitespace in yaws_sendfile error message
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/27105f7" target="_blank">27105f7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-27105f7');">
    slight tweak to json2 float test
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-27105f7');">
    // Steve Vinoski
  </div>
  <div id="msg-27105f7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-27105f7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Jun 2 15:02:28 2011</strong>

slight tweak to json2 float test
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/661aaf9" target="_blank">661aaf9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-661aaf9');">
    fixes for json2.erl and json.erl (Nico Kruber)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-661aaf9');">
    // Steve Vinoski
  </div>
  <div id="msg-661aaf9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-661aaf9');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Jun 2 02:18:28 2011</strong>

fixes for json2.erl and json.erl (Nico Kruber)

Implement more precise floating point number conversions -- avoid
converting via io_lib:format/2 by using erlang:float_to_list/1. Fix
encoding lists of (small) floats (json.erl mistakenly tried to convert
them to strings). Fix/add encoding of lists to {array, List}. Handle
unicode in object keys.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4451864" target="_blank">4451864</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4451864');">
    remove leftover dbg statements
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4451864');">
    // Steve Vinoski
  </div>
  <div id="msg-4451864" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4451864');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Jun 1 20:12:50 2011</strong>

remove leftover dbg statements
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8748762" target="_blank">8748762</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8748762');">
    quiet test messages for stopping yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8748762');">
    // Steve Vinoski
  </div>
  <div id="msg-8748762" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8748762');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Jun 1 16:13:21 2011</strong>

quiet test messages for stopping yaws

A recent change added extra stop calls for yaws to prevent problems
caused by yaws instances being left over from previous test runs. But
if yaws isn't running, these messages were emitting misleading error
messages. These are now redirected to /dev/null.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/183ec82" target="_blank">183ec82</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-183ec82');">
    restore removed jsonrpc:call/3
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-183ec82');">
    // Steve Vinoski
  </div>
  <div id="msg-183ec82" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-183ec82');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Jun 1 16:11:21 2011</strong>

restore removed jsonrpc:call/3

Restore the jsonrpc:call/3 function inadvertently removed in the
JSON-RPC 2.0 update. Add a new test for it. Remove test/src/httpc.erl
because it conflicted with inets:httpc during the execution of the new
test, and also because it's not used anywhere.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/11b272b" target="_blank">11b272b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-11b272b');">
    use {wordsize, external} in configure script
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-11b272b');">
    // Steve Vinoski
  </div>
  <div id="msg-11b272b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-11b272b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue May 31 04:42:02 2011</strong>

use {wordsize, external} in configure script

To properly check to see whether the Erlang emulator is 32-bit or
64-bit in the configure script, call erlang:system_info with the
argument {wordsize, external} instead of just wordsize. The latter
reveals only the internal word size, which happened to work previously
(and is the only option for older versions of Erlang) but doesn't work
for halfword emulators. Fallback to an argument of just wordsize for
older versions of Erlang.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d12607c" target="_blank">d12607c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d12607c');">
    add tests for authentication mechanisms
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d12607c');">
    // Christopher Faulet
  </div>
  <div id="msg-d12607c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d12607c');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 27 17:43:59 2011</strong>

add tests for authentication mechanisms
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ba8e4df" target="_blank">ba8e4df</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ba8e4df');">
    add a test for request shaping
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ba8e4df');">
    // Christopher Faulet
  </div>
  <div id="msg-ba8e4df" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ba8e4df');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 27 12:18:45 2011</strong>

add a test for request shaping
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/de9ca04" target="_blank">de9ca04</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-de9ca04');">
    add tests for argument rewriting
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-de9ca04');">
    // Christopher Faulet
  </div>
  <div id="msg-de9ca04" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-de9ca04');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 27 12:08:51 2011</strong>

add tests for argument rewriting
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6118169" target="_blank">6118169</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6118169');">
    add a test for external interpretation of php scripts
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6118169');">
    // Christopher Faulet
  </div>
  <div id="msg-6118169" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6118169');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 27 12:00:40 2011</strong>

add a test for external interpretation of php scripts
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/52e8852" target="_blank">52e8852</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-52e8852');">
    add tests for reentrant requests (scripts returning {page, P})
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-52e8852');">
    // Christopher Faulet
  </div>
  <div id="msg-52e8852" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-52e8852');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 27 11:50:32 2011</strong>

add tests for reentrant requests (scripts returning {page, P})
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/aab5243" target="_blank">aab5243</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-aab5243');">
    add a test for the 'expires' directive
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-aab5243');">
    // Christopher Faulet
  </div>
  <div id="msg-aab5243" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-aab5243');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 27 11:39:22 2011</strong>

add a test for the 'expires' directive
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4f3545f" target="_blank">4f3545f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4f3545f');">
    add tests for post and chunked transfer-encoding requests
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4f3545f');">
    // Christopher Faulet
  </div>
  <div id="msg-4f3545f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4f3545f');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 27 11:29:05 2011</strong>

add tests for post and chunked transfer-encoding requests
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/73c9dcb" target="_blank">73c9dcb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-73c9dcb');">
    fix filter_auths function
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-73c9dcb');">
    // Christopher Faulet
  </div>
  <div id="msg-73c9dcb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-73c9dcb');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 27 17:05:25 2011</strong>

fix filter_auths function
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d39553c" target="_blank">d39553c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d39553c');">
    fix get_chunked_client_data function
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d39553c');">
    // Christopher Faulet
  </div>
  <div id="msg-d39553c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d39553c');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu May 26 11:38:51 2011</strong>

fix get_chunked_client_data function

When the size of a chunk is a multiple of partial_post_size, last CRLF of
the chunk was not eaten.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0f828e8" target="_blank">0f828e8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0f828e8');">
    do not accumulate empty content
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0f828e8');">
    // Christopher Faulet
  </div>
  <div id="msg-0f828e8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0f828e8');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Fri May 27 09:15:17 2011</strong>

do not accumulate empty content

When a module used to rewrite arg records returns an HTTP response,
as a #rewrite_response{} record, if the content of this response is
empty, it must not be accumulated.
    </pre>
  </div>
</div>

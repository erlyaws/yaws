<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f5e6004" target="_blank">f5e6004</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f5e6004');">
    1.89 rel work
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f5e6004');">
    // Claes Wikstrom
  </div>
  <div id="msg-f5e6004" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f5e6004');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sat Sep 25 14:41:31 2010</strong>

1.89 rel work
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f3bfae3" target="_blank">f3bfae3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f3bfae3');">
    preparing for 1.89
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f3bfae3');">
    // Claes Wikstrom
  </div>
  <div id="msg-f3bfae3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f3bfae3');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@topaz.(none)&gt;</strong>
<strong>Date:   Sat Sep 25 14:22:40 2010</strong>

preparing for 1.89
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/896b8b1" target="_blank">896b8b1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-896b8b1');">
    Fix all.zip routine.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-896b8b1');">
    // Hans-Christian Esperer
  </div>
  <div id="msg-896b8b1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-896b8b1');">
    <pre>
<strong>Author: Hans-Christian Esperer &lt;hc@hcesperer.org&gt;</strong>
<strong>Date:   Wed Sep 15 19:48:02 2010</strong>

Fix all.zip routine.

all.zip should not not include .yaws files, nor directories
"protected" with an index file, nor directories protected by an auth
directive.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7f922a0" target="_blank">7f922a0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7f922a0');">
    adjust to R14B change in gen_tcp:recv for {packet,http} mode
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7f922a0');">
    // Steve Vinoski
  </div>
  <div id="msg-7f922a0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7f922a0');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Sep 15 15:54:21 2010</strong>

adjust to R14B change in gen_tcp:recv for {packet,http} mode

Where we handle the return value of gen_tcp:recv while reading headers
from the socket (such as with calls to yaws:do_recv), handle new R14B
return values as well as existing return values for previous
releases. According to the R14B release notes:

  For a socket in the HTTP packet mode, the return value from
  gen_tcp:recv/2,3 if there is an error in the header will be
  {ok,{http_error,String}} instead of {error,{http_error,String}} to
  be consistent with ssl:recv/2,3.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/62a7cd5" target="_blank">62a7cd5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-62a7cd5');">
    very minor whitespace fixes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-62a7cd5');">
    // Steve Vinoski
  </div>
  <div id="msg-62a7cd5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-62a7cd5');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Sep 2 00:12:02 2010</strong>

very minor whitespace fixes
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ad86ae3" target="_blank">ad86ae3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ad86ae3');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ad86ae3');">
    // Steve Vinoski
  </div>
  <div id="msg-ad86ae3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ad86ae3');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Sep 1 07:35:16 2010</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8e18857" target="_blank">8e18857</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8e18857');">
    for portability, use erlang:md5 rather than crypto md5
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8e18857');">
    // Steve Vinoski
  </div>
  <div id="msg-8e18857" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8e18857');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Sep 1 07:32:26 2010</strong>

for portability, use erlang:md5 rather than crypto md5

Since OpenSSL availability on Windows for working with the crypto
module is apparently questionable, use the erlang:md5 function in
place of the crypto md5 functions.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8e42e4e" target="_blank">8e42e4e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8e42e4e');">
    restore conf target, just make it use stdconf
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8e42e4e');">
    // Steve Vinoski
  </div>
  <div id="msg-8e42e4e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8e42e4e');">
    <pre>
<strong>Author: Steve Vinoski &lt;svinoski@erlang.(none)&gt;</strong>
<strong>Date:   Mon Aug 30 03:44:05 2010</strong>

restore conf target, just make it use stdconf

The previous change to test/t2/Makefile broke test/Makefile,
which expects t1 and t2 to have conf targets. Restore the conf
target but simply make it depend on stdconf.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d2d1f53" target="_blank">d2d1f53</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d2d1f53');">
    eliminate redundant conf target in t2 test Makefile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d2d1f53');">
    // Steve Vinoski
  </div>
  <div id="msg-d2d1f53" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d2d1f53');">
    <pre>
<strong>Author: Steve Vinoski &lt;svinoski@erlang.(none)&gt;</strong>
<strong>Date:   Mon Aug 30 02:37:01 2010</strong>

eliminate redundant conf target in t2 test Makefile

The test/t2/Makefile contained a conf target, but the
stdconf target in test/support/include.mk already does
what's needed. Eliminate conf and just use stdconf.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4ebda22" target="_blank">4ebda22</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4ebda22');">
    minor code cleanup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4ebda22');">
    // Steve Vinoski
  </div>
  <div id="msg-4ebda22" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4ebda22');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Aug 28 03:20:00 2010</strong>

minor code cleanup
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ab4f25e" target="_blank">ab4f25e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ab4f25e');">
    Merge branch 'master' of git://github.com/schemeway/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ab4f25e');">
    // Steve Vinoski
  </div>
  <div id="msg-ab4f25e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ab4f25e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Aug 28 02:29:55 2010</strong>

Merge branch 'master' of git://github.com/schemeway/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4871b16" target="_blank">4871b16</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4871b16');">
    Merge branch 'websockets' (Dominique Boucher)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4871b16');">
    // Dominique Boucher
  </div>
  <div id="msg-4871b16" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4871b16');">
    <pre>
<strong>Author: Dominique Boucher &lt;schemeway@gmail.com&gt;</strong>
<strong>Date:   Fri Aug 27 04:17:00 2010</strong>

Merge branch 'websockets' (Dominique Boucher)

Conflicts:
	src/yaws_websockets.erl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5a70e44" target="_blank">5a70e44</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5a70e44');">
    Reverted some names to ease merge.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5a70e44');">
    // Dominique Boucher
  </div>
  <div id="msg-5a70e44" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5a70e44');">
    <pre>
<strong>Author: Dominique Boucher &lt;schemeway@gmail.com&gt;</strong>
<strong>Date:   Fri Aug 27 04:15:14 2010</strong>

Reverted some names to ease merge.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1d1adb8" target="_blank">1d1adb8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1d1adb8');">
    teach fcgi_encode_name_value to handle non-flat lists
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1d1adb8');">
    // Steve Vinoski
  </div>
  <div id="msg-1d1adb8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1d1adb8');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Aug 27 04:11:10 2010</strong>

teach fcgi_encode_name_value to handle non-flat lists

The yaws_cgi:fcgi_encode_name_value function can now handle non-flat
lists for Name and Value, to allow callers to form them via io:format
for example.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fe2e42a" target="_blank">fe2e42a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fe2e42a');">
    Added support for websockets v76.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fe2e42a');">
    // Dominique Boucher
  </div>
  <div id="msg-fe2e42a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fe2e42a');">
    <pre>
<strong>Author: Dominique Boucher &lt;schemeway@gmail.com&gt;</strong>
<strong>Date:   Fri Aug 27 04:03:09 2010</strong>

Added support for websockets v76.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/af39f30" target="_blank">af39f30</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-af39f30');">
    Better error message when fds run out
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-af39f30');">
    // Claes Wikstrom
  </div>
  <div id="msg-af39f30" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-af39f30');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Aug 26 16:12:04 2010</strong>

Better error message when fds run out
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d1ecdbf" target="_blank">d1ecdbf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d1ecdbf');">
    avoid abnormal exit for FCGI worker failures
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d1ecdbf');">
    // Steve Vinoski
  </div>
  <div id="msg-d1ecdbf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d1ecdbf');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Aug 24 16:53:44 2010</strong>

avoid abnormal exit for FCGI worker failures

If an FCGI worker fails, which can happen for example if its TCP
connection to the FCGI server fails, we log an error message and
exit. Change the exit to always be normal to avoid filling logs with
additional process crash messages for any abnormal exits.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5155b86" target="_blank">5155b86</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5155b86');">
    Add auth_skip_docroot server config variable (Christian Hennig)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5155b86');">
    // Steve Vinoski
  </div>
  <div id="msg-5155b86" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5155b86');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Aug 23 06:47:43 2010</strong>

Add auth_skip_docroot server config variable (Christian Hennig)

At startup Yaws normally searches the entire docroot for .yaws_auth
files. For very large docroot directories, the time required to
perform the search can result in unacceptably long startup times. To
allow for large docroot directories, add auth_skip_docroot server
config variable which, if set to true, prevents Yaws from searching
the docroot for .yaws_auth files. The variable defaults to false.

Add documentation for auth_skip_docroot variable.

Also clean up some whitespace and typos in the yaws.conf.5 man page.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/88c4e83" target="_blank">88c4e83</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-88c4e83');">
    Fix use_old_ssl=bool() for R14A
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-88c4e83');">
    // Joseph Wayne Norton
  </div>
  <div id="msg-88c4e83" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-88c4e83');">
    <pre>
<strong>Author: Joseph Wayne Norton &lt;norton@alum.mit.edu&gt;</strong>
<strong>Date:   Fri Aug 13 15:51:16 2010</strong>

Fix use_old_ssl=bool() for R14A

Starting with R14A release, ssl_impl() must always be set for forwards and backwards compatibility.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/94b9b15" target="_blank">94b9b15</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-94b9b15');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-94b9b15');">
    // Steve Vinoski
  </div>
  <div id="msg-94b9b15" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-94b9b15');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Aug 3 07:12:54 2010</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ee35df4" target="_blank">ee35df4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ee35df4');">
    add --umask option to yaws script
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ee35df4');">
    // Steve Vinoski
  </div>
  <div id="msg-ee35df4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ee35df4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Aug 3 07:09:34 2010</strong>

add --umask option to yaws script

Users can use the --umask option to set the process umask for the yaws
daemon. By default the daemon inherits the umask of its parent process.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a1a368b" target="_blank">a1a368b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a1a368b');">
    Merge remote branch 'upstream/master'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a1a368b');">
    // Mikael Karlsson
  </div>
  <div id="msg-a1a368b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a1a368b');">
    <pre>
<strong>Author: Mikael Karlsson &lt;karlsson.rm@gmail.com&gt;</strong>
<strong>Date:   Mon Jul 26 10:47:26 2010</strong>

Merge remote branch 'upstream/master'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ebf7bc5" target="_blank">ebf7bc5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ebf7bc5');">
    Modifed yapp application - added simple example and changed oroginal authors mail address.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ebf7bc5');">
    // Mikael Karlsson
  </div>
  <div id="msg-ebf7bc5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ebf7bc5');">
    <pre>
<strong>Author: Mikael Karlsson &lt;karlsson.rm@gmail.com&gt;</strong>
<strong>Date:   Mon Jul 26 10:40:59 2010</strong>

Modifed yapp application - added simple example and changed oroginal authors mail address.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/08fcadd" target="_blank">08fcadd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-08fcadd');">
    add generated doc files to .gitignore
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-08fcadd');">
    // Steve Vinoski
  </div>
  <div id="msg-08fcadd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-08fcadd');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Jul 24 22:31:16 2010</strong>

add generated doc files to .gitignore
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7c6fd31" target="_blank">7c6fd31</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7c6fd31');">
    performance enhancements
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7c6fd31');">
    // Steve Vinoski
  </div>
  <div id="msg-7c6fd31" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7c6fd31');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Jul 24 21:18:37 2010</strong>

performance enhancements

Based on a yaws configuration consisting of an appmod registered on
"/" and simply returning {status, 204} with no response body, these
changes resulted in a 6.8% performance improvement on a modest older
Ubuntu 8.10 2.40GHz Core 2 Duo box with 2GB of RAM. The changes were
identified by analyzing fprof output, and performance was measured
using ab (ApacheBench) running on the same host as shown below:

ab -k -n 1000000 -c 1000 http://localhost:8200/

The access log was disabled and the listen backlog was set to 512, but
other than that the default configuration was used. Requests per
second went from ~14750 to ~15750. YMMV.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8b5e74b" target="_blank">8b5e74b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8b5e74b');">
    add support for PHP FCGI applications (Hans-Christian Esperer)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8b5e74b');">
    // Steve Vinoski
  </div>
  <div id="msg-8b5e74b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8b5e74b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Jul 21 02:26:40 2010</strong>

add support for PHP FCGI applications (Hans-Christian Esperer)

Add server configuration support for PHP FCGI applications. Modify
dispatching support to invoke a PHP FCGI server for .php files if
configured.

Modify existing FCGI application server configuration to use a single
separate fields. Use the same approach for the phpfcgi field. Patch up
yaws_cgi.erl for these changes.

Enhance yaws.conf.5 and yaws.tex to describe the new PHP FCGI
support.

Also fix a number of editorial issues in yaws.tex.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9c23ce4" target="_blank">9c23ce4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9c23ce4');">
    fix cached process counter (Hans-Christian Esperer)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9c23ce4');">
    // Steve Vinoski
  </div>
  <div id="msg-9c23ce4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9c23ce4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Jul 16 21:07:33 2010</strong>

fix cached process counter (Hans-Christian Esperer)

Stopping old cached processes was not properly resetting the cached
process counter, eventually resulting in a lack of any cached
processes.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/71ab858" target="_blank">71ab858</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-71ab858');">
    Merge remote branch 'upstream/master'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-71ab858');">
    // Mikael Karlsson
  </div>
  <div id="msg-71ab858" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-71ab858');">
    <pre>
<strong>Author: Mikael Karlsson &lt;karlsson.rm@gmail.com&gt;</strong>
<strong>Date:   Thu Jul 1 20:41:41 2010</strong>

Merge remote branch 'upstream/master'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ca1c8b1" target="_blank">ca1c8b1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ca1c8b1');">
    Fixed a number of compiler warnings and html validation errors
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ca1c8b1');">
    // Mikael Karlsson
  </div>
  <div id="msg-ca1c8b1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ca1c8b1');">
    <pre>
<strong>Author: Mikael Karlsson &lt;karlsson.rm@gmail.com&gt;</strong>
<strong>Date:   Thu Jul 1 20:40:56 2010</strong>

Fixed a number of compiler warnings and html validation errors
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/641939a" target="_blank">641939a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-641939a');">
    return part headers from yaws_multipart:read_multipart_form
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-641939a');">
    // Steve Vinoski
  </div>
  <div id="msg-641939a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-641939a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Jun 27 23:10:47 2010</strong>

return part headers from yaws_multipart:read_multipart_form

Each part of a multipart/form-data message can have part-specific
headers such as Content-Type and Content-Transfer-Encoding. This
changes adds these headers to the parameter lists within the dict
returned from yaws_multipart:read_multipart_form and to the parameter
list returned from yaws_api:parse_multipart_post.

Modify the documentation to reflect the fact that these headers are
now returned if present. Also fixed a coding error in the existing
multipart example in yaws.tex.

Add a new unit test to multipart_post_parsing.erl to verify the return
value of yaws_multipart:read_multipart_form. The unit test previously
checked only yaws_api:parse_multipart_post.

This work is based on a patch submitted by Dilshod Temirkhodjaev.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/144ac13" target="_blank">144ac13</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-144ac13');">
    docs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-144ac13');">
    // Claes Wikstrom
  </div>
  <div id="msg-144ac13" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-144ac13');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Jun 21 22:51:13 2010</strong>

docs
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1407f5e" target="_blank">1407f5e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1407f5e');">
    docs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1407f5e');">
    // Claes Wikstrom
  </div>
  <div id="msg-1407f5e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1407f5e');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Jun 15 19:04:59 2010</strong>

docs
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/02feeaa" target="_blank">02feeaa</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-02feeaa');">
    patch for no-return-in-nonvoid-function error
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-02feeaa');">
    // Alexander Simonov
  </div>
  <div id="msg-02feeaa" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-02feeaa');">
    <pre>
<strong>Author: Alexander Simonov &lt;alex@simonov.in.ua&gt;</strong>
<strong>Date:   Fri Jun 4 12:54:23 2010</strong>

patch for no-return-in-nonvoid-function error
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ebb4323" target="_blank">ebb4323</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ebb4323');">
    updates to make two-mode-mode work with emacs 23
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ebb4323');">
    // Steve Vinoski
  </div>
  <div id="msg-ebb4323" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ebb4323');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon May 31 04:44:09 2010</strong>

updates to make two-mode-mode work with emacs 23
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cd6d9be" target="_blank">cd6d9be</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cd6d9be');">
    fix typo in documentation example
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cd6d9be');">
    // Steve Vinoski
  </div>
  <div id="msg-cd6d9be" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cd6d9be');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 29 13:21:22 2010</strong>

fix typo in documentation example
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8cedffc" target="_blank">8cedffc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8cedffc');">
    remove local copy of erlsom.hrl (Mikael Karlsson)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8cedffc');">
    // Steve Vinoski
  </div>
  <div id="msg-8cedffc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8cedffc');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat May 29 13:20:14 2010</strong>

remove local copy of erlsom.hrl (Mikael Karlsson)

This was meant to be part of commit 358c69d53.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9608fa1" target="_blank">9608fa1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9608fa1');">
    doc error
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9608fa1');">
    // Claes Wikstrom
  </div>
  <div id="msg-9608fa1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9608fa1');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Fri May 28 10:48:44 2010</strong>

doc error
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/358c69d" target="_blank">358c69d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-358c69d');">
    remove compile time dependencies on erlsom hrl files (Mikael Karlsson)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-358c69d');">
    // Steve Vinoski
  </div>
  <div id="msg-358c69d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-358c69d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu May 27 06:29:58 2010</strong>

remove compile time dependencies on erlsom hrl files (Mikael Karlsson)

Also, due to problems with yaws_soap_lib:priv_dir/0, add some extra
code that makes use of code:priv_dir.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cc1575b" target="_blank">cc1575b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cc1575b');">
    do not append "/" to revproxy prefix for static config
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cc1575b');">
    // Steve Vinoski
  </div>
  <div id="msg-cc1575b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cc1575b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri May 21 17:10:50 2010</strong>

do not append "/" to revproxy prefix for static config

Commit 281d993 changed reverse proxy prefix handling to correct a
problem with leading slashes. This fix should have been part of that
commit as well, as it eliminates a previous change to yaws_config.erl
to ensure prefixes handled "/" correctly given how they were being
parsed in the revproxy code.

With this change, both static config and embedded startup config
should work correctly for revproxy.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/281d993" target="_blank">281d993</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-281d993');">
    allow leading slash on revproxy prefix
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-281d993');">
    // Steve Vinoski
  </div>
  <div id="msg-281d993" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-281d993');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu May 20 17:06:50 2010</strong>

allow leading slash on revproxy prefix

Trying to "mount" a remote site underneath a path other than "/" under
our docroot with a revproxy was hitting function clause errors in
yaws_revproxy.erl. Change the strip_prefix function there to properly
handle prefixes with leading slashes.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5da72a3" target="_blank">5da72a3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5da72a3');">
    Allow "stream processes" to close the client socket
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5da72a3');">
    // Steve Vinoski
  </div>
  <div id="msg-5da72a3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5da72a3');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed May 12 06:53:21 2010</strong>

Allow "stream processes" to close the client socket

Yaws allows application processes to take control of the client socket
via the streamcontent_from_pid directive. Sometimes during the course
of its data delivery the application might find that it needs to close
the socket, but the original design did not provide a way for the
application to tell Yaws the socket was closed. The result was that a
message like the one below would show up in the Yaws log whenever a
closed socket was returned to Yaws, due to Yaws trying to continue to
use the socket as if it were still open:

     =ERROR REPORT==== 12-May-2010::00:27:05 ===
     Yaws process died: {{badmatch,{error,einval}},
                         [{yaws,setopts,3},
                          {yaws,http_recv_request,2},
                          {yaws,do_http_get_headers,2},
                          {yaws,http_get_headers,2},
                          {yaws_server,aloop,3},
                          {yaws_server,acceptor0,2},
                          {proc_lib,init_p_do_apply,3}]}

Change yaws_api:stream_process_end/2 to allow the application to pass
the atom 'closed' as the first argument, in place of the socket
argument. This informs Yaws that the socket is closed and prevents it
from trying to keep using it, thus avoiding errors like the one above.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fd5e602" target="_blank">fd5e602</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fd5e602');">
    Augment yaws man page with --erlarg argument quoting info
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fd5e602');">
    // Steve Vinoski
  </div>
  <div id="msg-fd5e602" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fd5e602');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue May 11 07:50:56 2010</strong>

Augment yaws man page with --erlarg argument quoting info

If a multi-word argument is passed to --erlarg, it must be quoted in
the calling shell so that yaws sees it as a single argument. Add
details to the man page about this, and also show an example of how
single quotes within such a multi-word argument must receive extra
quoting.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/17be957" target="_blank">17be957</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-17be957');">
    Use gconf keepalive_timeout for connection lifetime (Brady McCary)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-17be957');">
    // Steve Vinoski
  </div>
  <div id="msg-17be957" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-17be957');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon May 10 19:51:37 2010</strong>

Use gconf keepalive_timeout for connection lifetime (Brady McCary)

Previously persistent connections were held open for a hard-coded 30
seconds, and if no request was received in that time, Yaws closed the
connection. Change this behavior to use the gconf keepalive_timeout
field to determine the amount of time the connection is held open
rather than using a hard-coded timeout. The default is still 30
seconds, but users can change the default by specifying a different
value for keepalive_timeout in their Yaws configuration.

Also, fix yaws.tex to document the keepalive_timeout setting as well
as other recent changes such as max_connections and keepalive_maxuses.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9194d12" target="_blank">9194d12</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9194d12');">
    handle multi-word arguments for heart restart command
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9194d12');">
    // Steve Vinoski
  </div>
  <div id="msg-9194d12" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9194d12');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun May 9 06:08:24 2010</strong>

handle multi-word arguments for heart restart command
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a1f8536" target="_blank">a1f8536</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a1f8536');">
    remove leftover debug io:format call
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a1f8536');">
    // Steve Vinoski
  </div>
  <div id="msg-a1f8536" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a1f8536');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 28 16:58:21 2010</strong>

remove leftover debug io:format call
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/82829b1" target="_blank">82829b1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-82829b1');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-82829b1');">
    // Steve Vinoski
  </div>
  <div id="msg-82829b1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-82829b1');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 27 23:28:41 2010</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0fa9db4" target="_blank">0fa9db4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0fa9db4');">
    fix typos on front page
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0fa9db4');">
    // Steve Vinoski
  </div>
  <div id="msg-0fa9db4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0fa9db4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 27 23:25:56 2010</strong>

fix typos on front page
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5fe881b" target="_blank">5fe881b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5fe881b');">
    Fix yaws_api:parse_set_cookie
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5fe881b');">
    // Anders Nygren
  </div>
  <div id="msg-5fe881b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5fe881b');">
    <pre>
<strong>Author: Anders Nygren &lt;anders.nygren@gmail.com&gt;</strong>
<strong>Date:   Mon Apr 26 22:19:08 2010</strong>

Fix yaws_api:parse_set_cookie

Fix bug where yaws_api:parse_set_cookie tried to convert a record to lower case.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1c20a97" target="_blank">1c20a97</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1c20a97');">
    patch by Sergey Shilov to set old_ssl for embedded mode
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1c20a97');">
    // Claes Wikstrom
  </div>
  <div id="msg-1c20a97" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1c20a97');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Thu Apr 22 15:49:13 2010</strong>

patch by Sergey Shilov to set old_ssl for embedded mode
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0809a53" target="_blank">0809a53</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0809a53');">
    fix socket usage for FCGI authorization (bruno rijsman, steve vinoski)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0809a53');">
    // Steve Vinoski
  </div>
  <div id="msg-0809a53" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0809a53');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 21 07:14:23 2010</strong>

fix socket usage for FCGI authorization (bruno rijsman, steve vinoski)

The FCGI authorization code was holding the socket to the FCGI server
open until Yaws finished replying to the client. Change handling for
FCGI authorization to gather the response and then return to the
caller, closing the socket to the FCGI server much sooner than before.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ade3631" target="_blank">ade3631</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ade3631');">
    work around an erlang:open_port bug for CGI QUERY_STRING env var
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ade3631');">
    // Steve Vinoski
  </div>
  <div id="msg-ade3631" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ade3631');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Apr 16 08:52:56 2010</strong>

work around an erlang:open_port bug for CGI QUERY_STRING env var

Some versions of erlang:open_port can't handle query strings that end
with an equal sign. This is because the broken versions treat
environment variable strings ending with '=' as environment variable
names intended to be deleted from the environment, i.e. as if they
have no value. The result is that no QUERY_STRING environment variable
gets set for these cases. We work around this by appending a &
character to any query string that ends in =.

Also, clean up some comment lines that extended past 80 characters.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0446f2b" target="_blank">0446f2b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0446f2b');">
    Added a new configuration parameter called "keepalive_maxuses" which allows the yaws admin to close persistant connections after X number of uses. This is handy for some types of system which may generate a lot of garbage in the process in the erlang VM or for other reasons where you want to limit how many requests can be processed on a single connection. The default setting remains unlimited.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0446f2b');">
    // Thomas O'Dowd
  </div>
  <div id="msg-0446f2b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0446f2b');">
    <pre>
<strong>Author: Thomas O'Dowd &lt;tpodowd@geminimobile.com&gt;</strong>
<strong>Date:   Wed Apr 14 08:08:35 2010</strong>

Added a new configuration parameter called "keepalive_maxuses" which allows
the yaws admin to close persistant connections after X number of uses. This
is handy for some types of system which may generate a lot of garbage in the
process in the erlang VM or for other reasons where you want to limit how
many requests can be processed on a single connection. The default setting
remains unlimited.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/59432b5" target="_blank">59432b5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-59432b5');">
    add www/*.txt files, which are generated by tests
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-59432b5');">
    // Steve Vinoski
  </div>
  <div id="msg-59432b5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-59432b5');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Apr 12 19:47:34 2010</strong>

add www/*.txt files, which are generated by tests
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d0b184d" target="_blank">d0b184d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d0b184d');">
    Merge branch 'embedded'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d0b184d');">
    // Steve Vinoski
  </div>
  <div id="msg-d0b184d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d0b184d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Apr 12 19:13:09 2010</strong>

Merge branch 'embedded'
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6b26557" target="_blank">6b26557</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6b26557');">
    improved embedded support
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6b26557');">
    // Steve Vinoski
  </div>
  <div id="msg-6b26557" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6b26557');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Apr 10 05:19:53 2010</strong>

improved embedded support

Add yaws_api:embedded_start_conf functions that take the same
arguments as yaws:start_embedded but instead of starting the yaws
application, return the gconf, sconf, and supervisor child specs
corresponding to the supplied arguments. When the application doing
the embedding is ready to start yaws, it can use the child specs to
start yaws under its own supervisors, and can then pass the gconf and
sconf to yaws_api:setconf to configure yaws. This approach gives the
application more control over how yaws is started and supervised.

Reimplement yaws:start_embedded in terms of the new
yaws_api:embedded_start_conf, maintaining backward compatibility.

Change www/embed.yaws and its example ybed code to describe the new
yaws_api:embedded_start_conf functions.

Updated web documentation and the yaws.tex file.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/34865d9" target="_blank">34865d9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-34865d9');">
    macosx description for privbind
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-34865d9');">
    // Claes Wikstrom
  </div>
  <div id="msg-34865d9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-34865d9');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Mon Apr 12 12:36:07 2010</strong>

macosx description for privbind
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/87e5714" target="_blank">87e5714</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-87e5714');">
    Merge branch 'process_options' of git://github.com/tpodowd/yaws into po
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-87e5714');">
    // Claes Wikstrom
  </div>
  <div id="msg-87e5714" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-87e5714');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Mon Apr 12 09:36:17 2010</strong>

Merge branch 'process_options' of git://github.com/tpodowd/yaws into po
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7334cee" target="_blank">7334cee</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7334cee');">
    added gc/sc debug outputs for -d flag
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7334cee');">
    // Claes Wikstrom
  </div>
  <div id="msg-7334cee" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7334cee');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sun Apr 11 23:08:45 2010</strong>

added gc/sc debug outputs for -d flag
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/968de1b" target="_blank">968de1b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-968de1b');">
    add files generated by tests and more build-generated files
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-968de1b');">
    // Steve Vinoski
  </div>
  <div id="msg-968de1b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-968de1b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Apr 10 05:16:23 2010</strong>

add files generated by tests and more build-generated files
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/aa217d8" target="_blank">aa217d8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-aa217d8');">
    Added a new configuration parameter to allow the administrator to control the erlang virtual machines garbage collection knobs when spawning a new process to handle incoming connections. The option is called "process_options". Its default value is the empty list which uses erlangs default options as is the case now. This is useful for long lived connections which can generate a lot of garbage if left untapped. Use with care :-)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-aa217d8');">
    // Thomas O'Dowd
  </div>
  <div id="msg-aa217d8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-aa217d8');">
    <pre>
<strong>Author: Thomas O'Dowd &lt;tpodowd@geminimobile.com&gt;</strong>
<strong>Date:   Wed Apr 7 12:14:44 2010</strong>

Added a new configuration parameter to allow the administrator to control
the erlang virtual machines garbage collection knobs when spawning a new
process to handle incoming connections. The option is called "process_options".
Its default value is the empty list which uses erlangs default options as
is the case now. This is useful for long lived connections which can generate
a lot of garbage if left untapped. Use with care :-)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d57f77c" target="_blank">d57f77c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d57f77c');">
    yaws 1.88 commit
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d57f77c');">
    // Claes Wikstrom
  </div>
  <div id="msg-d57f77c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d57f77c');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Mar 18 22:48:41 2010</strong>

yaws 1.88 commit
    </pre>
  </div>
</div>

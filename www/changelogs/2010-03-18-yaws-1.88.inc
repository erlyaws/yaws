<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c30bb56" target="_blank">c30bb56</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c30bb56');">
    prepare for 1.88
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c30bb56');">
    // Claes Wikstrom
  </div>
  <div id="msg-c30bb56" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c30bb56');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Mar 18 22:02:47 2010</strong>

prepare for 1.88
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/93b6f82" target="_blank">93b6f82</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-93b6f82');">
    ssl support for websockets, patch by wde
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-93b6f82');">
    // Claes Wikstrom
  </div>
  <div id="msg-93b6f82" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-93b6f82');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Thu Mar 18 13:04:07 2010</strong>

ssl support for websockets, patch by wde
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4fec139" target="_blank">4fec139</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4fec139');">
    fixed a broken link and added a Makefile that test compiles all our yaws files making sure nothing broken get shipped
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4fec139');">
    // Claes Wikstrom
  </div>
  <div id="msg-4fec139" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4fec139');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Mar 10 22:28:58 2010</strong>

fixed a broken link and added a Makefile that test compiles all our yaws files making sure nothing broken get shipped
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9dfb58e" target="_blank">9dfb58e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9dfb58e');">
    Erroneous common log format entries, time should be surrounded by brackets
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9dfb58e');">
    // Claes Wikstrom
  </div>
  <div id="msg-9dfb58e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9dfb58e');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Mar 10 21:57:13 2010</strong>

Erroneous common log format entries, time should be surrounded by brackets
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/92037d8" target="_blank">92037d8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-92037d8');">
    When executing yaws --hup the order of the hosts in a virthost group wan't maintained, thus breaking the feature of pick first virthost on nomatch
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-92037d8');">
    // Claes Wikstrom
  </div>
  <div id="msg-92037d8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-92037d8');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Mar 9 20:52:21 2010</strong>

When executing yaws --hup the order of the hosts in a virthost group wan't maintained, thus breaking the feature of pick first virthost on nomatch
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ee0aee7" target="_blank">ee0aee7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ee0aee7');">
    set nodelay on FCGI TCP connection, and avoid 0-length FCGI gen_tcp:recv
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ee0aee7');">
    // Steve Vinoski
  </div>
  <div id="msg-ee0aee7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ee0aee7');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Mar 7 22:52:21 2010</strong>

set nodelay on FCGI TCP connection, and avoid 0-length FCGI gen_tcp:recv

Set {nodelay,true} on the TCP connection to the FCGI server to improve
performance of small requests.

The code receives packets from the FCGI server and for some cases was
extracting length fields from some packets and then making further
gen_tcp:recv calls based on those length values. The code was not
checking for length values of 0 before calling recv, and passing 0 to
recv means to return all available bytes. For both correctness and
performance, the code must avoid the recv calls altogether when the
length is 0.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/56f41b6" target="_blank">56f41b6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-56f41b6');">
    convert error atoms into error strings for fcgi_worker_fail
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-56f41b6');">
    // Steve Vinoski
  </div>
  <div id="msg-56f41b6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-56f41b6');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Mar 3 01:14:53 2010</strong>

convert error atoms into error strings for fcgi_worker_fail
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/561a09c" target="_blank">561a09c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-561a09c');">
    clarification on ssl depth param
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-561a09c');">
    // Claes Wikstrom
  </div>
  <div id="msg-561a09c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-561a09c');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Tue Mar 2 16:32:49 2010</strong>

clarification on ssl depth param
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cfb32f4" target="_blank">cfb32f4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cfb32f4');">
    spelling corrections
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cfb32f4');">
    // Claes Wikstrom
  </div>
  <div id="msg-cfb32f4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cfb32f4');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Tue Mar 2 16:29:21 2010</strong>

spelling corrections
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7f2951c" target="_blank">7f2951c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7f2951c');">
    IpV6 support for listen socket
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7f2951c');">
    // Claes Wikstrom
  </div>
  <div id="msg-7f2951c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7f2951c');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Tue Mar 2 15:41:06 2010</strong>

IpV6 support for listen socket
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3cd9bc8" target="_blank">3cd9bc8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3cd9bc8');">
    Merge branch 'master' of git@github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3cd9bc8');">
    // Claes Wikstrom
  </div>
  <div id="msg-3cd9bc8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3cd9bc8');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Tue Mar 2 15:29:50 2010</strong>

Merge branch 'master' of git@github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/662d1f3" target="_blank">662d1f3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-662d1f3');">
    The default value for partial_post_size was nolimit, not a good default value and also erroneoulsy documented
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-662d1f3');">
    // Claes Wikstrom
  </div>
  <div id="msg-662d1f3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-662d1f3');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@zoe.(none)&gt;</strong>
<strong>Date:   Tue Mar 2 15:29:42 2010</strong>

The default value for partial_post_size was nolimit, not a good default value and also erroneoulsy documented
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/937925b" target="_blank">937925b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-937925b');">
    An atempt at having utf8 characters in the host names for yaws servers
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-937925b');">
    // Claes Wikstrom
  </div>
  <div id="msg-937925b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-937925b');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Feb 26 14:51:26 2010</strong>

An atempt at having utf8 characters in the host names for yaws servers
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ff2fc18" target="_blank">ff2fc18</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ff2fc18');">
    dialyzer work
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ff2fc18');">
    // Claes Wikstrom
  </div>
  <div id="msg-ff2fc18" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ff2fc18');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sat Feb 20 14:52:35 2010</strong>

dialyzer work
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6f0ab17" target="_blank">6f0ab17</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6f0ab17');">
    Anders Dahlin found that yaws log code doesn't delete the gen_event handler it adds error logger when terminating, that means that restarting yaws leaves old processes hanging around
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6f0ab17');">
    // Claes Wikstrom
  </div>
  <div id="msg-6f0ab17" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6f0ab17');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Feb 19 11:12:40 2010</strong>

Anders Dahlin found that yaws log code doesn't delete the gen_event handler it adds error logger when terminating, that means that restarting yaws leaves old processes hanging around
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b45c0a4" target="_blank">b45c0a4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b45c0a4');">
    dialyzer work
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b45c0a4');">
    // Claes Wikstrom
  </div>
  <div id="msg-b45c0a4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b45c0a4');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Feb 18 18:06:38 2010</strong>

dialyzer work
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4cfb13c" target="_blank">4cfb13c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4cfb13c');">
    Merge branch 'master' of git@github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4cfb13c');">
    // Claes Wikstrom
  </div>
  <div id="msg-4cfb13c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4cfb13c');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Feb 18 17:49:06 2010</strong>

Merge branch 'master' of git@github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2c2f776" target="_blank">2c2f776</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2c2f776');">
    Closed Issue #31, made parsing of yaws.conf more tight complaining on e.g allowd_scripts = [ yaws ]
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2c2f776');">
    // Claes Wikstrom
  </div>
  <div id="msg-2c2f776" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2c2f776');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Feb 18 15:44:11 2010</strong>

Closed Issue #31, made parsing of yaws.conf more tight complaining on e.g allowd_scripts = [ yaws ]
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f18fc6c" target="_blank">f18fc6c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f18fc6c');">
    fix for FastCGI/PHP Issue (http://github.com/klacke/yaws/issues#issue/30) (davide)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f18fc6c');">
    // Steve Vinoski
  </div>
  <div id="msg-f18fc6c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f18fc6c');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Feb 17 07:08:47 2010</strong>

fix for FastCGI/PHP Issue (http://github.com/klacke/yaws/issues#issue/30) (davide)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ee5b63f" target="_blank">ee5b63f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ee5b63f');">
    increase FCGI read timeout
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ee5b63f');">
    // Steve Vinoski
  </div>
  <div id="msg-ee5b63f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ee5b63f');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Feb 17 07:01:36 2010</strong>

increase FCGI read timeout
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4696b6b" target="_blank">4696b6b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4696b6b');">
    auth through .yaws_auth files has become broken, this fixes it
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4696b6b');">
    // Claes Wikstrom
  </div>
  <div id="msg-4696b6b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4696b6b');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Feb 11 23:51:17 2010</strong>

auth through .yaws_auth files has become broken, this fixes it
    </pre>
  </div>
</div>

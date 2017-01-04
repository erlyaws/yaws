<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0a9b80f" target="_blank">0a9b80f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0a9b80f');">
    prepare for 1.98
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0a9b80f');">
    // Claes Wikstrom
  </div>
  <div id="msg-0a9b80f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0a9b80f');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Nov 4 19:26:06 2013</strong>

prepare for 1.98
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/597544b" target="_blank">597544b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-597544b');">
    Bad HTTP 3.0 message sent to yaws, crashed the server entirely
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-597544b');">
    // Claes Wikstrom
  </div>
  <div id="msg-597544b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-597544b');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Oct 30 17:16:00 2013</strong>

Bad HTTP 3.0 message sent to yaws, crashed the server entirely
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a04e38a" target="_blank">a04e38a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a04e38a');">
    rebar compile eunit compile_only=true fails for yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a04e38a');">
    // Joseph Wayne Norton
  </div>
  <div id="msg-a04e38a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a04e38a');">
    <pre>
<strong>Author: Joseph Wayne Norton &lt;norton@alum.mit.edu&gt;</strong>
<strong>Date:   Tue Oct 22 17:01:30 2013</strong>

rebar compile eunit compile_only=true fails for yaws

Changes to allow yaws to be compiled by rebar for eunit testing.  This
change is helpful when embedding yaws as a dependency of an Erlang
application.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4167754" target="_blank">4167754</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4167754');">
    Use Yaws linked-in driver by default to send large file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4167754');">
    // Christopher Faulet
  </div>
  <div id="msg-4167754" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4167754');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Oct 21 12:18:41 2013</strong>

Use Yaws linked-in driver by default to send large file

In a previous commit (c887a374), the use of sendfile (OTP and Yaws versions)
was disabled. The OTP version is buggy, so we recommand to not use
it. But the Yaws linked-in driver can safely be used. So, there is no reason
to not use it. Now, it is the default method.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7aafdc5" target="_blank">7aafdc5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7aafdc5');">
    fix rebar.config.script for YAWS_SOAP build
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7aafdc5');">
    // Steve Vinoski
  </div>
  <div id="msg-7aafdc5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7aafdc5');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Oct 18 12:51:04 2013</strong>

fix rebar.config.script for YAWS_SOAP build
    </pre>
  </div>
</div>

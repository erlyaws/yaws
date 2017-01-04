<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/20585c2" target="_blank">20585c2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-20585c2');">
    Add quotes to Makefile.am when building rels
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-20585c2');">
    // Klacke
  </div>
  <div id="msg-20585c2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-20585c2');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Mon Jul 13 19:45:26 2015</strong>

Add quotes to Makefile.am when building rels
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/89b4374" target="_blank">89b4374</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-89b4374');">
    Yaws 2.0 release work
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-89b4374');">
    // Klacke
  </div>
  <div id="msg-89b4374" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-89b4374');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Mon Jul 13 19:35:50 2015</strong>

Yaws 2.0 release work
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b601eb3" target="_blank">b601eb3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b601eb3');">
    Update known dialyzer warnings for Erlang 18.0 changes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b601eb3');">
    // Steve Vinoski
  </div>
  <div id="msg-b601eb3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b601eb3');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Jul 13 13:06:14 2015</strong>

Update known dialyzer warnings for Erlang 18.0 changes
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/316b4c6" target="_blank">316b4c6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-316b4c6');">
    Fix Yaws build for Erlang 18.0
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-316b4c6');">
    // Steve Vinoski
  </div>
  <div id="msg-316b4c6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-316b4c6');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Jul 12 00:27:22 2015</strong>

Fix Yaws build for Erlang 18.0

Erlang 18.0 deprecated erlang:now/0 and this was causing compilation
errors for Yaws. Add new portability functions to the yaws module:
yaws:unique_triple() returns a unique 3-tuple of positive integers,
and yaws:get_time_tuple() returns the current time in the same 3-tuple
format as erlang:now(). Fix all Yaws modules as needed to call these
functions as appropriate instead of calling erlang:now(). Add support
to configure.ac, include.mk, and rebar.config.script such that these
portability functions are correctly conditionally compiled depending
on the Erlang erts version in use.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d43bb5d" target="_blank">d43bb5d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d43bb5d');">
    Merge pull request #223 from zephyrean/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d43bb5d');">
    // Steve Vinoski
  </div>
  <div id="msg-d43bb5d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d43bb5d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Jun 6 00:01:38 2015</strong>

Merge pull request #223 from zephyrean/master

Fix a bug where decoding of haxe string depended upon completely irrelevant native file system encoding
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e4a8499" target="_blank">e4a8499</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e4a8499');">
    fix a bug where decoding of haxe string depended upon completely irrelevant native file system encoding
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e4a8499');">
    // Mike
  </div>
  <div id="msg-e4a8499" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e4a8499');">
    <pre>
<strong>Author: Mike &lt;mpustovalov@gmail.com&gt;</strong>
<strong>Date:   Thu Jun 4 17:54:50 2015</strong>

fix a bug where decoding of haxe string depended upon completely irrelevant native file system encoding
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/99407cb" target="_blank">99407cb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-99407cb');">
    Update rebar.config to require Erlang/OTP R14B02 or higher to compile Yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-99407cb');">
    // Christopher Faulet
  </div>
  <div id="msg-99407cb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-99407cb');">
    <pre>
<strong>Author: Christopher Faulet &lt;cfaulet@qualys.com&gt;</strong>
<strong>Date:   Thu May 7 15:48:30 2015</strong>

Update rebar.config to require Erlang/OTP R14B02 or higher to compile Yaws

The requirement was added for the autotools (see ff010694), but not for Rebar.
This was an oversight on my part.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d053787" target="_blank">d053787</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d053787');">
    Fixed broken junk image
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d053787');">
    // Klacke
  </div>
  <div id="msg-d053787" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d053787');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Thu Apr 30 00:14:30 2015</strong>

Fixed broken junk image
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8001863" target="_blank">8001863</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8001863');">
    Renew self-signed ssl certificat to use the right signature algorithm
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8001863');">
    // Christopher Faulet
  </div>
  <div id="msg-8001863" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8001863');">
    <pre>
<strong>Author: Christopher Faulet &lt;cfaulet@qualys.com&gt;</strong>
<strong>Date:   Wed Apr 29 09:39:24 2015</strong>

Renew self-signed ssl certificat to use the right signature algorithm

The old certificat was signed with sha256WithRSAEncryption algorithm, which is
not supported by Erlang releases older than R15B01. So the testsuite failed on
these releases.
The new certificate is signed with sha1WithRSAEncryption.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f6d26c6" target="_blank">f6d26c6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f6d26c6');">
    Check support for the SSL 'log_alert' option
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f6d26c6');">
    // Christopher Faulet
  </div>
  <div id="msg-f6d26c6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f6d26c6');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Apr 27 21:33:17 2015</strong>

Check support for the SSL 'log_alert' option

This fix the commit 1bf94af6 for Erlang releases older than R16B02.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6cfcabd" target="_blank">6cfcabd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6cfcabd');">
    Fix HTTP authentication when user-defined module is used
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6cfcabd');">
    // Christopher Faulet
  </div>
  <div id="msg-6cfcabd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6cfcabd');">
    <pre>
<strong>Author: Christopher Faulet &lt;cfaulet@qualys.com&gt;</strong>
<strong>Date:   Wed Apr 22 08:52:46 2015</strong>

Fix HTTP authentication when user-defined module is used
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fd1325a" target="_blank">fd1325a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fd1325a');">
    Fix #216: handle exhtml return values from out/1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fd1325a');">
    // Steve Vinoski
  </div>
  <div id="msg-fd1325a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fd1325a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Apr 16 19:49:07 2015</strong>

Fix #216: handle exhtml return values from out/1

Do not crash if out/1 returns exhtml. Add a new regression test for
this bug.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/021fae6" target="_blank">021fae6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-021fae6');">
    Adapt previous Andrei patch to do more stuff during Yaws startup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-021fae6');">
    // Christopher Faulet
  </div>
  <div id="msg-021fae6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-021fae6');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Apr 13 21:16:47 2015</strong>

Adapt previous Andrei patch to do more stuff during Yaws startup

And update the testsuite accordingly.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/18ce696" target="_blank">18ce696</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-18ce696');">
    Merge branch 'expires-improvements' of https://github.com/andreineculau/yaws into andreineculau-expires-improvements
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-18ce696');">
    // Christopher Faulet
  </div>
  <div id="msg-18ce696" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-18ce696');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Mar 27 10:05:43 2015</strong>

Merge branch 'expires-improvements' of https://github.com/andreineculau/yaws into andreineculau-expires-improvements
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2f057c6" target="_blank">2f057c6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2f057c6');">
    update expires docs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2f057c6');">
    // Andrei Neculau
  </div>
  <div id="msg-2f057c6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2f057c6');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrei.neculau@gmail.com&gt;</strong>
<strong>Date:   Sun Mar 22 21:58:25 2015</strong>

update expires docs
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/26a608d" target="_blank">26a608d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-26a608d');">
    allow expires type/*
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-26a608d');">
    // Andrei Neculau
  </div>
  <div id="msg-26a608d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-26a608d');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrei.neculau@gmail.com&gt;</strong>
<strong>Date:   Sun Mar 22 22:46:47 2015</strong>

allow expires type/*
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/eadc686" target="_blank">eadc686</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-eadc686');">
    allow expires */*
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-eadc686');">
    // Andrei Neculau
  </div>
  <div id="msg-eadc686" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-eadc686');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrei.neculau@gmail.com&gt;</strong>
<strong>Date:   Wed Mar 18 13:52:48 2015</strong>

allow expires */*
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1075411" target="_blank">1075411</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1075411');">
    allow expires always
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1075411');">
    // Andrei Neculau
  </div>
  <div id="msg-1075411" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1075411');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrei.neculau@gmail.com&gt;</strong>
<strong>Date:   Wed Dec 10 22:56:32 2014</strong>

allow expires always
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/40c1898" target="_blank">40c1898</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-40c1898');">
    Merge pull request #209 from phonohawk/fix-crashmsg-xss
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-40c1898');">
    // Claes Wikstrom
  </div>
  <div id="msg-40c1898" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-40c1898');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sun Mar 22 17:02:01 2015</strong>

Merge pull request #209 from phonohawk/fix-crashmsg-xss

Fix possible XSS in yaws_outmod:crashmsg/3
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e17574f" target="_blank">e17574f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e17574f');">
    spelling err in man page
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e17574f');">
    // Klacke
  </div>
  <div id="msg-e17574f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e17574f');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Sat Mar 21 18:45:19 2015</strong>

spelling err in man page
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a1e0004" target="_blank">a1e0004</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a1e0004');">
    Fix possible XSS in yaws_outmod:crashmsg/3
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a1e0004');">
    // PHO
  </div>
  <div id="msg-a1e0004" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a1e0004');">
    <pre>
<strong>Author: PHO &lt;pho@cielonegro.org&gt;</strong>
<strong>Date:   Fri Mar 20 05:09:54 2015</strong>

Fix possible XSS in yaws_outmod:crashmsg/3

Stacktraces have a dump of arguments for function calls
nowadays. Without escaping HTML special characters, it is possible to
make yaws_outmod:crashmsg/3 dump strings containing something like
"&lt;/pre&gt;&lt;script&gt;alert('Hello, XSS!')&lt;/script&gt;" and thus results in a
XSS vulnerability.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1bf94af" target="_blank">1bf94af</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1bf94af');">
    Silense on tls errors
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1bf94af');">
    // Klacke
  </div>
  <div id="msg-1bf94af" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1bf94af');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Thu Mar 12 20:04:48 2015</strong>

Silense on tls errors

These days, internet facing servers get SSL attacks, we should be
silent there and just drop connection attempt silently
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/af8fa0f" target="_blank">af8fa0f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-af8fa0f');">
    Merge pull request #202 from zephyrean/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-af8fa0f');">
    // Claes Wikstrom
  </div>
  <div id="msg-af8fa0f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-af8fa0f');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Mar 10 19:27:03 2015</strong>

Merge pull request #202 from zephyrean/master

fix: external logger_mod module tried to init before ebin_dir is added to path
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8fe395d" target="_blank">8fe395d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8fe395d');">
    fix external logger_mod module tried to init before ebin_dir is added to path
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8fe395d');">
    // Mike
  </div>
  <div id="msg-8fe395d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8fe395d');">
    <pre>
<strong>Author: Mike &lt;mpustovalov@gmail.com&gt;</strong>
<strong>Date:   Fri Mar 6 14:31:23 2015</strong>

fix external logger_mod module tried to init before ebin_dir is added to path
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6465cc2" target="_blank">6465cc2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6465cc2');">
    Merge pull request #199 from minostro/show-message-when-failing-to-parse-message
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6465cc2');">
    // Steve Vinoski
  </div>
  <div id="msg-6465cc2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6465cc2');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Feb 14 04:13:33 2015</strong>

Merge pull request #199 from minostro/show-message-when-failing-to-parse-message

message is part of the error tuple when failing to parse soap response message
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b2b50f6" target="_blank">b2b50f6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b2b50f6');">
    displays message when failing to parse it
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b2b50f6');">
    // Milton Inostroza
  </div>
  <div id="msg-b2b50f6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b2b50f6');">
    <pre>
<strong>Author: Milton Inostroza &lt;milinostroza@groupon.com&gt;</strong>
<strong>Date:   Sat Feb 14 01:10:07 2015</strong>

displays message when failing to parse it
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4f014b4" target="_blank">4f014b4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4f014b4');">
    Add more documentation for arg_rewrite_mod
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4f014b4');">
    // Steve Vinoski
  </div>
  <div id="msg-4f014b4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4f014b4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Feb 13 02:46:41 2015</strong>

Add more documentation for arg_rewrite_mod

Add documentation for arg_rewrite_mod to the yaws.conf man page and
yaws.pdf, especially noting the use of the #rewrite_response{} record
for replying from a rewrite function.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bfc2bb0" target="_blank">bfc2bb0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bfc2bb0');">
    Updated authbind/privbind docs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bfc2bb0');">
    // Klacke
  </div>
  <div id="msg-bfc2bb0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bfc2bb0');">
    <pre>
<strong>Author: Klacke &lt;cwikstro@cisco.com&gt;</strong>
<strong>Date:   Sun Feb 8 23:33:54 2015</strong>

Updated authbind/privbind docs
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cefd5bc" target="_blank">cefd5bc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cefd5bc');">
    Fix SSL socket handling for dispatchmods
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cefd5bc');">
    // Steve Vinoski
  </div>
  <div id="msg-cefd5bc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cefd5bc');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Feb 3 22:31:02 2015</strong>

Fix SSL socket handling for dispatchmods

Fixed the code for invoking a dispatchmod to no longer assume the
socket is a non-SSL socket.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1887597" target="_blank">1887597</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1887597');">
    Make rebar account for 32-/64-bit builds
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1887597');">
    // Steve Vinoski
  </div>
  <div id="msg-1887597" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1887597');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Jan 31 10:26:04 2015</strong>

Make rebar account for 32-/64-bit builds

In rebar.config, use the rebar ERLANG_ARCH environment variable to
determine appropriate compiler/linker flags to match the 32- or 64-bit
build of the Erlang runtime in use.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d31e948" target="_blank">d31e948</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d31e948');">
    Add yaws as a dep in node rebar.config template
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d31e948');">
    // Steve Vinoski
  </div>
  <div id="msg-d31e948" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d31e948');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Jan 19 17:24:41 2015</strong>

Add yaws as a dep in node rebar.config template
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/80903ff" target="_blank">80903ff</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-80903ff');">
    Ignore emacs backup files in .gitignore
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-80903ff');">
    // Steve Vinoski
  </div>
  <div id="msg-80903ff" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-80903ff');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Jan 19 17:18:59 2015</strong>

Ignore emacs backup files in .gitignore
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c36e326" target="_blank">c36e326</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c36e326');">
    For rebar, make ibrowse a dep only for Yaws dev mode
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c36e326');">
    // Steve Vinoski
  </div>
  <div id="msg-c36e326" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c36e326');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Jan 18 02:22:48 2015</strong>

For rebar, make ibrowse a dep only for Yaws dev mode

For rebar builds, rather than treating ibrowse as a normal dependency even
though it's used only for testing, make rebar.config.script check for the
existence of a .rebar/YAWS_DEV_MODE file and only if present, add ibrowse
as a dependency. This allows projects that use Yaws as a rebar dependency
to avoid having to pull in ibrowse.

(Credit goes to Seth Falcon for this idea; he added something like this to
the Basho webmachine project for a test-only dependency there, and I
borrowed his idea for Yaws.)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5d755a6" target="_blank">5d755a6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5d755a6');">
    dialyzer changes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5d755a6');">
    // Steve Vinoski
  </div>
  <div id="msg-5d755a6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5d755a6');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Jan 5 00:38:02 2015</strong>

dialyzer changes

Remove no_debug_info from rebar.config to support dialyzer.

Fix known_dialyzer_warnings to compensate for latest source changes.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f54fe07" target="_blank">f54fe07</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f54fe07');">
    Renew self-signed ssl certificat
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f54fe07');">
    // Christopher Faulet
  </div>
  <div id="msg-f54fe07" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f54fe07');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Dec 18 14:35:43 2014</strong>

Renew self-signed ssl certificat
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e1d9d8e" target="_blank">e1d9d8e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e1d9d8e');">
    Add Erlang/OTP 17.3 support in .travis.yml
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e1d9d8e');">
    // Christopher Faulet
  </div>
  <div id="msg-e1d9d8e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e1d9d8e');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Dec 18 11:53:45 2014</strong>

Add Erlang/OTP 17.3 support in .travis.yml
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/06b3cb9" target="_blank">06b3cb9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-06b3cb9');">
    Add Andrei Neculau in the contributors (and sort them by name)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-06b3cb9');">
    // Christopher Faulet
  </div>
  <div id="msg-06b3cb9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-06b3cb9');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Dec 18 11:46:22 2014</strong>

Add Andrei Neculau in the contributors (and sort them by name)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9eed1d9" target="_blank">9eed1d9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9eed1d9');">
    Rework how SSL protocol_version is handled
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9eed1d9');">
    // Andrei Neculau
  </div>
  <div id="msg-9eed1d9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9eed1d9');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrei.neculau@gmail.com&gt;</strong>
<strong>Date:   Fri Dec 12 23:18:31 2014</strong>

Rework how SSL protocol_version is handled

* Don't ignore SSL protocol_version in embedded mode

* Stop setting global default SSL protocol_version

* Warning that R16B01 is required to SSL protocol_version
    After testing R14B02+ with `application:set_env(ssl, protocol_version, X)`
    and passing `versions` to `ssl:listen`, it looks like only R16B01+ follows
    the settings.

    References:
      - https://travis-ci.org/capflam/otp-ssl-test/builds/44369449
      - http://erlang.org/pipermail/erlang-questions/2014-October/081388.html
      - https://github.com/klacke/yaws/pull/192

Fix pull request #193
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ff01069" target="_blank">ff01069</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ff01069');">
    Require Erlang/OTP R14B02 or higher to compile Yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ff01069');">
    // Christopher Faulet
  </div>
  <div id="msg-ff01069" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ff01069');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Dec 16 16:06:00 2014</strong>

Require Erlang/OTP R14B02 or higher to compile Yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5f867c9" target="_blank">5f867c9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5f867c9');">
    Use default notification policy for travis
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5f867c9');">
    // Christopher Faulet
  </div>
  <div id="msg-5f867c9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5f867c9');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Sat Dec 13 09:20:14 2014</strong>

Use default notification policy for travis
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9b41c2a" target="_blank">9b41c2a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9b41c2a');">
    Fix commit adb3c360
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9b41c2a');">
    // Christopher Faulet
  </div>
  <div id="msg-9b41c2a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9b41c2a');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed Dec 10 10:28:05 2014</strong>

Fix commit adb3c360
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f4e71c2" target="_blank">f4e71c2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f4e71c2');">
    Show report.log if yaws is not started during a test
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f4e71c2');">
    // Christopher Faulet
  </div>
  <div id="msg-f4e71c2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f4e71c2');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Dec 9 17:16:24 2014</strong>

Show report.log if yaws is not started during a test
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ebcc532" target="_blank">ebcc532</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ebcc532');">
    Do not test Yaws anymore on R14B02 on Travis platform
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ebcc532');">
    // Christopher Faulet
  </div>
  <div id="msg-ebcc532" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ebcc532');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Dec 9 17:01:11 2014</strong>

Do not test Yaws anymore on R14B02 on Travis platform

It seems that we hit the bug OTP-9214 on Travis. This has worked in the
past. But now, for an unknown reason, the testsuite fails on R14B02 because of
this bug.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2c77a45" target="_blank">2c77a45</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2c77a45');">
    Do not use hostname to generate default yaws configuration
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2c77a45');">
    // Christopher Faulet
  </div>
  <div id="msg-2c77a45" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2c77a45');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Dec 9 16:49:52 2014</strong>

Do not use hostname to generate default yaws configuration

Fix issue #191
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/564a83d" target="_blank">564a83d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-564a83d');">
    Fix regular-install script by using $(logdir) variable
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-564a83d');">
    // Christopher Faulet
  </div>
  <div id="msg-564a83d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-564a83d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Dec 9 16:41:23 2014</strong>

Fix regular-install script by using $(logdir) variable
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/adb3c36" target="_blank">adb3c36</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-adb3c36');">
    Properly handle Yaws installation if a previous version already exists
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-adb3c36');">
    // Christopher Faulet
  </div>
  <div id="msg-adb3c36" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-adb3c36');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Dec 9 16:28:33 2014</strong>

Properly handle Yaws installation if a previous version already exists

From version 1.99, /usr/lib/yaws is a symlink on
/usr/lib/erlang/lib/yaws-{VSN}. In previous versions, this was a directory. So
we must remove this directory to create the symlink at the right place.

Because this directory can contain user data, we rename it instead of removing
it.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ed20aef" target="_blank">ed20aef</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ed20aef');">
    Merge pull request #189 from zxq9/master
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ed20aef');">
    // Christopher Faulet
  </div>
  <div id="msg-ed20aef" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ed20aef');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Dec 3 09:42:18 2014</strong>

Merge pull request #189 from zxq9/master

Update build instructions
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ae830a2" target="_blank">ae830a2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ae830a2');">
    Update build instructions
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ae830a2');">
    // Craig Everett
  </div>
  <div id="msg-ae830a2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ae830a2');">
    <pre>
<strong>Author: Craig Everett &lt;zxq9@zxq9.com&gt;</strong>
<strong>Date:   Wed Dec 3 02:39:41 2014</strong>

Update build instructions

The build instructions now comply with Christopher's notes
at http://sourceforge.net/p/erlyaws/mailman/message/32571909/
    </pre>
  </div>
</div>

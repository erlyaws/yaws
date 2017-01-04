<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9ec1435" target="_blank">9ec1435</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9ec1435');">
    Reworked the change-notes generator
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9ec1435');">
    // Klacke
  </div>
  <div id="msg-9ec1435" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9ec1435');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Wed Oct 7 23:49:27 2015</strong>

Reworked the change-notes generator
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e17953d" target="_blank">e17953d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e17953d');">
    Prepare for 2.0.2
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e17953d');">
    // Klacke
  </div>
  <div id="msg-e17953d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e17953d');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Wed Oct 7 14:04:27 2015</strong>

Prepare for 2.0.2
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/866ce0d" target="_blank">866ce0d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-866ce0d');">
    Use binary:compile_pattern for multipart messages
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-866ce0d');">
    // Kristoffer Koch
  </div>
  <div id="msg-866ce0d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-866ce0d');">
    <pre>
<strong>Author: Kristoffer Koch &lt;kristoffer.koch@gmail.com&gt;</strong>
<strong>Date:   Sat Oct 3 15:28:07 2015</strong>

Use binary:compile_pattern for multipart messages

Remove the (elegant) bm_start and bm_find in favor of
binary:compile_pattern and binary_match to find multipart
boundaries. My simple measurements shows that this improves
performance about 33 times for multipart boundary searching.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6505731" target="_blank">6505731</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6505731');">
    Add multipart benchmark
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6505731');">
    // Kristoffer Koch
  </div>
  <div id="msg-6505731" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6505731');">
    <pre>
<strong>Author: Kristoffer Koch &lt;kristoffer.koch@gmail.com&gt;</strong>
<strong>Date:   Sat Oct 3 12:33:32 2015</strong>

Add multipart benchmark
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/620c55a" target="_blank">620c55a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-620c55a');">
    Rewrote the news generator on yaws landing page
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-620c55a');">
    // Klacke
  </div>
  <div id="msg-620c55a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-620c55a');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Thu Oct 1 21:25:19 2015</strong>

Rewrote the news generator on yaws landing page
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/331b95d" target="_blank">331b95d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-331b95d');">
    Fix #239: return 400 for missing Host header
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-331b95d');">
    // Steve Vinoski
  </div>
  <div id="msg-331b95d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-331b95d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Sep 30 15:23:19 2015</strong>

Fix #239: return 400 for missing Host header

If the global conf variable pick_first_virthost_on_nomatch is set to
false and a request has no Host header or has multiple Host headers,
return 400 Bad Request. Add new tests for this scenario and modify the
docs where needed.

Note that pick_first_virthost_on_nomatch is true by default. The way
Yaws behaves when pick_first_virthost_on_nomatch is true is
unchanged. If you want the 400 Bad Request behavior, you must change
your configuration to set pick_first_virthost_on_nomatch to false.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6e26dc0" target="_blank">6e26dc0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6e26dc0');">
    Fix error in yaws_log_file_h introduced by the PR #241
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6e26dc0');">
    // Christopher Faulet
  </div>
  <div id="msg-6e26dc0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6e26dc0');">
    <pre>
<strong>Author: Christopher Faulet &lt;cfaulet@qualys.com&gt;</strong>
<strong>Date:   Thu Oct 1 10:36:37 2015</strong>

Fix error in yaws_log_file_h introduced by the PR #241

The pull request #241 tried to fix the commit 0d61db6. But a bug was always
here. This patch really fixes the bug and a unit test was added to be sure.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/320fcbd" target="_blank">320fcbd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-320fcbd');">
    Merge pull request #241 from weisslj/fix_yaws_log_file_h_error
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-320fcbd');">
    // Steve Vinoski
  </div>
  <div id="msg-320fcbd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-320fcbd');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 1 02:22:17 2015</strong>

Merge pull request #241 from weisslj/fix_yaws_log_file_h_error

Fix error in yaws_log_file_h (cf. 0d61db6)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6cd2801" target="_blank">6cd2801</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6cd2801');">
    Fix error in yaws_log_file_h (cf. 0d61db6)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6cd2801');">
    // Johannes Weißl
  </div>
  <div id="msg-6cd2801" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6cd2801');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Thu Oct 1 01:32:41 2015</strong>

Fix error in yaws_log_file_h (cf. 0d61db6)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5fa0af7" target="_blank">5fa0af7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5fa0af7');">
    JIT compile error in yapp docs. yaws file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5fa0af7');">
    // Klacke
  </div>
  <div id="msg-5fa0af7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5fa0af7');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Wed Sep 30 20:34:12 2015</strong>

JIT compile error in yapp docs. yaws file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d88933e" target="_blank">d88933e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d88933e');">
    Add Erlang/OTP 18.1 support in .travis.yml
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d88933e');">
    // Christopher Faulet
  </div>
  <div id="msg-d88933e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d88933e');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Sep 28 22:11:16 2015</strong>

Add Erlang/OTP 18.1 support in .travis.yml
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0d61db6" target="_blank">0d61db6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0d61db6');">
    Fix yaws_log_file_h module for Erlang release 18.1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0d61db6');">
    // Christopher Faulet
  </div>
  <div id="msg-0d61db6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0d61db6');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Sep 28 21:58:09 2015</strong>

Fix yaws_log_file_h module for Erlang release 18.1

In 18.1, the internal state of the error_logger_file_h module has
changed. Because yaws_log_file_h module is a wrapper around it, we must handle
all versions, pre-18.1 and post-18.1.

Fix #240
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/76cb149" target="_blank">76cb149</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-76cb149');">
    Fix #238: use text/xml as SOAP MIME type
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-76cb149');">
    // Steve Vinoski
  </div>
  <div id="msg-76cb149" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-76cb149');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Sep 7 17:09:15 2015</strong>

Fix #238: use text/xml as SOAP MIME type
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/01c5f89" target="_blank">01c5f89</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-01c5f89');">
    Fix link of the travis status image in README.md
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-01c5f89');">
    // Christopher Faulet
  </div>
  <div id="msg-01c5f89" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-01c5f89');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Aug 27 20:11:09 2015</strong>

Fix link of the travis status image in README.md

[ci skip]
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ab417eb" target="_blank">ab417eb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ab417eb');">
    Include URL to travis
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ab417eb');">
    // Klacke
  </div>
  <div id="msg-ab417eb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ab417eb');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Thu Aug 27 12:05:12 2015</strong>

Include URL to travis
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cc66371" target="_blank">cc66371</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cc66371');">
    Use container-based infrastructure of travis-ci
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cc66371');">
    // Christopher Faulet
  </div>
  <div id="msg-cc66371" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cc66371');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Aug 26 21:29:44 2015</strong>

Use container-based infrastructure of travis-ci
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4ddc61f" target="_blank">4ddc61f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4ddc61f');">
    Add Erlang/OTP 17.4, 17.5 and 18.0 support in .travis.yml
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4ddc61f');">
    // Christopher Faulet
  </div>
  <div id="msg-4ddc61f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4ddc61f');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Aug 26 21:29:12 2015</strong>

Add Erlang/OTP 17.4, 17.5 and 18.0 support in .travis.yml
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/343c79d" target="_blank">343c79d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-343c79d');">
    Fix some small bugs in Makefiles
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-343c79d');">
    // Christopher Faulet
  </div>
  <div id="msg-343c79d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-343c79d');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Aug 26 21:28:32 2015</strong>

Fix some small bugs in Makefiles

* src/Makefile.am: yaws.erl is used to generate mime_types.erl so it must be
  compiled first.

* test/t12/Makefile.am: Add missing file in EXTRA_DIST
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/caf47d8" target="_blank">caf47d8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-caf47d8');">
    Retrieve debug env variable before creating the global config record
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-caf47d8');">
    // Christopher Faulet
  </div>
  <div id="msg-caf47d8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-caf47d8');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Aug 26 21:27:27 2015</strong>

Retrieve debug env variable before creating the global config record

This is required to set cache_refresh_secs to 0 when debug is set to true and
when Yaws is started in embedded mode. Fixes #221.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1530c4a" target="_blank">1530c4a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1530c4a');">
    Add new test t13 for max_connections fixes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1530c4a');">
    // Steve Vinoski
  </div>
  <div id="msg-1530c4a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1530c4a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Aug 26 16:26:01 2015</strong>

Add new test t13 for max_connections fixes

Added test/t13 rather than fitting the test into an existing test
directory because max_connections is a global setting in yaws.conf,
and so it would affect all other tests in the same directory.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ce6c608" target="_blank">ce6c608</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ce6c608');">
    Catch a einval badmatch triggered by yaws:setopts/3
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ce6c608');">
    // Andrei Neculau
  </div>
  <div id="msg-ce6c608" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ce6c608');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrein@tail-f.com&gt;</strong>
<strong>Date:   Thu May 21 13:50:24 2015</strong>

Catch a einval badmatch triggered by yaws:setopts/3

inet/ssl:setopts would return {error, einval} when the socket is closed.
This can be noticed by setting max_connections to a low-enough level.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d2ad152" target="_blank">d2ad152</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d2ad152');">
    Always decrement the connection counter when a request crashes. Fixes #218.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d2ad152');">
    // Andrei Neculau
  </div>
  <div id="msg-d2ad152" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d2ad152');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrein@tail-f.com&gt;</strong>
<strong>Date:   Thu May 21 13:49:53 2015</strong>

Always decrement the connection counter when a request crashes. Fixes #218.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fe75020" target="_blank">fe75020</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fe75020');">
    Merge pull request #236 from phonohawk/issue-232
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fe75020');">
    // Christopher Faulet
  </div>
  <div id="msg-fe75020" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fe75020');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Aug 24 22:09:50 2015</strong>

Merge pull request #236 from phonohawk/issue-232

Revert "Fix HTTP authentication when user-defined module is used"
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0834b57" target="_blank">0834b57</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0834b57');">
    Documentation changes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0834b57');">
    // Steve Vinoski
  </div>
  <div id="msg-0834b57" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0834b57');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Aug 24 20:34:21 2015</strong>

Documentation changes

Remove obsolete note about future work in yapp intro. Update minimum
Erlang/OTP release from R8 to R14B02 and do some minor rewording of a
warning in yaws.tex.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/786a1b9" target="_blank">786a1b9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-786a1b9');">
    use only size and mtime for generating etags
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-786a1b9');">
    // Andrei Neculau
  </div>
  <div id="msg-786a1b9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-786a1b9');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrei.neculau@gmail.com&gt;</strong>
<strong>Date:   Tue Mar 17 08:54:04 2015</strong>

use only size and mtime for generating etags
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6e029f4" target="_blank">6e029f4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6e029f4');">
    Add new known warnings to known_dialyzer_warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6e029f4');">
    // Steve Vinoski
  </div>
  <div id="msg-6e029f4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6e029f4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Aug 24 03:21:04 2015</strong>

Add new known warnings to known_dialyzer_warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3cbffcf" target="_blank">3cbffcf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3cbffcf');">
    Support the SSL server option client_renegotiation
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3cbffcf');">
    // Leo Liu
  </div>
  <div id="msg-3cbffcf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3cbffcf');">
    <pre>
<strong>Author: Leo Liu &lt;sdl.web@gmail.com&gt;</strong>
<strong>Date:   Fri Jul 3 03:56:04 2015</strong>

Support the SSL server option client_renegotiation

This option is new with Erlang/OTP 18.0, so it's ignored for older
versions.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/510d8af" target="_blank">510d8af</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-510d8af');">
    Fix intermittent websocket test failure
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-510d8af');">
    // Steve Vinoski
  </div>
  <div id="msg-510d8af" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-510d8af');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Aug 24 00:23:04 2015</strong>

Fix intermittent websocket test failure

The "too big message" test was failing intermittently due to what
looks like a race condition on which end was closing the socket
first because of the illegal message size.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4046f0d" target="_blank">4046f0d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4046f0d');">
    Fix yaws_vdir.erl include directive
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4046f0d');">
    // Steve Vinoski
  </div>
  <div id="msg-4046f0d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4046f0d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Aug 24 00:20:19 2015</strong>

Fix yaws_vdir.erl include directive

Recent versions of rebar were correctly unhappy with a malformed
include_lib in yaws_vdir.erl. Looks like this file was missed in a
previous edit to fix this problem. Changed to a regular include
instead, matching other source files.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6713aa8" target="_blank">6713aa8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6713aa8');">
    Add yaws_api:websocket_close/1,2
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6713aa8');">
    // Yury Gargay
  </div>
  <div id="msg-6713aa8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6713aa8');">
    <pre>
<strong>Author: Yury Gargay &lt;yury.gargay@gmail.com&gt;</strong>
<strong>Date:   Wed Apr 8 13:34:54 2015</strong>

Add yaws_api:websocket_close/1,2

Add API for closing a websocket. Add underlying implementation
yaws_websockets:close/2.

Also fixed some minor typos and indentation problems in various
websockets-related example files and docs.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fea78ab" target="_blank">fea78ab</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fea78ab');">
    add check for have_sendfile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fea78ab');">
    // Andrei Neculau
  </div>
  <div id="msg-fea78ab" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fea78ab');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrei.neculau@gmail.com&gt;</strong>
<strong>Date:   Wed Mar 18 13:24:29 2015</strong>

add check for have_sendfile
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5fdbf40" target="_blank">5fdbf40</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5fdbf40');">
    added cast to prevent solaris warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5fdbf40');">
    // Andrei Neculau
  </div>
  <div id="msg-5fdbf40" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5fdbf40');">
    <pre>
<strong>Author: Andrei Neculau &lt;andrei.neculau@gmail.com&gt;</strong>
<strong>Date:   Tue Mar 17 08:58:12 2015</strong>

added cast to prevent solaris warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/19de530" target="_blank">19de530</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-19de530');">
    fix typos in yaws.tex
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-19de530');">
    // Steve Vinoski
  </div>
  <div id="msg-19de530" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-19de530');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Aug 22 15:26:20 2015</strong>

fix typos in yaws.tex
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e45922b" target="_blank">e45922b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e45922b');">
    Fix typos
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e45922b');">
    // Attila Gulyas
  </div>
  <div id="msg-e45922b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e45922b');">
    <pre>
<strong>Author: Attila Gulyas &lt;toraritte@gmail.com&gt;</strong>
<strong>Date:   Sat Aug 15 01:13:40 2015</strong>

Fix typos

Fix typos in in man/yaws.conf.5, and fix 2 news headers on index page
(www/news) by removing 3 empty lines.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/21ab0a1" target="_blank">21ab0a1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-21ab0a1');">
    Revert "Fix HTTP authentication when user-defined module is used"
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-21ab0a1');">
    // PHO
  </div>
  <div id="msg-21ab0a1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-21ab0a1');">
    <pre>
<strong>Author: PHO &lt;pho@cielonegro.org&gt;</strong>
<strong>Date:   Fri Aug 21 09:13:31 2015</strong>

Revert "Fix HTTP authentication when user-defined module is used"

This reverts commit 6cfcabd46a1db887e442130ba271d9831e4a5cd2.

After the said commit being introduced, user-defined authmod modules stopped working due to record type mismatch {badrecord,auth}. I edited the test t3 so that we can prevent this sort of error. This fixes #232.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a596302" target="_blank">a596302</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a596302');">
    Merge pull request #234 from toraritte/yman-fix
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a596302');">
    // Claes Wikstrom
  </div>
  <div id="msg-a596302" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a596302');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Aug 17 12:22:34 2015</strong>

Merge pull request #234 from toraritte/yman-fix

UTF-8 fix for www/yman.yaws and served man pages
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4fc604d" target="_blank">4fc604d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4fc604d');">
    UTF-8 fix for www/yman.yaws and served man pages
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4fc604d');">
    // Attila Gulyas
  </div>
  <div id="msg-4fc604d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4fc604d');">
    <pre>
<strong>Author: Attila Gulyas &lt;toraritte@gmail.com&gt;</strong>
<strong>Date:   Sun Aug 16 22:30:59 2015</strong>

UTF-8 fix for www/yman.yaws and served man pages

Yaws built with autotools or rebar would crash while trying to
serve man pages if os:cmd/1 returned utf-8 encoded strings.

- www/yman.yaws
Using unicode:characters_to_binary/1 for correct encoding
Issue https://github.com/klacke/yaws/issues/212 solves
this in a similar way.

- www/TAB.inc
Changed charset to utf-8 in &lt;head&gt; to handle splitting
long words in line boundary ("â€" =&gt; "-").

As far as I know unicode:characters_to_binary(Data) wouldn't
be a problem even if the input is in latin1 because according
to the doc:
Data = latin1_chardata() | chardata() | external_chardata()

ISO-8859-1 is also compatible with UTF-8 therefore changing
the charset shouldn't be the problem either.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b46a6e1" target="_blank">b46a6e1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b46a6e1');">
    Merge pull request #233 from toraritte/patch-1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b46a6e1');">
    // Steve Vinoski
  </div>
  <div id="msg-b46a6e1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b46a6e1');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Aug 14 12:22:07 2015</strong>

Merge pull request #233 from toraritte/patch-1

Fix typos in www/appmod.yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0955833" target="_blank">0955833</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0955833');">
    Fix typos in www/appmod.yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0955833');">
    // Attila Gulyas
  </div>
  <div id="msg-0955833" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0955833');">
    <pre>
<strong>Author: Attila Gulyas &lt;toraritte@gmail.com&gt;</strong>
<strong>Date:   Fri Aug 14 08:08:27 2015</strong>

Fix typos in www/appmod.yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8d4e2ab" target="_blank">8d4e2ab</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8d4e2ab');">
    Add support for binary and unicode FCGI params
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8d4e2ab');">
    // Ev Bogdanov
  </div>
  <div id="msg-8d4e2ab" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8d4e2ab');">
    <pre>
<strong>Author: Ev Bogdanov &lt;evbogdanov@yahoo.com&gt;</strong>
<strong>Date:   Sun Aug 2 20:01:35 2015</strong>

Add support for binary and unicode FCGI params

Allow names and values in FCGI extra_env parameter lists to be either
lists or binaries, and handle them properly as unicode as well. Fix
the documentation for extra_env in yaws_api.5. Change the comment
describing extra_env in yaws_api.erl to note that binaries are allowed
too.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/35e3fea" target="_blank">35e3fea</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-35e3fea');">
    New ssl option for yaws.conf: dhfile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-35e3fea');">
    // PHO
  </div>
  <div id="msg-35e3fea" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-35e3fea');">
    <pre>
<strong>Author: PHO &lt;pho@cielonegro.org&gt;</strong>
<strong>Date:   Mon Aug 3 08:00:59 2015</strong>

New ssl option for yaws.conf: dhfile

A fixed 1024-bit Diffie-Hellman group can no longer be considered to
be safe (*1). Add the dhfile option (*2) to yaws.conf.

*1: https://weakdh.org/sysadmin.html
*2: http://www.erlang.org/doc/man/ssl.html
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b08bdd5" target="_blank">b08bdd5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b08bdd5');">
    crashmsg too silent
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b08bdd5');">
    // Klacke
  </div>
  <div id="msg-b08bdd5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b08bdd5');">
    <pre>
<strong>Author: Klacke &lt;klacke@tail-f.com&gt;</strong>
<strong>Date:   Fri Jul 17 12:48:11 2015</strong>

crashmsg too silent

We should always write crash messages to the error log.
    </pre>
  </div>
</div>

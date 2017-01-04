<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/38a644c" target="_blank">38a644c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-38a644c');">
    Minor release tweak
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-38a644c');">
    // Claes Wikstrom
  </div>
  <div id="msg-38a644c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-38a644c');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sun Nov 23 14:37:45 2014</strong>

Minor release tweak
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/197307e" target="_blank">197307e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-197307e');">
    Add yapp appup and c_src *.d files to .gitignore
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-197307e');">
    // Steve Vinoski
  </div>
  <div id="msg-197307e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-197307e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Nov 15 04:42:18 2014</strong>

Add yapp appup and c_src *.d files to .gitignore
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a21a69d" target="_blank">a21a69d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a21a69d');">
    Add documentation and templates for rebar releases
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a21a69d');">
    // Steve Vinoski
  </div>
  <div id="msg-a21a69d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a21a69d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Nov 15 04:38:05 2014</strong>

Add documentation and templates for rebar releases

Add the rebar-templates directory and the files it contains to provide
rebar templates for creating Erlang releases that use Yaws. Add
www/rebar_release.yaws to provide documentation for using the templates and
creating releases.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5ddbba2" target="_blank">5ddbba2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5ddbba2');">
    Set file encoding for wiki application
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5ddbba2');">
    // Sebastian Godelet
  </div>
  <div id="msg-5ddbba2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5ddbba2');">
    <pre>
<strong>Author: Sebastian Godelet &lt;sebastian.godelet+github@gmail.com&gt;</strong>
<strong>Date:   Tue Nov 11 13:02:33 2014</strong>

Set file encoding for wiki application

Added encoding directive to correctly compile when configured with utf-8,
file applications/wiki/src/wiki.erl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/10dd428" target="_blank">10dd428</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-10dd428');">
    Prepare for yaws 1.99
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-10dd428');">
    // Claes Wikstrom
  </div>
  <div id="msg-10dd428" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-10dd428');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Nov 11 17:27:13 2014</strong>

Prepare for yaws 1.99
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1396986" target="_blank">1396986</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1396986');">
    Fix FCGI worker when an error occures during the response streaming
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1396986');">
    // Christopher Faulet
  </div>
  <div id="msg-1396986" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1396986');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Nov 6 10:39:42 2014</strong>

Fix FCGI worker when an error occures during the response streaming

When a FCGI server is called, the response can be streamed by the fcgi
worker. Because yaws_cgi:fcgi_get_output/1 exits on error, if the exception is
not catched, the client connection will wait for a quite long timeout to be
closed.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e080033" target="_blank">e080033</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e080033');">
    Add SSL protocol_version configuration element
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e080033');">
    // Steve Vinoski
  </div>
  <div id="msg-e080033" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e080033');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 16 16:43:17 2014</strong>

Add SSL protocol_version configuration element

Add protocol_version configuration element to allow supported protocols for
SSL servers to be specified.

Also add a new unit test for protocol_version.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/09e26e0" target="_blank">09e26e0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-09e26e0');">
    Remove leftover io:format calls from ciphers config
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-09e26e0');">
    // Steve Vinoski
  </div>
  <div id="msg-09e26e0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-09e26e0');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 16 16:45:07 2014</strong>

Remove leftover io:format calls from ciphers config
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e6e0ded" target="_blank">e6e0ded</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e6e0ded');">
    Fix #187: handle EINPROGRESS and EALREADY in sendfile driver
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e6e0ded');">
    // Steve Vinoski
  </div>
  <div id="msg-e6e0ded" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e6e0ded');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Sep 25 23:22:42 2014</strong>

Fix #187: handle EINPROGRESS and EALREADY in sendfile driver

Thanks to Maas-Maarten Zeeman for reporting this.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/693b043" target="_blank">693b043</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-693b043');">
    Update known dialyzer warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-693b043');">
    // Christopher Faulet
  </div>
  <div id="msg-693b043" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-693b043');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Jul 30 11:10:47 2014</strong>

Update known dialyzer warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9dec10f" target="_blank">9dec10f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9dec10f');">
    Fix yaws_debug module to get debug dump on recent Erlang/OTP releases
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9dec10f');">
    // Christopher Faulet
  </div>
  <div id="msg-9dec10f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9dec10f');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Jul 30 11:04:23 2014</strong>

Fix yaws_debug module to get debug dump on recent Erlang/OTP releases
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4bc84e3" target="_blank">4bc84e3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4bc84e3');">
    Add --configtest option in Yaws script to check a configuration file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4bc84e3');">
    // Christopher Faulet
  </div>
  <div id="msg-4bc84e3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4bc84e3');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Jul 30 11:03:34 2014</strong>

Add --configtest option in Yaws script to check a configuration file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7f2c4a1" target="_blank">7f2c4a1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7f2c4a1');">
    Support subconfig/subconfigdir directives in the server part
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7f2c4a1');">
    // Christopher Faulet
  </div>
  <div id="msg-7f2c4a1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7f2c4a1');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Jul 30 11:02:27 2014</strong>

Support subconfig/subconfigdir directives in the server part
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/53db6c7" target="_blank">53db6c7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-53db6c7');">
    Fix features detection when rebar is used
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-53db6c7');">
    // Christopher Faulet
  </div>
  <div id="msg-53db6c7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-53db6c7');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Jul 25 10:47:54 2014</strong>

Fix features detection when rebar is used
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1c065c4" target="_blank">1c065c4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1c065c4');">
    Add 'honor_cipher_order' option in ssl part
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1c065c4');">
    // Christopher Faulet
  </div>
  <div id="msg-1c065c4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1c065c4');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Jul 24 16:43:23 2014</strong>

Add 'honor_cipher_order' option in ssl part

This option can be used to set to true/false the 'honor_cipher_order' parameter
on the ssl socket. This option was introduced in SSL application in Erlang/OTP
17.0, so it will be ignored by Yaws for previous releases. When supported, this
option is set by default to true.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d7e4460" target="_blank">d7e4460</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d7e4460');">
    Merge branch 'klacke-yaws-log-fd-leak'
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d7e4460');">
    // Claes Wikstrom
  </div>
  <div id="msg-d7e4460" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d7e4460');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Sep 22 15:48:09 2014</strong>

Merge branch 'klacke-yaws-log-fd-leak'

Fd leak
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/08156ba" target="_blank">08156ba</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-08156ba');">
    Reloading the log could lead to an fd leak in the case where we were having two servers on the same port, i.e both http and https
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-08156ba');">
    // Claes Wikstrom
  </div>
  <div id="msg-08156ba" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-08156ba');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Mon Sep 22 15:46:48 2014</strong>

Reloading the log could lead to an fd leak in the
case where we were having two servers on the same port,
i.e both http and https
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a48c310" target="_blank">a48c310</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a48c310');">
    Fix yaws_sendfile to correctly handle non-latin1 filenames
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a48c310');">
    // Steve Vinoski
  </div>
  <div id="msg-a48c310" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a48c310');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Sep 15 22:38:43 2014</strong>

Fix yaws_sendfile to correctly handle non-latin1 filenames

Fix a problem where a filename containing Chinese characters would cause
yaws_sendfile to die in a list_to_binary call.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2dfbbb3" target="_blank">2dfbbb3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2dfbbb3');">
    Exclude hipe application in reltool.config
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2dfbbb3');">
    // Steve Vinoski
  </div>
  <div id="msg-2dfbbb3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2dfbbb3');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Sep 10 21:26:25 2014</strong>

Exclude hipe application in reltool.config

Several users have reported problems building releases using
rel/reltool.config due to problems with hipe, so extend reltool.config to
exclude hipe.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/769bc4b" target="_blank">769bc4b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-769bc4b');">
    Add test/eunit/subconfig_DATA to .gitignore
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-769bc4b');">
    // Steve Vinoski
  </div>
  <div id="msg-769bc4b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-769bc4b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Sep 10 21:25:53 2014</strong>

Add test/eunit/subconfig_DATA to .gitignore
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/d8fd203" target="_blank">d8fd203</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-d8fd203');">
    Document access logs as NCSA combined/XLF/ELF log format
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-d8fd203');">
    // Steve Vinoski
  </div>
  <div id="msg-d8fd203" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-d8fd203');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Sep 10 21:23:32 2014</strong>

Document access logs as NCSA combined/XLF/ELF log format
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/810cba5" target="_blank">810cba5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-810cba5');">
    Merge pull request #183 from weisslj/ehtml-attribute-fixes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-810cba5');">
    // Steve Vinoski
  </div>
  <div id="msg-810cba5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-810cba5');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Aug 27 15:04:31 2014</strong>

Merge pull request #183 from weisslj/ehtml-attribute-fixes

EHTML: Fix small bug, allow binary attribute values and update documentation
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/391cb2d" target="_blank">391cb2d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-391cb2d');">
    Merge pull request #184 from weisslj/fix-build-without-pam
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-391cb2d');">
    // Steve Vinoski
  </div>
  <div id="msg-391cb2d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-391cb2d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Aug 26 14:41:48 2014</strong>

Merge pull request #184 from weisslj/fix-build-without-pam

Fix building without PAM (and small typo in configure help)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f535e7f" target="_blank">f535e7f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f535e7f');">
    Fix typo in configure help (`--disable-pam`)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f535e7f');">
    // Johannes Weißl
  </div>
  <div id="msg-f535e7f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f535e7f');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Tue Aug 26 09:58:32 2014</strong>

Fix typo in configure help (`--disable-pam`)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c25095b" target="_blank">c25095b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c25095b');">
    Fix building without PAM
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c25095b');">
    // Johannes Weißl
  </div>
  <div id="msg-c25095b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c25095b');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Tue Aug 26 09:58:21 2014</strong>

Fix building without PAM

Fixes this error in `make`:

    /usr/bin/install: cannot stat ‘epam’: No such file or directory
    make[1]: *** [post-build] Error 1
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c8e37b5" target="_blank">c8e37b5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c8e37b5');">
    yaws_api: Cosmetics (reduce redundancy)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c8e37b5');">
    // Johannes Weißl
  </div>
  <div id="msg-c8e37b5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c8e37b5');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Sat Aug 23 15:31:19 2014</strong>

yaws_api: Cosmetics (reduce redundancy)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/49d69b2" target="_blank">49d69b2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-49d69b2');">
    yaws_api: Allow binary attribute values in ehtml
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-49d69b2');">
    // Johannes Weißl
  </div>
  <div id="msg-49d69b2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-49d69b2');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Sat Aug 23 14:49:52 2014</strong>

yaws_api: Allow binary attribute values in ehtml

Binary attribute values are already supported in exhtml.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b513bdd" target="_blank">b513bdd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b513bdd');">
    yaws_api: Fix ehtml {check, _, number()} attributes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b513bdd');">
    // Johannes Weißl
  </div>
  <div id="msg-b513bdd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b513bdd');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Sat Aug 23 14:21:50 2014</strong>

yaws_api: Fix ehtml {check, _, number()} attributes

Attribute {check, Key, 10} should expand the same way as {Key, 10}.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cbbbdbe" target="_blank">cbbbdbe</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cbbbdbe');">
    Update and synchronize ehtml documentation
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cbbbdbe');">
    // Johannes Weißl
  </div>
  <div id="msg-cbbbdbe" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cbbbdbe');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Mon Aug 25 07:23:30 2014</strong>

Update and synchronize ehtml documentation
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/89ce1d6" target="_blank">89ce1d6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-89ce1d6');">
    yaws_api: Make end tags HTML5-compatible in ehtml_expand
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-89ce1d6');">
    // Johannes Weißl
  </div>
  <div id="msg-89ce1d6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-89ce1d6');">
    <pre>
<strong>Author: Johannes Weißl &lt;jargon@molb.org&gt;</strong>
<strong>Date:   Wed Aug 20 13:42:27 2014</strong>

yaws_api: Make end tags HTML5-compatible in ehtml_expand

Currently, HTML generated by Yaws ehtml does not validate correctly as
HTML5 (e.g. using http://validator.w3.org/). The reason is that in HTML5
"void elements" like &lt;br&gt; must not have a closing end tag:

Forbidden: &lt;br&gt;&lt;/br&gt;
Allowed: &lt;br&gt;
Allowed (and also valid XHTML): &lt;br /&gt;

Also, since the self-closing tag syntax ("/&gt;") is simply ignored in
HTML5, some elements must have an end tag:

Forbidden: &lt;p /&gt;
Allowed: &lt;p&gt;&lt;/p&gt;

This patch fixes both issues and also adds tests.

References:
- http://stackoverflow.com/questions/10598501/closing-tags-in-html5
- http://stackoverflow.com/questions/1946426/html-5-is-it-br-br-or-br
- http://www.w3.org/TR/html-markup/syntax.html#void-element
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/357ad6c" target="_blank">357ad6c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-357ad6c');">
    Use iolist_to_binary/iolist_size instead of custom functions
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-357ad6c');">
    // Christopher Faulet
  </div>
  <div id="msg-357ad6c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-357ad6c');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Jul 23 14:28:13 2014</strong>

Use iolist_to_binary/iolist_size instead of custom functions

And avoid some useless conversions
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bbbf5f2" target="_blank">bbbf5f2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bbbf5f2');">
    Merge pull request #180 from yakaz/fix-contenttype-matching-in-yaws_api
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bbbf5f2');">
    // Steve Vinoski
  </div>
  <div id="msg-bbbf5f2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bbbf5f2');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Jul 23 15:06:30 2014</strong>

Merge pull request #180 from yakaz/fix-contenttype-matching-in-yaws_api

yaws_api: Fix matching of Content-Type in `parse_post/1`
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2fa5ead" target="_blank">2fa5ead</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2fa5ead');">
    yaws_api: Fix matching of Content-Type in parse_post/1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2fa5ead');">
    // Jean-Sébastien Pédron
  </div>
  <div id="msg-2fa5ead" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2fa5ead');">
    <pre>
<strong>Author: Jean-Sébastien Pédron &lt;jean-sebastien@yakaz.com&gt;</strong>
<strong>Date:   Wed Jul 23 14:56:17 2014</strong>

yaws_api: Fix matching of Content-Type in parse_post/1

"application/x-www-form-urlencoded" can be followed by attributes, such
as "; charset=UTF-8".
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b8fd96c" target="_blank">b8fd96c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b8fd96c');">
    Update known dialyzer warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b8fd96c');">
    // Christopher Faulet
  </div>
  <div id="msg-b8fd96c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b8fd96c');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Jul 21 18:07:17 2014</strong>

Update known dialyzer warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f77d932" target="_blank">f77d932</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f77d932');">
    Split content data of FCGI records to be compliant with the spec
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f77d932');">
    // Christopher Faulet
  </div>
  <div id="msg-f77d932" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f77d932');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Jul 21 17:58:26 2014</strong>

Split content data of FCGI records to be compliant with the spec

According to the FCGI specification, the size of the content data should be
lesser or equal to 65535.
See http://www.fastcgi.com/drupal/node/6?q=node/22#S3.3 for details.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a64236b" target="_blank">a64236b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a64236b');">
    Remove support of wildcards for R15B03 and previous
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a64236b');">
    // Christopher Faulet
  </div>
  <div id="msg-a64236b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a64236b');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Jul 10 16:31:39 2014</strong>

Remove support of wildcards for R15B03 and previous

A bug in filelib:wildcard/1,2 was fixed in R16B:
  https://github.com/erlang/otp/commit/70b5e24c

So, wildcard strings in the configuration file are now forbidden for older
releases.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e2a40fa" target="_blank">e2a40fa</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e2a40fa');">
    Add the Travis build status in Readme.md
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e2a40fa');">
    // Christopher Faulet
  </div>
  <div id="msg-e2a40fa" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e2a40fa');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Jul 10 15:03:53 2014</strong>

Add the Travis build status in Readme.md
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e81c94a" target="_blank">e81c94a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e81c94a');">
    Add support of wildcards and relative paths to load sub-config files.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e81c94a');">
    // Christopher Faulet
  </div>
  <div id="msg-e81c94a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e81c94a');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Jul 10 12:06:31 2014</strong>

Add support of wildcards and relative paths to load sub-config files.

Now, subconfig and subconfigdir targets accept relative paths, searched
relatively to the configuration location. So for the configuration
"/etc/yaws/yaws.conf", all relative files and directories will be searched from
"/etc/yaws". Of course, absolute paths are always supported.

furthermore, the subconfig target can take Unix-style wildcard strings to
include several files at once. See filelib:wildcard/1 for details.

Last, hidden files, starting by a dot, will be ignored in any cases.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/90e36db" target="_blank">90e36db</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-90e36db');">
    Check #deflate.min_compress_size option for large files
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-90e36db');">
    // Christopher Faulet
  </div>
  <div id="msg-90e36db" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-90e36db');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Thu Jul 10 11:57:58 2014</strong>

Check #deflate.min_compress_size option for large files

Now large files are compressed only if their size is greater than
the #deflate.min_compress_size option.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a3edbee" target="_blank">a3edbee</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a3edbee');">
    Relax rules that allow the parsing of a query string or an urlencoded body
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a3edbee');">
    // Christopher Faulet
  </div>
  <div id="msg-a3edbee" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a3edbee');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Jul 8 15:43:35 2014</strong>

Relax rules that allow the parsing of a query string or an urlencoded body

The parsing of a query string or an urlencoded body was allowed, respectively,
for 'GET' or 'POST' requests. Now, this can be done for any kind of requests. Of
course, a body is parsed only if its Content-Type is set to
'x-www-form-urlencoded'.

In the same way, a body can now be parsed as a multipart content if its
Content-Type is set to 'multipart/form-data', regardless of the request type.

As a consequence of these changes, yaws_api:getvar/2 now returns the merged
result of values found in the query string and in the urlencoded body (if any).
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7630ffa" target="_blank">7630ffa</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7630ffa');">
    Keep the original request during an internal redirection
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7630ffa');">
    // Christopher Faulet
  </div>
  <div id="msg-7630ffa" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7630ffa');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Jul 1 11:04:02 2014</strong>

Keep the original request during an internal redirection
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5b362c4" target="_blank">5b362c4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5b362c4');">
    Add systemd service script and fix other init scripts
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5b362c4');">
    // Christopher Faulet
  </div>
  <div id="msg-5b362c4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5b362c4');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jun 23 10:27:47 2014</strong>

Add systemd service script and fix other init scripts

Other changes:
 * Replace README by README.md
 * Add travis configuration
 * Fix testsuite and dependencies detection
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/5f04abe" target="_blank">5f04abe</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-5f04abe');">
    Handle a bug in unicode module in releases R14B04 and previous
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-5f04abe');">
    // Christopher Faulet
  </div>
  <div id="msg-5f04abe" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-5f04abe');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jun 23 10:26:43 2014</strong>

Handle a bug in unicode module in releases R14B04 and previous

The code points 16#FFFE and 16#FFFF (not assigned but valid) are considered as
invalid in releases R14B04 and previous. So for these releases, the websockets
testsuite always failed. This commit worked around the bug.

It was fixed in OTP with the commit 34db767655:
   https://github.com/erlang/otp/commit/34db767655
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/26badd6" target="_blank">26badd6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-26badd6');">
    Add yaws:hup/0 to allow Yaws reloads from another erlang module
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-26badd6');">
    // Christopher Faulet
  </div>
  <div id="msg-26badd6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-26badd6');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jun 23 10:23:22 2014</strong>

Add yaws:hup/0 to allow Yaws reloads from another erlang module
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6e2a971" target="_blank">6e2a971</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6e2a971');">
    Do not set recbuf by default on listening sockets
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6e2a971');">
    // Christopher Faulet
  </div>
  <div id="msg-6e2a971" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6e2a971');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jun 23 10:22:48 2014</strong>

Do not set recbuf by default on listening sockets

This patch removes a bug I experienced on FreeBSD during tests on the loopback
interface because of its MTU. So, I don't really know why that happens on
FreeBSD and not on Linux, but because loopback MTU is greater than the recbuf
value (16K on loopback vs 8k in Yaws), if a client sends data, Yaws will read
them very slowly.

Furthermore, there is no good reason to tweak this value by default. And, if
required, it is always possible to configure it.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8ff00fe" target="_blank">8ff00fe</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8ff00fe');">
    Fix a backward compatibility bug with inet:parse_strict_address/1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8ff00fe');">
    // Christopher Faulet
  </div>
  <div id="msg-8ff00fe" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8ff00fe');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Mon Jun 23 10:22:03 2014</strong>

Fix a backward compatibility bug with inet:parse_strict_address/1

inet:parse_strict_address/1 was introduced in R16. For previous releases, we
need to rely on inet_parse:ipv4strict_address/1 and
inet_parse:ipv6strict_address/1 functions.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cba96bc" target="_blank">cba96bc</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cba96bc');">
    Massive autotools refactoring & cleanup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cba96bc');">
    // Christopher Faulet
  </div>
  <div id="msg-cba96bc" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cba96bc');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Jun 10 16:44:58 2014</strong>

Massive autotools refactoring & cleanup

Now, all makefile are generated by automake. This way, many things are more
configurable and maintainable.

Main (visible) changes:

 * Now, you can configure and compile Yaws outside the source directory. It is
   also possible to generate an archive for distribution, using the 'dist' target
   (from now, 'distcheck' target should always pass).

 * We track .erl dependencies using '-M*' flags of ERLC.

 * generated header 'yaws_configure.hrl' disappears. HAVE_SENDFILE,
   HAVE_ERLANG_SENDFILE and HAVE_CRYPTO_HASH macros are defined in ERLC flags.

 * yaws_generated:localinstall/0 function disappears (see comment about local
   install).

 * To create a windows installer, we just need to run the 'mkinstaller'
   target. Read win32/README.developer for details.

 * We use standard Erlang installation path for Yaws, relative to the erlang
   library directory (/usr/lib/erlang/lib). Now, '$(libdir)/yaws' is a link. We
   do the same for yapp application.

 * "local install" was removed. Now, to do a "developer install", we just need
   to set a prefix during the project configuration using --prefix option. So,
   you need to install yaws to test it.

 * DESTDIR variable is still supported.

 * scripts/make-release was rewritten to use 'dist' and 'mkinstaller' targets.

 * Installation of Yaws applications has slightly changed. Now they are
   installed in their own directory, in '$(localstatedir)/yaws'. So, chat
   application will be installed in '$(localstatedir)/yaws/chat', with 'www' and
   'ebin' subdirs.

Main targets (others than all/install/clean....):

  * all           : compile Yaws
  * debug         : compile Yaws with debug flags
  * clean         : remove files produced by all or debug target
  * install       : do a proper install of Yaws
  * doc or docs   : build the documentation
  * check or test : launch tests
  * cleantest     : remove files produced by check target
  * dialyzer      : run dialyzer on Yaws
  * mkinstaller   : build an installer for windows
  * cleaninstaller: remove files produced by mkinstaller target
  * apps          : compile Yaws applications (chat,mail,wiki,yapp)
  * cleanapps     : remove files produced by apps target
  * installapps   : install Yaws applications
  * fullinstall   : install + installapps
  * fullclean     : clean + cleantest + cleanapps + cleaninstaller

  To install an application, run: (cd application/{APP} && make install)

Of course, many things can be customized during configuration and Rebar still
works as expected. To do an install with (almost) the same tree than with
previous of Yaws, do:

  $&gt; ./configure yawsdir=${prefix}/lib/yaws yappdir=${prefix}/lib/yapp \
        --sysconfdir=/etc --localstatedir=/var
  $&gt; make install

Here is the default installation tree on my debian:

   usr
    ├── lib
    │   └── erlang
    │       └── lib
    │           ├── yapp-0.4.2
    │           │   ├── doc/
    │           │   ├── ebin/
    │           │   ├── examples/
    │           │   └── priv/docroot/
    │           └── yaws-1.98
    │               ├── ebin/
    │               ├── include/
    │               └── priv/
    │               ├── examples/
    │
    ├── local/bin/yaws
    │
    ├── local/etc/init.d/yaws
    ├── local/etc/yaws/
    ├── local/etc/mail/yaws-webmail.conf
    │
    ├── local/lib/pkgconfig/yaws.pc
    ├── local/lib/yapp -&gt; /usr/lib/erlang/lib/yapp-0.4.2
    ├── local/lib/yaws -&gt; /usr/lib/erlang/lib/yaws-1.98
    │
    ├── local/share/doc/yaws/yaws.pdf
    ├── local/share/man/{man1,man5}/
    │
    ├── local/var/log/yaws
    ├── local/var/run/yaws
    └── local/var/yaws
        ├── chat/{ebin,www}
        ├── mail/{ebin,www}
        ├── wiki/{ebin,www}
        └── www
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c266017" target="_blank">c266017</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c266017');">
    update known dialyzer warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c266017');">
    // Steve Vinoski
  </div>
  <div id="msg-c266017" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c266017');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed May 28 00:49:55 2014</strong>

update known dialyzer warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2e86da1" target="_blank">2e86da1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2e86da1');">
    convert non-ASCII characters in json2 decoding
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2e86da1');">
    // Steve Vinoski
  </div>
  <div id="msg-2e86da1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2e86da1');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue May 20 15:34:39 2014</strong>

convert non-ASCII characters in json2 decoding

Non-ASCII characters in JSON strings are now decoded to UTF-8.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e549f86" target="_blank">e549f86</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e549f86');">
    fix broken rebar build
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e549f86');">
    // Steve Vinoski
  </div>
  <div id="msg-e549f86" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e549f86');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 29 21:52:18 2014</strong>

fix broken rebar build

In commit cb493908 I broke the ability for yaws to be used as a rebar
dependency for other projects. Move src/yaws.app.src.in back to
src/yaws.app.src. Move the code in rebar.config.script to
src/yaws.app.src.script file to process the app dependencies for
src/yaws.app.src. Change src/Makefile to process app dependencies when
creating ebin/yaws.app.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/580a0f5" target="_blank">580a0f5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-580a0f5');">
    Merge pull request #176 from richcarl/remove-vsn-attributes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-580a0f5');">
    // Steve Vinoski
  </div>
  <div id="msg-580a0f5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-580a0f5');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Apr 22 16:24:26 2014</strong>

Merge pull request #176 from richcarl/remove-vsn-attributes

remove -vsn attributes to ensure the MD5 sum is used for all modules
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/40313a4" target="_blank">40313a4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-40313a4');">
    remove -vsn attributes to ensure the MD5 sum is used for all modules
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-40313a4');">
    // Richard Carlsson
  </div>
  <div id="msg-40313a4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-40313a4');">
    <pre>
<strong>Author: Richard Carlsson &lt;richardc@klarna.com&gt;</strong>
<strong>Date:   Tue Apr 22 16:05:48 2014</strong>

remove -vsn attributes to ensure the MD5 sum is used for all modules
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1ff49f3" target="_blank">1ff49f3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1ff49f3');">
    Merge pull request #175 from yakaz/fcgi-ipv6
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1ff49f3');">
    // Steve Vinoski
  </div>
  <div id="msg-1ff49f3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1ff49f3');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 16 16:16:16 2014</strong>

Merge pull request #175 from yakaz/fcgi-ipv6

IPv6 address parsing + IPv6 lookup for FastCGI and reverse proxy
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/53fdbfe" target="_blank">53fdbfe</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-53fdbfe');">
    Add a nslookup_pref global parameter
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-53fdbfe');">
    // Jean-Sébastien Pédron
  </div>
  <div id="msg-53fdbfe" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-53fdbfe');">
    <pre>
<strong>Author: Jean-Sébastien Pédron &lt;jean-sebastien@yakaz.com&gt;</strong>
<strong>Date:   Wed Apr 9 17:31:05 2014</strong>

Add a nslookup_pref global parameter

This parameter allows to change the name resolution and connection
preference for fcgi servers and revproxy URLs. It takes a list of the
form [inet | inet6].

For instance, to perform only IPv4 resolution:
    nslookup_pref = [inet]

To perform both IPv4 and IPv6, but try IPv6 first:
    nslookup_pref = [inet6, inet]

The default value is [inet]. Therefore, the behavior remains the same
with this addition.

This parameter is used by two new functions:
    o  yaws:tcp_connect/{3,4}
    o  yaws:ssl_connect/{3,4}

They are wrappers around gen_tcp:connect/{3,4} and ssl:connect/{3,4} and
take care of connection retries for each configured family.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2e662ff" target="_blank">2e662ff</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2e662ff');">
    Accept an IPv6 between "[...]" as an fcgi server hostname
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2e662ff');">
    // Jean-Sébastien Pédron
  </div>
  <div id="msg-2e662ff" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2e662ff');">
    <pre>
<strong>Author: Jean-Sébastien Pédron &lt;jean-sebastien@yakaz.com&gt;</strong>
<strong>Date:   Wed Apr 9 17:24:26 2014</strong>

Accept an IPv6 between "[...]" as an fcgi server hostname

Here are some examples:
    php_handler = &lt;fcgi, [::1]:9000&gt;
    fcgi_app_server = [::1]:9000

The square brackets syntax is already used in URLs (http://[::1]:8080/)
and allows to mix an IPv6 address and a port number.

While here, fix a bug in revproxy URL parsing where IPv6 between
brackets were considered as a syntax error.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3bd2cbf" target="_blank">3bd2cbf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3bd2cbf');">
    Merge pull request #174 from yakaz/install-freebsd
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3bd2cbf');">
    // Steve Vinoski
  </div>
  <div id="msg-3bd2cbf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3bd2cbf');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Apr 14 16:19:52 2014</strong>

Merge pull request #174 from yakaz/install-freebsd

scripts/Install: Create ${etcdir}/rc.d on FreeBSD
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ab4708c" target="_blank">ab4708c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ab4708c');">
    scripts/Install: Create ${etcdir}/rc.d on FreeBSD
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ab4708c');">
    // Jean-Sébastien Pédron
  </div>
  <div id="msg-ab4708c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ab4708c');">
    <pre>
<strong>Author: Jean-Sébastien Pédron &lt;jean-sebastien@yakaz.com&gt;</strong>
<strong>Date:   Mon Apr 14 16:11:09 2014</strong>

scripts/Install: Create ${etcdir}/rc.d on FreeBSD

If Yaws is installed in a non-standard directory, we must create
${etcdir}/rc.d before copying the init script there.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e00ac65" target="_blank">e00ac65</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e00ac65');">
    update priv/mime.types from Apache
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e00ac65');">
    // Steve Vinoski
  </div>
  <div id="msg-e00ac65" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e00ac65');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Apr 13 19:43:43 2014</strong>

update priv/mime.types from Apache
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/02218b2" target="_blank">02218b2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-02218b2');">
    allow a cleanup pid with replace_cookie_session
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-02218b2');">
    // Steve Vinoski
  </div>
  <div id="msg-02218b2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-02218b2');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Apr 12 20:11:49 2014</strong>

allow a cleanup pid with replace_cookie_session

Add yaws_api:replace_cookie_session/3 and
yaws_session_server:replace_session/3 to allow the user to pass a cleanup
pid in order to associate a new cleanup process with the replacement
session. Thanks to Patrick Moy for suggesting this.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/cb49390" target="_blank">cb49390</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-cb49390');">
    allow extra app dependencies for yaws.app to be configured
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-cb49390');">
    // Steve Vinoski
  </div>
  <div id="msg-cb49390" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-cb49390');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Apr 3 21:37:28 2014</strong>

allow extra app dependencies for yaws.app to be configured

Yaws can depend on crypto for random numbers and hashes, and on the
compiler for .yaws pages. But the default yaws.app file specifies only
kernel and stdlib as application dependencies, since not all Yaws
deployments use crypto or compiler. Still, if a Yaws system uses crypto and
compiler without including them in the yaws.app file, building and
deploying an Erlang/OTP release for that system will fail.

Add --enable-crypto and --enable-compiler options to the configure script
to allow the user to add either or both of these applications to the
yaws.app file. Specifying the --enable-crypto option also causes the Yaws
startup code to start the crypto application; likewise for
--enable-compiler and the compiler application.

For rebar builds, enhance rebar.config.script to generate the same files
the configure script generates for any extra app dependencies. Setting the
YAWS_APPDEPS environment variable to a space- or comma-separated list of
addition applications to add to yaws.app, e.g.

YAWS_APPDEPS='crypto compiler'

causes rebar to add those apps to the generated yaws.app file and ensure
they're started before Yaws started.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/52e7b4c" target="_blank">52e7b4c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-52e7b4c');">
    dialyzer maintenance
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-52e7b4c');">
    // Steve Vinoski
  </div>
  <div id="msg-52e7b4c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-52e7b4c');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Apr 2 03:56:43 2014</strong>

dialyzer maintenance

Fix known dialyzer warnings. Also change Makefile dialyzer rule to make it
easier to see pass/fail results.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/36a3600" target="_blank">36a3600</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-36a3600');">
    remove unused -DDYNAMIC_DRIVER from c_src/Makefile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-36a3600');">
    // Steve Vinoski
  </div>
  <div id="msg-36a3600" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-36a3600');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Mar 27 15:16:56 2014</strong>

remove unused -DDYNAMIC_DRIVER from c_src/Makefile

Thanks to Richard Carlsson for pointing out this unused option.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/db48102" target="_blank">db48102</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-db48102');">
    add check for YAWS_DISABLE_PAM to rebar.config.script
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-db48102');">
    // Steve Vinoski
  </div>
  <div id="msg-db48102" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-db48102');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Mar 25 14:06:13 2014</strong>

add check for YAWS_DISABLE_PAM to rebar.config.script

Setting the OS environment variable YAWS_DISABLE_PAM to any value causes
rebar builds to avoid building pam. This is equivalent to using the
--disable-pam option with configure, and is useful for building in
environments where pam development files are not available.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/754f56e" target="_blank">754f56e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-754f56e');">
    add .rebar dir to .gitignore
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-754f56e');">
    // Steve Vinoski
  </div>
  <div id="msg-754f56e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-754f56e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Mar 25 14:05:47 2014</strong>

add .rebar dir to .gitignore
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f86bd40" target="_blank">f86bd40</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f86bd40');">
    fix #173: set ibrowse dep to v3.0.4 in rebar.config
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f86bd40');">
    // Steve Vinoski
  </div>
  <div id="msg-f86bd40" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f86bd40');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Mar 20 05:16:13 2014</strong>

fix #173: set ibrowse dep to v3.0.4 in rebar.config

Set ibrowse dependency to version 3.0.4 to match what test/Makefile
selects. Later versions do not work so well on OS X Mavericks.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/dbd71df" target="_blank">dbd71df</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-dbd71df');">
    fix #172: make yaws_ticker handle EXIT messages correctly
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-dbd71df');">
    // Steve Vinoski
  </div>
  <div id="msg-dbd71df" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-dbd71df');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Mar 15 00:00:34 2014</strong>

fix #172: make yaws_ticker handle EXIT messages correctly

The yaws_ticker processes were not cleaned up due to an incorrect EXIT
message receive spec. Thanks to Nico Kruber for reporting the problem, and
for supplying a new regression test case which is now in the test/eunit
directory.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/abce39c" target="_blank">abce39c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-abce39c');">
    add virtual server redirect conf example to redirect.yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-abce39c');">
    // Steve Vinoski
  </div>
  <div id="msg-abce39c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-abce39c');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Mar 12 18:06:16 2014</strong>

add virtual server redirect conf example to redirect.yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bdb7d0b" target="_blank">bdb7d0b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bdb7d0b');">
    change "rscheme" to "rmethod" in the docs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bdb7d0b');">
    // Steve Vinoski
  </div>
  <div id="msg-bdb7d0b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bdb7d0b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Mar 12 15:13:19 2014</strong>

change "rscheme" to "rmethod" in the docs
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3384055" target="_blank">3384055</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3384055');">
    handle filename encodings properly in dir listings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3384055');">
    // Steve Vinoski
  </div>
  <div id="msg-3384055" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3384055');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Mar 8 17:13:30 2014</strong>

handle filename encodings properly in dir listings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/615f94a" target="_blank">615f94a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-615f94a');">
    Adjust line numbers in known_dialyzer_warnings file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-615f94a');">
    // Christopher Faulet
  </div>
  <div id="msg-615f94a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-615f94a');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed Feb 26 09:05:24 2014</strong>

Adjust line numbers in known_dialyzer_warnings file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/68f1f7c" target="_blank">68f1f7c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-68f1f7c');">
    Respect virtual hosts order when the configuration is reloaded
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-68f1f7c');">
    // Christopher Faulet
  </div>
  <div id="msg-68f1f7c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-68f1f7c');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Wed Feb 26 08:45:29 2014</strong>

Respect virtual hosts order when the configuration is reloaded

During a reload, when a virtual host is added or updated, it must be at the same
position than in the configuration file. With this fix, it is possible to
reorder virtual hosts, or add new ones at any position.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/15a171f" target="_blank">15a171f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-15a171f');">
    Clean up the parsing of the ssl configuration
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-15a171f');">
    // Christopher Faulet
  </div>
  <div id="msg-15a171f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-15a171f');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Feb 25 12:13:45 2014</strong>

Clean up the parsing of the ssl configuration
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2dcb7a5" target="_blank">2dcb7a5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2dcb7a5');">
    Add 'secure_renegotiate' option in ssl part
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2dcb7a5');">
    // Christopher Faulet
  </div>
  <div id="msg-2dcb7a5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2dcb7a5');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher@yakaz.com&gt;</strong>
<strong>Date:   Tue Feb 25 11:22:49 2014</strong>

Add 'secure_renegotiate' option in ssl part

This option can be used to set to true/false the 'secure_renegotiate' parameter
on the ssl socket.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/24a6229" target="_blank">24a6229</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-24a6229');">
    dialyzer updates
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-24a6229');">
    // Steve Vinoski
  </div>
  <div id="msg-24a6229" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-24a6229');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Feb 26 00:40:29 2014</strong>

dialyzer updates

Fix a dialyzer problem in yaws.erl and update the known_dialyzer_warnings
file.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b8c1875" target="_blank">b8c1875</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b8c1875');">
    replace charset.def with generated yaws_charset.hrl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b8c1875');">
    // Steve Vinoski
  </div>
  <div id="msg-b8c1875" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b8c1875');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Feb 24 19:28:03 2014</strong>

replace charset.def with generated yaws_charset.hrl

Richard Carlsson pointed out in a private email that having configure
generate the priv/charset.def file to be read and interpreted by
src/mime_type_c.erl was overly complicated.

Modify configure script and rebar.config.script to instead generate
src/yaws_charset.hrl, and include that into src/mime_type_c.erl. Thanks to
Richard for suggesting these simplifications.

Also, enhance rebar.config.script to be able to extract the desired charset
from the YAWS_CHARSET OS environment variable if set, thus providing rebar
users a way to set the charset, which they couldn't do before.

Fix test/t2/app_test.erl to not fail on Content-Type header tests if the
header value contains a charset specification.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a311320" target="_blank">a311320</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a311320');">
    use ibrowse v3.0.4 for testing
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a311320');">
    // Steve Vinoski
  </div>
  <div id="msg-a311320" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a311320');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Feb 6 02:24:28 2014</strong>

use ibrowse v3.0.4 for testing

On OS X (and perhaps elsewhere too) there were tons of false errors in the
test output, all coming from the ibrowse client complaining about closed
connections. This made it impossible to know whether the tests were
actually working or not. Instead of using ibrowse master, pin the version
the yaws tests use to 3.0.4, which doesn't exhibit this problem.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2360e86" target="_blank">2360e86</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2360e86');">
    fix #168 by checking for pam headers and library
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2360e86');">
    // Steve Vinoski
  </div>
  <div id="msg-2360e86" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2360e86');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Feb 4 16:47:41 2014</strong>

fix #168 by checking for pam headers and library

Make configure script check for the pam_appl.h header in multiple
directories, and add the directory in which it's found to CFLAGS. Ensure
the checking takes EXTRAINCLUDE settings into account. Remove
specifications of the /usr/include/pam include path from CFLAGS in the
configure script, and also from c_src/Makefile, as it's no longer necessary
to specify that path explicitly. Add EXTRAINCLUDE to CFLAGS and CPPFLAGS
when pam is enabled (note that adding it to CPPFLAGS is needed so the new
configure checking for pam_appl.h works correctly), and remove the addition
of EXTRAINCLUDE to CFLAGS in c_src/Makefile. Add check for pam library. Fix
checking of the --disable-pam configure option.

Thanks to Antoine Catton for starting the work on these changes.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/318f5a2" target="_blank">318f5a2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-318f5a2');">
    combine multiple Accept-Encoding headers
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-318f5a2');">
    // Steve Vinoski
  </div>
  <div id="msg-318f5a2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-318f5a2');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Jan 17 23:05:29 2014</strong>

combine multiple Accept-Encoding headers

According to HTTP 1.1 section 4.2, a receiver may combine multiple
instances of any header whose value can be a comma-separated list, which
applies to the case of receiving multiple Accept-Encoding headers. HTTPbis
in section 3.2.2 keeps the same rule. Yaws was using only the first such
header and ignoring any others, but should instead be combining the
multiple Accept-Encoding headers into one before processing the combined
value. This commit fixes that and adds a new test case for it.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ba8eba4" target="_blank">ba8eba4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ba8eba4');">
    add Resources section to Yaws web pages
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ba8eba4');">
    // Steve Vinoski
  </div>
  <div id="msg-ba8eba4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ba8eba4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Jan 7 21:23:52 2014</strong>

add Resources section to Yaws web pages

Add a Resources section to the Yaws web pages, where pointers/links to
external articles, presentations etc. about Yaws can be provided.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fbe25c5" target="_blank">fbe25c5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fbe25c5');">
    fix error in code example in Yaws PDF doc
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fbe25c5');">
    // Steve Vinoski
  </div>
  <div id="msg-fbe25c5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fbe25c5');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Dec 31 21:06:19 2013</strong>

fix error in code example in Yaws PDF doc
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f17c891" target="_blank">f17c891</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f17c891');">
    add some embedded config details to embed.yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f17c891');">
    // Steve Vinoski
  </div>
  <div id="msg-f17c891" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f17c891');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Dec 23 18:35:18 2013</strong>

add some embedded config details to embed.yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/dea23d2" target="_blank">dea23d2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-dea23d2');">
    exclude ibrowse dialyzer build for testing
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-dea23d2');">
    // Steve Vinoski
  </div>
  <div id="msg-dea23d2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-dea23d2');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Dec 19 02:51:14 2013</strong>

exclude ibrowse dialyzer build for testing
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c978fe0" target="_blank">c978fe0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c978fe0');">
    Change binary conversion function in yaws_rpc in order to deal with unicode characters
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c978fe0');">
    // Lastres
  </div>
  <div id="msg-c978fe0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c978fe0');">
    <pre>
<strong>Author: Lastres &lt;ramonlastres@gmail.com&gt;</strong>
<strong>Date:   Wed Nov 20 17:57:17 2013</strong>

Change binary conversion function in yaws_rpc in order to deal with unicode characters
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/556be77" target="_blank">556be77</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-556be77');">
    minor edit of www/configuration.yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-556be77');">
    // Steve Vinoski
  </div>
  <div id="msg-556be77" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-556be77');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Dec 18 23:43:03 2013</strong>

minor edit of www/configuration.yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/f29005b" target="_blank">f29005b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-f29005b');">
    Inefficient handling of the message queue and selective receive for untimed_stream_loop_send fixed by Martin Bjorklund.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-f29005b');">
    // Claes Wikstrom
  </div>
  <div id="msg-f29005b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-f29005b');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Dec 17 10:45:41 2013</strong>

Inefficient handling of the message queue and selective receive for untimed_stream_loop_send fixed by Martin Bjorklund.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0043df3" target="_blank">0043df3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0043df3');">
    proper include statement in yaws_stats.hrl that includes relative instead on relying on flags to erlc to find the included file
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0043df3');">
    // Claes Wikstrom
  </div>
  <div id="msg-0043df3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0043df3');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Nov 19 09:33:33 2013</strong>

proper include statement in yaws_stats.hrl that includes relative instead on relying on flags to erlc to find the included file
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/73a2486" target="_blank">73a2486</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-73a2486');">
    update cookie documentation
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-73a2486');">
    // Steve Vinoski
  </div>
  <div id="msg-73a2486" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-73a2486');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Nov 10 20:56:54 2013</strong>

update cookie documentation

Documentation for cookies used the old yaws_api:setcookie functions, which
are deprecated. Change the docs to use yaws_api:set_cookie/3 instead. Also
add a little more detail to cookies.yaws regarding how
yaws_api:set_cookie/3 is used and what it returns.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c7576cd" target="_blank">c7576cd</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c7576cd');">
    replace now() with os:timestamp() in some places
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c7576cd');">
    // Steve Vinoski
  </div>
  <div id="msg-c7576cd" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c7576cd');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Nov 10 20:54:19 2013</strong>

replace now() with os:timestamp() in some places

The now() call requires global locking to ensure monotonicity, so replace
some calls to now() with calls to os:timestamp() where monotonicity isn't
required. There are probably other such calls in other modules that can be
similarly replaced.

Calculate size of data sent only when stats requires it.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ee0be8b" target="_blank">ee0be8b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ee0be8b');">
    allow embedded listen IPs to be supplied as either lists or tuples
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ee0be8b');">
    // Steve Vinoski
  </div>
  <div id="msg-ee0be8b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ee0be8b');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Nov 8 01:52:15 2013</strong>

allow embedded listen IPs to be supplied as either lists or tuples
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b3a6b24" target="_blank">b3a6b24</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b3a6b24');">
    fix broken srcdir tests
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b3a6b24');">
    // Steve Vinoski
  </div>
  <div id="msg-b3a6b24" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b3a6b24');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Nov 8 01:00:52 2013</strong>

fix broken srcdir tests

Commit a04e38a fixed building the tests with rebar, but broke the
test/t11/srcdir tests when built and run with make. This commit reworks the
approach to allow the tests to run correctly and also build correctly with
rebar.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4c4f96e" target="_blank">4c4f96e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4c4f96e');">
    generate empty priv/charset.def for rebar builds
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4c4f96e');">
    // Steve Vinoski
  </div>
  <div id="msg-4c4f96e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4c4f96e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Nov 8 00:59:39 2013</strong>

generate empty priv/charset.def for rebar builds
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3cfbfd9" target="_blank">3cfbfd9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3cfbfd9');">
    enhance "tests passed" messages to include test dir
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3cfbfd9');">
    // Steve Vinoski
  </div>
  <div id="msg-3cfbfd9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3cfbfd9');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Nov 8 00:35:48 2013</strong>

enhance "tests passed" messages to include test dir
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8f7e6de" target="_blank">8f7e6de</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8f7e6de');">
    change use of sendfile flag macros for Erlang R13
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8f7e6de');">
    // Steve Vinoski
  </div>
  <div id="msg-8f7e6de" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8f7e6de');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Nov 7 04:31:09 2013</strong>

change use of sendfile flag macros for Erlang R13

Change the use of the macros used to set sendfile configuration flags in
yaws_config.erl so the code works on older versions of Erlang/OTP,
specifically the R13 versions. Tested with R13B04.
    </pre>
  </div>
</div>

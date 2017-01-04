<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7b68b2f" target="_blank">7b68b2f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7b68b2f');">
    prepare for 1.92
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7b68b2f');">
    // Claes Wikstrom
  </div>
  <div id="msg-7b68b2f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7b68b2f');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Fri Dec 23 22:28:54 2011</strong>

prepare for 1.92
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c3d6b7c" target="_blank">c3d6b7c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c3d6b7c');">
    rebar_reltool_link: fix typo and post_generate
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c3d6b7c');">
    // Tuncer Ayaz
  </div>
  <div id="msg-c3d6b7c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c3d6b7c');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Thu Dec 22 19:51:52 2011</strong>

rebar_reltool_link: fix typo and post_generate
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b1234de" target="_blank">b1234de</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b1234de');">
    tweak settings in rel/files/vm.args
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b1234de');">
    // Steve Vinoski
  </div>
  <div id="msg-b1234de" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b1234de');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Dec 21 15:58:32 2011</strong>

tweak settings in rel/files/vm.args
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/b6b9d87" target="_blank">b6b9d87</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-b6b9d87');">
    Add reltool based release support
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-b6b9d87');">
    // Tuncer Ayaz
  </div>
  <div id="msg-b6b9d87" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-b6b9d87');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Mon Dec 19 17:48:07 2011</strong>

Add reltool based release support

Running "rebar generate" now creates a self-contained yaws system
under the build directory's "rel" subdirectory. The yaws script it
provides at ./rel/yaws/bin/yaws isn't the same as the normal yaws
script (the one normally found at ./bin/yaws); rather, it's a special
script that starts yaws and the Erlang applications on which it
depends as a local self-contained Erlang node. You can run

./rel/yaws/bin/yaws console

to start an interactive yaws node, or

./rel/yaws/bin/yaws start

to run it as a daemon, which you can later stop with

./rel/yaws/bin/yaws stop

This script does not accept the command-line options that ./bin/yaws
does, at least for now. If this is a problem, please raise an issue at
the Yaws github repo or on the Yaws mailing list.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1f8b315" target="_blank">1f8b315</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1f8b315');">
    make "rebar compile" install under the build dir
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1f8b315');">
    // Steve Vinoski
  </div>
  <div id="msg-1f8b315" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1f8b315');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Dec 21 05:02:54 2011</strong>

make "rebar compile" install under the build dir

A "rebar compile" used to install yaws, yaws.conf, and other files
under the user's home directory, just like "make local_install". This
approach isn't idiomatic to rebar usage, so change it to install files
locally under the build directory. Starting ./bin/yaws from the build
directory starts this local installation.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/378da72" target="_blank">378da72</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-378da72');">
    clean src/mime_type_c.beam during "make clean"
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-378da72');">
    // Steve Vinoski
  </div>
  <div id="msg-378da72" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-378da72');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Dec 20 16:25:10 2011</strong>

clean src/mime_type_c.beam during "make clean"
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0b7c11e" target="_blank">0b7c11e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0b7c11e');">
    handle long HTTP header lines in R15B
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0b7c11e');">
    // Steve Vinoski
  </div>
  <div id="msg-0b7c11e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0b7c11e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Dec 20 16:07:37 2011</strong>

handle long HTTP header lines in R15B

R15B includes a fix for receiving HTTP request and header lines longer
than Erlang's internal TCP buffer size of 1460 bytes. In the future we
might have to allow this limit to be configurable, but for now
hard-code a maximum HTTP header line size of 16384 bytes.

This will have no effect for Erlang releases older than R15B.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4cf5fa8" target="_blank">4cf5fa8</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4cf5fa8');">
    add support for HTTP PATCH (RFC 5789)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4cf5fa8');">
    // Steve Vinoski
  </div>
  <div id="msg-4cf5fa8" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4cf5fa8');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Dec 19 23:33:23 2011</strong>

add support for HTTP PATCH (RFC 5789)

Add support for the HTTP PATCH method, added to HTTP by RFC 5789. The
Erlang HTTP request line parser doesn't recognize PATCH yet, but this
change will continue to work if and when it does.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/e3f358e" target="_blank">e3f358e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-e3f358e');">
    treat src warnings as errors
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-e3f358e');">
    // Steve Vinoski
  </div>
  <div id="msg-e3f358e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-e3f358e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Dec 19 22:53:53 2011</strong>

treat src warnings as errors

Fix compilation warning in yaws_dav.erl so that no compilation errors
exist in the build, allowing us to treat warnings as errors in
rebar.config and in src/Makefile.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/62b89d9" target="_blank">62b89d9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-62b89d9');">
    remove call to tuple fun in yaws_api.erl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-62b89d9');">
    // Steve Vinoski
  </div>
  <div id="msg-62b89d9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-62b89d9');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Dec 18 20:08:23 2011</strong>

remove call to tuple fun in yaws_api.erl

R15B deprecates tuple fun calls, emitting a warning when they're
called at runtime. Replace single use of {M, F} with fun M:F/1 in
yaws_api.erl to avoid runtime warning.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/39c8d49" target="_blank">39c8d49</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-39c8d49');">
    update WebSockets implementation to support RFC 6455
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-39c8d49');">
    // JD Bothma
  </div>
  <div id="msg-39c8d49" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-39c8d49');">
    <pre>
<strong>Author: JD Bothma &lt;jbothma@gmail.com&gt;</strong>
<strong>Date:   Fri Sep 30 19:08:18 2011</strong>

update WebSockets implementation to support RFC 6455

This change allows websocket connections to be set up between browsers
and the yaws server. RFC 6455 for WebSocket connections is supported,
in addition to the hybi working group RFC drafts 10 to 17.

The quickest way to try this out is by compiling yaws as usual, then
visiting /websockets_example.yaws at the default local installation
host. This can be done using Google Chrome 14+, Firefox 7+ or any
other browser supporting WebSocket version 8 or above. Information
about getting started with WebSockets using this implementation is
given in /websockets.yaws.

This drops support for the older draft RFCs, specifically those of the
hixie working group which were previously supported by yaws but are
significantly different from the hybi working group's specification.

The interface for using WebSocket with yaws has changed
somewhat. Instead of spawning a websocket owner process which
maintains a server loop such as that shown in the old
websockets_endpoint.yaws, the application developer now implements a
callback module such as those in src/basic_echo_callback.erl or
src/advanced_echo_callback.erl -- the difference being that the
advanced callback style is only necessary if you need advanced
features of WebSocket such as fragmented messages. One suggested way
to deploy your callback module and its dependencies is as part of an
application in an OTP release, with yaws as a dependency. Rebar can be
used to build the dependencies, fetch and build yaws, and create a
release which will ensure the modules are in the path of the runtime
system.

Most behaviour tested by the Autobahn test suite 0.43 pass when
configured to connect to the /websockets_autobahn_endpoint.yaws and
/websockets_example_endpoint.yaws over an unencrypted
connection. Significantly, websocket connection closing is not
implemented and the socket is left to be cleaned up by the Runtime
System when either the connection is lost or the owning processes
dies. Secondly, certain cases where websocket frames are fragmented
within UTF-8 code points cause the check for valid text type messages
to incorrectly fail the connection.

Subprotocols are not currently supported.

Augment yaws.tex with a new WebSocket Protocol chapter (Steve
Vinoski).
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8e3ec41" target="_blank">8e3ec41</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8e3ec41');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8e3ec41');">
    // Steve Vinoski
  </div>
  <div id="msg-8e3ec41" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8e3ec41');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Dec 15 21:57:08 2011</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4e78762" target="_blank">4e78762</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4e78762');">
    make local_install put var dir under $HOME
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4e78762');">
    // Steve Vinoski
  </div>
  <div id="msg-4e78762" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4e78762');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Dec 15 21:54:58 2011</strong>

make local_install put var dir under $HOME

The "make local_install" command was using /usr/local/var by default,
which might not exist and the user might not have permissions to
create. Put var and its subdirs under $HOME with the rest of
local_install instead.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ac5e9be" target="_blank">ac5e9be</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ac5e9be');">
    Merge pull request #70 from djui/patch-1
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ac5e9be');">
    // Claes Wikstrom
  </div>
  <div id="msg-ac5e9be" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ac5e9be');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Dec 15 19:48:35 2011</strong>

Merge pull request #70 from djui/patch-1

Starting to fix a bug related to this issue #69
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0458c5d" target="_blank">0458c5d</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0458c5d');">
    change return type of sendfile set_error_buffer() (Tuncer Ayaz)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0458c5d');">
    // Steve Vinoski
  </div>
  <div id="msg-0458c5d" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0458c5d');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Dec 15 04:47:43 2011</strong>

change return type of sendfile set_error_buffer() (Tuncer Ayaz)

At the suggestion of Tuncer Ayaz, change the return type of
set_error_buffer() in the yaws sendfile driver from size_t to
ErlDrvSizeT. This makes the code make more sense since the result of
set_error_buffer() is sometimes used as the return value for driver
callback functions.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/2523f29" target="_blank">2523f29</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-2523f29');">
    Merge pull request #72 from tuncer/R15B
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-2523f29');">
    // Claes Wikstrom
  </div>
  <div id="msg-2523f29" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-2523f29');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Wed Dec 14 23:30:56 2011</strong>

Merge pull request #72 from tuncer/R15B

R15B compatibility
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9873a60" target="_blank">9873a60</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9873a60');">
    Add missing driver entries
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9873a60');">
    // Tuncer Ayaz
  </div>
  <div id="msg-9873a60" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9873a60');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Fri Dec 9 22:08:27 2011</strong>

Add missing driver entries
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0911702" target="_blank">0911702</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0911702');">
    Fix R15B compatibility (ErlDrvSizeT typedef)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0911702');">
    // Tuncer Ayaz
  </div>
  <div id="msg-0911702" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0911702');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Fri Dec 9 22:08:10 2011</strong>

Fix R15B compatibility (ErlDrvSizeT typedef)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fc85359" target="_blank">fc85359</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fc85359');">
    fix reference to undefined misc:mktemp/1 function
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fc85359');">
    // Steve Vinoski
  </div>
  <div id="msg-fc85359" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fc85359');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Nov 27 16:10:56 2011</strong>

fix reference to undefined misc:mktemp/1 function

The yaws_exhtml module was referencing a function misc:mktemp/1 from a
library not included with yaws. Add mktemp/1 to the yaws module and
call it instead.

Thanks to Richard Carlsson for reporting this.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/74845f5" target="_blank">74845f5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-74845f5');">
    Starting to fix a bug related to this issue #69
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-74845f5');">
    // Uwe Dauernheim
  </div>
  <div id="msg-74845f5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-74845f5');">
    <pre>
<strong>Author: Uwe Dauernheim &lt;uwe@dauernheim.net&gt;</strong>
<strong>Date:   Thu Nov 24 12:57:00 2011</strong>

Starting to fix a bug related to this issue #69
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/7811247" target="_blank">7811247</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-7811247');">
    whitespace cleanup
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-7811247');">
    // Steve Vinoski
  </div>
  <div id="msg-7811247" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-7811247');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Oct 31 21:02:14 2011</strong>

whitespace cleanup

Remove all trailing whitespace from all text files. Some bot sent
Klacke and me a pull request saying it had done this for us, but I
reviewed the diffs and it was affecting lines that shouldn't have been
affected, so perl and I did it ourselves instead.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1b8546f" target="_blank">1b8546f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1b8546f');">
    Merge pull request #67 from willemdj/issue_65
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1b8546f');">
    // Steve Vinoski
  </div>
  <div id="msg-1b8546f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1b8546f');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Oct 29 19:40:21 2011</strong>

Merge pull request #67 from willemdj/issue_65

Allow to pass options to erlsom (fix issue #65)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ec553ff" target="_blank">ec553ff</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ec553ff');">
    Allow to pass options to erlsom (fix issue #65)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ec553ff');">
    // Willem de Jong
  </div>
  <div id="msg-ec553ff" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ec553ff');">
    <pre>
<strong>Author: Willem de Jong &lt;w.a.de.jong@gmail.com&gt;</strong>
<strong>Date:   Sat Oct 29 14:22:04 2011</strong>

Allow to pass options to erlsom (fix issue #65)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/942e307" target="_blank">942e307</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-942e307');">
    ensure .yaws id dir is created in embedded mode (#66)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-942e307');">
    // Steve Vinoski
  </div>
  <div id="msg-942e307" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-942e307');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sun Oct 23 20:25:01 2011</strong>

ensure .yaws id dir is created in embedded mode (#66)

Fix #66 by making sure the .yaws directory corresponding to the yaws
ID is created for embedded applications using
yaws_api:embedded_start_conf.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/59a8cc1" target="_blank">59a8cc1</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-59a8cc1');">
    make sed commands for version substitution more robust
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-59a8cc1');">
    // Steve Vinoski
  </div>
  <div id="msg-59a8cc1" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-59a8cc1');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 20 15:46:13 2011</strong>

make sed commands for version substitution more robust

The sed commands in yaws.app.src and rebar-pre-script were pretty
picky regarding the presence of empty lines or other garbage in the
vsn.mk file. Change to make sed operate only on the desired version
text.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/08b88c4" target="_blank">08b88c4</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-08b88c4');">
    fix compile warnings introduced in 96dd91e0
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-08b88c4');">
    // Steve Vinoski
  </div>
  <div id="msg-08b88c4" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-08b88c4');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 20 15:39:50 2011</strong>

fix compile warnings introduced in 96dd91e0
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/21228ba" target="_blank">21228ba</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-21228ba');">
    rebar: fix loading of version string from vsn.mk
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-21228ba');">
    // Tuncer Ayaz
  </div>
  <div id="msg-21228ba" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-21228ba');">
    <pre>
<strong>Author: Tuncer Ayaz &lt;tuncer.ayaz@gmail.com&gt;</strong>
<strong>Date:   Tue Oct 11 11:39:35 2011</strong>

rebar: fix loading of version string from vsn.mk
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/bf30d1a" target="_blank">bf30d1a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-bf30d1a');">
    move some test files
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-bf30d1a');">
    // Steve Vinoski
  </div>
  <div id="msg-bf30d1a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-bf30d1a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 20 03:54:10 2011</strong>

move some test files

Relocate test files for some test/t2 tests to the new www directory
underneath that directory, instead of reaching up and across to the
top-level www directory. Change test code and config files as
necessary to match.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/96dd91e" target="_blank">96dd91e</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-96dd91e');">
    fix CGI redirect HTTP status bug
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-96dd91e');">
    // Steve Vinoski
  </div>
  <div id="msg-96dd91e" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-96dd91e');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 20 02:52:59 2011</strong>

fix CGI redirect HTTP status bug

The CGI 1.1 spec (RFC 3875) requires a server to augment a CGI
response with a 302 status code if that responses consists only of a
HTTP Location header and optional CGI extension headers, but Yaws was
not doing this. Fix this and add a unit test to verify it.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/186f07a" target="_blank">186f07a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-186f07a');">
    fix expires header time when crossing DST boundary
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-186f07a');">
    // Steve Vinoski
  </div>
  <div id="msg-186f07a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-186f07a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Thu Oct 20 02:46:14 2011</strong>

fix expires header time when crossing DST boundary

The expires header test was failing because the header time was
calculated in a way that failed to account for any daylight savings
time issues, specifically when the access time occurs when DST is
active but the expiry time occurs after DST has ended (or
vice-versa). Fixed.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0884ecb" target="_blank">0884ecb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0884ecb');">
    listen opts must be passed also to SSL sockets, Bug discovered and fixed by per Hedeland
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0884ecb');">
    // Claes Wikstrom
  </div>
  <div id="msg-0884ecb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0884ecb');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Oct 18 10:45:19 2011</strong>

listen opts must be passed also to SSL sockets, Bug discovered and fixed by per Hedeland
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/48542ca" target="_blank">48542ca</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-48542ca');">
    listen opts must be passed also to SSL sockets, Bug discovered and fixed by per Hedeland
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-48542ca');">
    // Claes Wikstrom
  </div>
  <div id="msg-48542ca" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-48542ca');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Tue Oct 18 10:44:13 2011</strong>

listen opts must be passed also to SSL sockets, Bug discovered and fixed by per Hedeland
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fd05270" target="_blank">fd05270</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fd05270');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fd05270');">
    // Claes Wikstrom
  </div>
  <div id="msg-fd05270" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fd05270');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Thu Oct 13 18:47:35 2011</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/88ee737" target="_blank">88ee737</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-88ee737');">
    don't setup dirs in embeddded mode
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-88ee737');">
    // Claes Wikstrom
  </div>
  <div id="msg-88ee737" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-88ee737');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@kisel.(none)&gt;</strong>
<strong>Date:   Thu Oct 13 18:46:55 2011</strong>

don't setup dirs in embeddded mode
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/136ff2c" target="_blank">136ff2c</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-136ff2c');">
    Populate soap_srv_mods field in yaws:setup_gconf/2
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-136ff2c');">
    // Essien Ita Essien
  </div>
  <div id="msg-136ff2c" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-136ff2c');">
    <pre>
<strong>Author: Essien Ita Essien &lt;essiene@gmail.com&gt;</strong>
<strong>Date:   Wed Oct 5 19:15:16 2011</strong>

Populate soap_srv_mods field in yaws:setup_gconf/2

When running in embedded mode, this function is used to setup the
gconf. Currently, the soap_srv_mods field is not built and is
ignored. Correct that by setting the soap_srv_mods field.

Signed-off-by: Essien Ita Essien &lt;essiene@gmail.com&gt;
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ffa8cdb" target="_blank">ffa8cdb</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ffa8cdb');">
    remove obsolete username config from yaws.tex
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ffa8cdb');">
    // Steve Vinoski
  </div>
  <div id="msg-ffa8cdb" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ffa8cdb');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Oct 1 23:23:20 2011</strong>

remove obsolete username config from yaws.tex

The "username" global config setting was removed from the code ages
ago but it was still in the LaTeX doc. Remove it.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/028f2f0" target="_blank">028f2f0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-028f2f0');">
    add config setting for acceptor pool size
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-028f2f0');">
    // Steve Vinoski
  </div>
  <div id="msg-028f2f0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-028f2f0');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Sep 27 00:00:59 2011</strong>

add config setting for acceptor pool size

Add new config setting to allow the size of the acceptor process pool
to be set to something other than the default. The default size is the
same as what it was prior to this change. Also add documentation for
the new setting, and augment the yaws.conf.template with information
about it.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/914a52f" target="_blank">914a52f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-914a52f');">
    refactor hard-to-read code in yaws_config
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-914a52f');">
    // Steve Vinoski
  </div>
  <div id="msg-914a52f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-914a52f');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Sep 26 21:00:47 2011</strong>

refactor hard-to-read code in yaws_config

The code to handle subconfig dirs suffered from way too many levels of
indentation. Refactor it to make it easier to read.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0d6b539" target="_blank">0d6b539</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0d6b539');">
    document process_options config setting
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0d6b539');">
    // Steve Vinoski
  </div>
  <div id="msg-0d6b539" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0d6b539');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Sep 26 21:00:24 2011</strong>

document process_options config setting

The process_options config setting was not documented, so add it to
yaws.tex.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c2a51e3" target="_blank">c2a51e3</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c2a51e3');">
    fix minor documentation issue for ehtml function values
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c2a51e3');">
    // Steve Vinoski
  </div>
  <div id="msg-c2a51e3" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c2a51e3');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Sep 26 05:52:05 2011</strong>

fix minor documentation issue for ehtml function values
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/26e79be" target="_blank">26e79be</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-26e79be');">
    allow functions to be specified as values in ehtml
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-26e79be');">
    // Steve Vinoski
  </div>
  <div id="msg-26e79be" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-26e79be');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Sep 26 04:52:09 2011</strong>

allow functions to be specified as values in ehtml

Ehtml now allows functions to be used as values. You can specify a
function value as an anonymous arity 0 function, or as a tuple
consisting of a module, function name, and argument list (i.e.,
{M,F,[Args]}). Functions must return valid ehtml values, and they may
return other functions as long as those functions eventually return a
valid non-function ehtml value.

Augment yaws.tex to document this new feature. Add a new eunit test
for the feature as well.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/206b4d9" target="_blank">206b4d9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-206b4d9');">
    delete chatty messages, make yaws_server upgrade-friendly (Klarna)
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-206b4d9');">
    // Steve Vinoski
  </div>
  <div id="msg-206b4d9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-206b4d9');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Sep 23 09:42:33 2011</strong>

delete chatty messages, make yaws_server upgrade-friendly (Klarna)

Incorporate changes from Klarna (via Richard Carlsson) to delete
chatty messages in a variety of places. These messages were for
success cases; they were deleted because success cases should be
quiet.

Change yaws --check to take an optional --verbose option to allow
original verbose success messages to be emitted. Also change the yaws
script so that the --id option works for --check.

Change yaws_server to make fully-qualified calls to gserv_loop to
ensure code upgrades call into the newly-loaded module version.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c61e0c0" target="_blank">c61e0c0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c61e0c0');">
    changes for OS X Lion for build and test
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c61e0c0');">
    // Steve Vinoski
  </div>
  <div id="msg-c61e0c0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c61e0c0');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Fri Sep 23 09:27:43 2011</strong>

changes for OS X Lion for build and test

The configure script had a place verifying Darwin version 10.*, which
doesn't work for Lion since it's version 11.*. Allow any version
greater than or equal to 10. Also the wgettest script under test/t1
uses grep in a way that no longer seems to work under Lion (very odd),
so change it to work around the issue.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/769fa3a" target="_blank">769fa3a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-769fa3a');">
    careful review of yaws.tex, with numerous fixes
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-769fa3a');">
    // Steve Vinoski
  </div>
  <div id="msg-769fa3a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-769fa3a');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Wed Sep 14 05:10:01 2011</strong>

careful review of yaws.tex, with numerous fixes
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a117c80" target="_blank">a117c80</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a117c80');">
    Merge branch 'master' of github.com:klacke/yaws
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a117c80');">
    // Steve Vinoski
  </div>
  <div id="msg-a117c80" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a117c80');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Tue Sep 6 20:06:33 2011</strong>

Merge branch 'master' of github.com:klacke/yaws
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1589530" target="_blank">1589530</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1589530');">
    add yapp.app to .gitignore
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1589530');">
    // Steve Vinoski
  </div>
  <div id="msg-1589530" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1589530');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Jul 18 00:38:35 2011</strong>

add yapp.app to .gitignore
    </pre>
  </div>
</div>

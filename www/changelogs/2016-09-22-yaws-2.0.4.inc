<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/01013c2" target="_blank">01013c2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-01013c2');">
    Preparing for 2.0.4
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-01013c2');">
    // Klacke Wikstrom
  </div>
  <div id="msg-01013c2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-01013c2');">
    <pre>
<strong>Author: Klacke Wikstrom &lt;cwikstro@cisco.com&gt;</strong>
<strong>Date:   Thu Sep 22 11:40:17 2016</strong>

Preparing for 2.0.4
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/4b664f6" target="_blank">4b664f6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-4b664f6');">
    Fix timing issue in the testsuite, mainly encountered during Travis builds
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-4b664f6');">
    // Christopher Faulet
  </div>
  <div id="msg-4b664f6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-4b664f6');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Sep 20 14:32:24 2016</strong>

Fix timing issue in the testsuite, mainly encountered during Travis builds
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/029eade" target="_blank">029eade</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-029eade');">
    Add support for salted hashes in users credentials
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-029eade');">
    // Christopher Faulet
  </div>
  <div id="msg-029eade" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-029eade');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Sep 13 15:16:51 2016</strong>

Add support for salted hashes in users credentials

Now, it is possible to use a random salt to hash passwords. The salt must be
provided, encoded in base64.
In yaws configuration is must follow the format:

  user = "User:{Algo}$Salt$Hash"

And in .yaws_file:

  {"User", "Algo", "Salt", "Hash"}.

Using clear passwords or hashes without salt is still possible. Yaws startup
script always uses salted hashes
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ea7940a" target="_blank">ea7940a</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ea7940a');">
    Remove support for Erlang/OTP R14 and R15 releases
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ea7940a');">
    // Christopher Faulet
  </div>
  <div id="msg-ea7940a" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ea7940a');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Sep 13 15:15:27 2016</strong>

Remove support for Erlang/OTP R14 and R15 releases

Now, R16B01 or higher is required.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/6d6b170" target="_blank">6d6b170</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-6d6b170');">
    Add support of hashed passwords in yaws configuration and in .yaws_auth files
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-6d6b170');">
    // Christopher Faulet
  </div>
  <div id="msg-6d6b170" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-6d6b170');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Tue Sep 13 13:19:49 2016</strong>

Add support of hashed passwords in yaws configuration and in .yaws_auth files

Now, it is possible to used hashed password to define user's credential. In the
yaws configuration, you can add following lines is &lt;auth&gt; sections:

  user = "User:{Algo}Base64Hash"

and in .yaws_auth files:

  {"User", "Algo", "Base64Hash"}.

Algo is one of: md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512

and Base64Hash is the result of the hash functions, encoded in base64. For
example:

  "/N4rLtula/QIYB+3If6bXDONEO5CnqBPrlURto+/j7k=" for "bar" password.

yaws startup script has been updated to add a way to generate user's credential:

  $&gt; yaws --auth --algo sha256 USER
  baz

  User's credential successfully generated:
      Put this line in your Yaws config (in &lt;auth&gt; section): user = "USER:{sha256}uqWglk0zIPvAxqkiFARTyFE+okq4/QV3A0gEqWckgJY="

      Or in a .yaws_auth file: {"USER", "sha256", "uqWglk0zIPvAxqkiFARTyFE+okq4/QV3A0gEqWckgJY="}.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/215ad88" target="_blank">215ad88</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-215ad88');">
    Remove sensitive information from crash messages in yaws_outmod
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-215ad88');">
    // Christopher Faulet
  </div>
  <div id="msg-215ad88" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-215ad88');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Sep 12 13:33:02 2016</strong>

Remove sensitive information from crash messages in yaws_outmod

This is probably a bad idea to keep yaws_outmd to print crash messages in
production. This module was developped to ease debugging. But, to avoid
problems, as far as possible, authentication information are removed, if any.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1a45542" target="_blank">1a45542</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1a45542');">
    Store the hash of passwords in #auth{} records instead of the clear version
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1a45542');">
    // Christopher Faulet
  </div>
  <div id="msg-1a45542" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1a45542');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Sep 12 13:17:10 2016</strong>

Store the hash of passwords in #auth{} records instead of the clear version

So, if passwords leak (in logs, in crash messages or whatever), it will be
significantly harder for hacker to use it. Passwords are hashed using SHA256.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0d6dbd7" target="_blank">0d6dbd7</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0d6dbd7');">
    Revert "Move username/password tuples from server state to an ETS table"
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0d6dbd7');">
    // Christopher Faulet
  </div>
  <div id="msg-0d6dbd7" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0d6dbd7');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Sep 12 10:50:49 2016</strong>

Revert "Move username/password tuples from server state to an ETS table"

This reverts commit 37f44d03a08a54d68874ce9880c2dfbcf946f81b.

There are many flaws with this patch. First, the ETS table used to store
user/password tuples is always created during the configuration parsing (or
during the loading in embedded mode). So during a configuration reload, the old
server configurations and the new one never match because the table id in
used, when Yaws configuration is reloaded, the servers are restarted, even when
there is no changes.

Then, the ETS table is owned by the process that parses the configuration (or
that loads it in embedded mode). So the table is deleted when this process
dies. This happens when the configuration is reloaded using the startup
script. This is a huge bug, because after a reload, all requests requiring a
user/password authentication will crash because the table is gone.

And finally, ETS table owned by old server configurations is never garbaged.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fd8ee5b" target="_blank">fd8ee5b</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fd8ee5b');">
    Fix a leak when a server conf is updated because of a forgotten ets table ref
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fd8ee5b');">
    // Christopher Faulet
  </div>
  <div id="msg-fd8ee5b" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fd8ee5b');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Sep 12 09:54:04 2016</strong>

Fix a leak when a server conf is updated because of a forgotten ets table ref

Each server owns an ets table to store some information about files and scripts
and to cache small static files. When the server configuration is updated, the
old #sconf record is replaced by a new one and a new ets table is created. But
the old one was never deleted. So we could have a proliferation of forgotten ets
tables leading to a memoring leak after successive updates.

So now, instead of creating a new ets table when a server configuration is
updated, we keep the old one. This avoids useless creation of ets tables and
fixes the memory leak.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1f48d28" target="_blank">1f48d28</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1f48d28');">
    Update error messages reported by yaws_compile module
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1f48d28');">
    // Christopher Faulet
  </div>
  <div id="msg-1f48d28" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1f48d28');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Sep 19 15:07:34 2016</strong>

Update error messages reported by yaws_compile module
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/8b297f2" target="_blank">8b297f2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-8b297f2');">
    Support strong/basic_validation options in yaws_compile
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-8b297f2');">
    // Christopher Faulet
  </div>
  <div id="msg-8b297f2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-8b297f2');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Sep 19 11:26:23 2016</strong>

Support strong/basic_validation options in yaws_compile

Because these options don't generate binary code, the corresponding '&lt;erl&gt;'
block is ignored. So, in the result of yaws_compile:compile/2, following spec

    {mod, Line, Script, NumChars,  Mod, Fun}

will be replaced by

    {skip, NumChars}
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/ed1820f" target="_blank">ed1820f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-ed1820f');">
    Remove temporary files generated during scripts parsing when job is done
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-ed1820f');">
    // Christopher Faulet
  </div>
  <div id="msg-ed1820f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-ed1820f');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Sep 16 16:45:13 2016</strong>

Remove temporary files generated during scripts parsing when job is done

When .yaws scripts are parsed, each "&lt;erl&gt;" block produces a temporary erlang
module (i.e a .erl file). These modules are compiled and used during page
rendering.

Instead of keeping these files til the next parsing, we remove them just after
the compilation.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/60101c2" target="_blank">60101c2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-60101c2');">
    Support user-defined options to compile .yaws scripts
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-60101c2');">
    // Christopher Faulet
  </div>
  <div id="msg-60101c2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-60101c2');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Sep 16 16:29:07 2016</strong>

Support user-defined options to compile .yaws scripts

The function yaws_compile:compile_file/2 has been exported. It can be used to
set some additional compiler options. By default, following options are used:

  * [binary,return_errors] during normal scripts compilation, when yaws is
    running

  * [binary,return_errors,return_warnings,debug_info] during a check
    (yaws --check)

So the additional options must be compatible with these ones. For example, 'P',
'S' or 'E' options must not be used. There is no check to validate provided
options.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/60ee490" target="_blank">60ee490</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-60ee490');">
    Fix small typo in testsuites
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-60ee490');">
    // Christopher Faulet
  </div>
  <div id="msg-60ee490" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-60ee490');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Sep 15 11:33:15 2016</strong>

Fix small typo in testsuites
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/3364e06" target="_blank">3364e06</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-3364e06');">
    Rewrite yaws_compile module to improve .yaws script parsing
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-3364e06');">
    // Christopher Faulet
  </div>
  <div id="msg-3364e06" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-3364e06');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Sep 14 15:53:54 2016</strong>

Rewrite yaws_compile module to improve .yaws script parsing

yaws_compile modules has been rewritten from scratch to be more flexible. Now,
it can handle following syntaxes:

    &lt;erl&gt;out(_A) -&gt; {html, "test1"}. &lt;/erl&gt;

    &lt;b&gt;&lt;erl&gt;out(_A) -&gt; {html, "test2"}. &lt;/erl&gt;&lt;/b&gt;

    &lt;b&gt;&lt;erl&gt;
      out(_A) -&gt; {html, "test3"}.
    &lt;/erl&gt;&lt;/b&gt;

In addition, it support strange tag syntaxes with spaces and newlines, like:

    &lt;erl
       module =
         "my_module"
     &gt;
       out(_A) -&gt; {html, "test"}.
    &lt;/erl
          &gt;

The sames syntaxes are supported for verbatim blocks of course.

Unknown attributes in erl tags (all except "module") are ignored instead of
crashing the Yaws process. When a module name is provided for an erl block, we
check it to be sure that it will not override an existing module (owned by
another yaws script or a "real" module, like yaws !).

About verbatim blocks, it is now possible to add HTML attributes that will be
keep after substitution. For example:

     &lt;verbatim class="code" id="listing-1"&gt;
        ...
     &lt;/verbatim&gt;

will be replaced by:

     &lt;pre class="code" id="listing-1"&gt;
        ...
     &lt;/pre&gt;

Finally, about performance, the new parser tends to be faster in many cases when
one file is compiled. But it is really faster when many scripts are compiled in
parallel. The old parser used a global lock on the node. So when a script was
parsed, all others was blocked. Now, a lock is set per script. So two different
script can be parsed in same time.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/135d5ef" target="_blank">135d5ef</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-135d5ef');">
    Better support for erts version >= 10
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-135d5ef');">
    // Nico
  </div>
  <div id="msg-135d5ef" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-135d5ef');">
    <pre>
<strong>Author: Nico &lt;nicolas@corporama.com&gt;</strong>
<strong>Date:   Tue Sep 13 16:56:00 2016</strong>

Better support for erts version &gt;= 10
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0dcf4e6" target="_blank">0dcf4e6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0dcf4e6');">
    Rewrite opening of report.log so that previous file is not overwritten when Yaws starts. This is particularly a pain when Yaws permanently restarts and that the report.log file contains the reason of the crash.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0dcf4e6');">
    // Nico
  </div>
  <div id="msg-0dcf4e6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0dcf4e6');">
    <pre>
<strong>Author: Nico &lt;nico@corpo-laptop3.corporama.com&gt;</strong>
<strong>Date:   Tue Sep 13 16:33:09 2016</strong>

Rewrite opening of report.log so that previous file is not overwritten
when Yaws starts.
This is particularly a pain when Yaws permanently restarts and that
the report.log file contains the reason of the crash.

The use of error_logger_file_h:init(File) was opening the file as [write]
This patch reverts it to [append], while trying to handle both types of
results (pre R18 and post R18)
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/dde4be0" target="_blank">dde4be0</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-dde4be0');">
    Do not build yaws anymore for R14 releases on travis
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-dde4be0');">
    // Christopher Faulet
  </div>
  <div id="msg-dde4be0" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-dde4be0');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Mon Sep 12 08:56:37 2016</strong>

Do not build yaws anymore for R14 releases on travis

There are some incompatibilities with common_test and inets. So, for now builds
on R14B03 and R14B04 are disabled.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/635a3af" target="_blank">635a3af</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-635a3af');">
    Fix the configuration reload when SNI is enabled
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-635a3af');">
    // Christopher Faulet
  </div>
  <div id="msg-635a3af" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-635a3af');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Sep 8 14:19:31 2016</strong>

Fix the configuration reload when SNI is enabled

This fixes #286.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/51911f2" target="_blank">51911f2</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-51911f2');">
    Upload build artifacts producted by Travis on external repo
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-51911f2');">
    // Christopher Faulet
  </div>
  <div id="msg-51911f2" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-51911f2');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Thu Sep 1 16:37:17 2016</strong>

Upload build artifacts producted by Travis on external repo

Because Travis logs are not suffisant to find bugs when the testsuites execution
fail, we upload build artifcats on an extern repository.

For now, this is done on github, in the repository
capflam/yaws-travis-build-artifacts. There is no versionning, we just create new
release for each travis build and add assets in its release.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fc7c0de" target="_blank">fc7c0de</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fc7c0de');">
    Refactor the testsuite to use common_test instead of a in-house framework
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fc7c0de');">
    // Christopher Faulet
  </div>
  <div id="msg-fc7c0de" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fc7c0de');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Aug 5 13:58:31 2016</strong>

Refactor the testsuite to use common_test instead of a in-house framework

The testsuite has been rewritten to use Erlang common_test framework.
Internally, eunit is used to check results. The TS can be launched with the
'make check' command. It is only available with the autotools.

testsuite/README.md describes how the testsuite works, how to launch specific
suites or testcases and how to write new tests.

ibrowse dependency has been removed. So it is possible to run the TS without
connectivity. We use a custom HTTP client to do checks. So it is easy to adapt
it when needed. The API to write new tests is in the module
testsuite/testsuite.erl.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/df1d7bf" target="_blank">df1d7bf</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-df1d7bf');">
    Rework how random ports are handled
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-df1d7bf');">
    // Christopher Faulet
  </div>
  <div id="msg-df1d7bf" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-df1d7bf');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Aug 12 16:48:38 2016</strong>

Rework how random ports are handled

It is possible to let Yaws choose the listening port of a server by leaving it
to 0. Now, this port is chosen during the configuration parsing. There are 2
reasons to do it this way:

  * First, the #sconf record has the real listening port instead of 0. This
    avoids the checks to get it. This is also convenient to have it when the
    configuration is retrieved or dumped.

  * Then, if there are 2 or more servers configured to use a random port, each
    one will have its own port. Each server will be isolated in its own group.
    So it is possible to have in same time a HTTP and a HTTPS vhost using a
    random port.

There is a drawback when the configuration is reloaded. All servers will be
restarted and will use a new listening port. But random ports are used for test
purposes so this is not really an issue.

We use IANA range for dynamic or private ports (49152 -&gt; 65535) to choose a
port. We try to find the first free port in this range. An environment variable
is used to know the last assigned random port (attached to the kernel
application, so it is possible to reserve ports manually before Yaws is
started).

This new feature is mainly used by the new testsuite.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/96888a9" target="_blank">96888a9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-96888a9');">
    Check "Transfer-Encoding" header first, before "Content-Length" header
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-96888a9');">
    // Christopher Faulet
  </div>
  <div id="msg-96888a9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-96888a9');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Aug 12 16:24:59 2016</strong>

Check "Transfer-Encoding" header first, before "Content-Length" header

Usually, when "Transfer-Encoding" header is set to "chunk", "Content-Length"
header is not set. So it is safe to check these headers in any order. In Yaws,
"Content-Length" header was checked first.
But, some clients (e.g. inets) set the "Content-Length" header to 0 when the
body is chunked. So, in this situation, the body was skipped by Yaws. And with
persistant connections, subsequent requests was rejected.

So, to be compliant with these buggy clients, we check "Transfer-Encoding"
header first to know if the body is chunked. Then, if not, we check
"Content-Length" header to know the body size.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/810bcb6" target="_blank">810bcb6</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-810bcb6');">
    Use MOD:module_info/1 instead of erlang:function_exported/3 in yaws_dynopts
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-810bcb6');">
    // Christopher Faulet
  </div>
  <div id="msg-810bcb6" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-810bcb6');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Fri Aug 12 15:52:56 2016</strong>

Use MOD:module_info/1 instead of erlang:function_exported/3 in yaws_dynopts

erlang:function_exported/3 doesn't load the requested module. So, it always
returns false if the module was not previously loaded. So, in certain
circumstances, yaws_dynopts returns the wrong value.
Instead, we use MOD:module_info/1 to retrieve the same information. So, we are
sure to load the module.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/1208b68" target="_blank">1208b68</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-1208b68');">
    Fix #283: FastCGI protocol error for empty HTTP body requests
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-1208b68');">
    // Olivier Favre
  </div>
  <div id="msg-1208b68" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-1208b68');">
    <pre>
<strong>Author: Olivier Favre &lt;olivier@wonderpush.com&gt;</strong>
<strong>Date:   Mon Aug 22 20:09:31 2016</strong>

Fix #283: FastCGI protocol error for empty HTTP body requests

In my setup I receive the following 500 error when POSTing an empty
request to any PHP script.

  CGI failure: {"recv from application server failed",closed}

My investigations showed that the FCGI_STDIN &lt;&lt;&gt;&gt; frame was sent twice
and the FastCGI server closes the connection right away, (most of the
time) before having time to anwser.

The FastCGI protocol describes the end of a stream as an empty record
for that stream. What Yaws does is it basically closes the stream
twice. Moreover, a stream is described as zero or more non-empty
records, followed by an empty record.

http://www.mit.edu/~yandros/doc/specs/fcgi-spec.html#S3.3

  "A stream record is part of a stream, i.e. a series of zero or more
  non-empty records (length != 0) of the stream type, followed by an
  empty record (length == 0) of the stream type."

http://www.nongnu.org/fastcgi/#id2854157

  "Furthermore, it will forward any POST data it receives from the
  client to the application via the STDIN stream. Once all data have
  been forwarded, an empty STDIN packet is sent to close the stream."

Though the specification is not as precise as an RFC, sending two
empty records is not in the spirit of the spec.

Add special handling for empty body requests to
yaws_cgi:fcgi_pass_through_client_data/1.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9d8fb07" target="_blank">9d8fb07</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9d8fb07');">
    Security flaw http://httpoxy.org/ fixed A security flaw with HTTP_PROXY fixed. When we now construct the cgi env variables, we just skip the Proxy header. Reported by dominic@varspool.com
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9d8fb07');">
    // Klacke Wikstrom
  </div>
  <div id="msg-9d8fb07" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9d8fb07');">
    <pre>
<strong>Author: Klacke Wikstrom &lt;cwikstro@cisco.com&gt;</strong>
<strong>Date:   Mon Jul 25 12:46:30 2016</strong>

Security flaw http://httpoxy.org/ fixed
A security flaw with HTTP_PROXY fixed. When we now construct the
cgi env variables, we just skip the Proxy header.
Reported by dominic@varspool.com
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/80813f9" target="_blank">80813f9</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-80813f9');">
    Update known dialyzer warnings
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-80813f9');">
    // Steve Vinoski
  </div>
  <div id="msg-80813f9" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-80813f9');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Jul 11 01:44:59 2016</strong>

Update known dialyzer warnings
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/274e0a5" target="_blank">274e0a5</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-274e0a5');">
    Fix typos in shopcart.erl
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-274e0a5');">
    // Steve Vinoski
  </div>
  <div id="msg-274e0a5" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-274e0a5');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Mon Jul 11 01:37:20 2016</strong>

Fix typos in shopcart.erl
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/393b01f" target="_blank">393b01f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-393b01f');">
    randome/random.
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-393b01f');">
    // Christopher Meiklejohn
  </div>
  <div id="msg-393b01f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-393b01f');">
    <pre>
<strong>Author: Christopher Meiklejohn &lt;christopher.meiklejohn@gmail.com&gt;</strong>
<strong>Date:   Fri Jul 8 15:15:08 2016</strong>

randome/random.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/c88ab72" target="_blank">c88ab72</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-c88ab72');">
    Fix #274: remove doc ref to is_default
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-c88ab72');">
    // Steve Vinoski
  </div>
  <div id="msg-c88ab72" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-c88ab72');">
    <pre>
<strong>Author: Steve Vinoski &lt;vinoski@ieee.org&gt;</strong>
<strong>Date:   Sat Jul 2 12:44:53 2016</strong>

Fix #274: remove doc ref to is_default

The is_default feature was removed a long long time ago but was still
mentioned in yaws.tex, so remove it. Replace it with a reference to
pick_first_virthost_on_nomatch.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/fe8bd36" target="_blank">fe8bd36</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-fe8bd36');">
    Merge pull request #272 from klacke/etnt/sconf_gconf_set_functions
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-fe8bd36');">
    // Claes Wikstrom
  </div>
  <div id="msg-fe8bd36" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-fe8bd36');">
    <pre>
<strong>Author: Claes Wikstrom &lt;klacke@hyber.org&gt;</strong>
<strong>Date:   Sat Jul 2 12:15:38 2016</strong>

Merge pull request #272 from klacke/etnt/sconf_gconf_set_functions

Adding set functions for the sconf and gconf records
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/0d1021f" target="_blank">0d1021f</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-0d1021f');">
    Do not decode request path in logs
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-0d1021f');">
    // Christopher Faulet
  </div>
  <div id="msg-0d1021f" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-0d1021f');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Jun 1 16:46:35 2016</strong>

Do not decode request path in logs

Log files are openned in raw mode, so UTF-8 characters (&gt;255) cannot be
written. Their bytewise representation should be used instead. But, there is no
good reason to decode paths. Some analyzing tools could be ascii oriented and
this requires an extra encoding to get the original request.

So now, no decoding is done on the request path in access and auth logs.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/9373692" target="_blank">9373692</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-9373692');">
    Fix the UTF-8 handling for directories and files in yaws_ls
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-9373692');">
    // Christopher Faulet
  </div>
  <div id="msg-9373692" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-9373692');">
    <pre>
<strong>Author: Christopher Faulet &lt;christopher.faulet@capflam.org&gt;</strong>
<strong>Date:   Wed Jun 1 16:39:06 2016</strong>

Fix the UTF-8 handling for directories and files in yaws_ls

This patch fixes the commit 8c0773f1. See
https://github.com/klacke/yaws/commit/8c0773f1 for details.
    </pre>
  </div>
</div>
<div class="git-log">
  <div class="git-commit">
    [<a href="https://github.com/klacke/yaws/commit/a371e80" target="_blank">a371e80</a>]
  </div>
  <div class="git-title-msg" onclick="toggleVisibility('#msg-a371e80');">
    Adding set functions for the sconf and gconf records
  </div>
  <div class="git-info" onclick="toggleVisibility('#msg-a371e80');">
    // Torbjorn Tornkvist
  </div>
  <div id="msg-a371e80" class="git-body-msg" style="display: none;" onclick="toggleVisibility('#msg-a371e80');">
    <pre>
<strong>Author: Torbjorn Tornkvist &lt;ttornkvi@cisco.com&gt;</strong>
<strong>Date:   Wed May 18 16:28:33 2016</strong>

Adding set functions for the sconf and gconf records
    </pre>
  </div>
</div>

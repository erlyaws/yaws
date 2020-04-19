This is Yaws, a webserver for dynamic content written in Erlang.

![Build Status](https://github.com/erlyaws/yaws/actions/workflows/main.yml/badge.svg)

Prepare build
-------------

Get and install an Erlang system (http://www.erlang.org).

**To compile Yaws, Erlang/OTP 21.3 or higher is required.**

If you've cloned the source from github and you want to build using autotools,
note there is no ./configure script in the source, so create one:

    $> autoreconf -fi

Install build dependencies. Required packages to compile Yaws are (based on
debian packages):

* build-essential
* autoconf/automake/libtool
* erlang - Required apps:
 * erlang-{kernel/stdlib/sasl/erts}
 * erlang-compiler
 * erlang-crypto
 * erlang-xmerl
* libpam0g-dev

To build the documentation (optional), you also need to install:

* texlive-latex-base
* texlive-latex-recommended
* texlive-fonts-recommended
* texlive-font-utils
* texlive-extra-utils
* ghostscript

On Ubuntu/debian this is pretty much equal to:

    $> apt-get build-dep yaws

On MacOS and OS X, be sure the necessary Xcode command-line tools and
development environment are set up correctly by running the following
command:

    xcode-select --install

Finally, to run the test suites, yaws need to install:

* git
* wget
* curl
* erlang-eunit
* erlang-inets
* erlang-mnesia
* erlang-ssl
* cadaver (optional)


Build
-----

You can build via autotools:

    $> ./configure --prefix=/usr/local

or via rebar3 on the `rebar3-support` branch:

    $> git switch rebar3-support
    $> rebar3 compile

If using rebar3, you'll get a "local installation" that you can run
via `rebar3 shell`.

If using autotools, the build will be configured by default for installation
under /usr/local. For more information about installation directories and
other supported options, see the configure help. Useful options are:

    --with-defaultcharset=CHARSET specify default charset, i.e UTF-8
    --with-extrainclude=DIR       dir to include if e.g. pam is installed in some odd place
    --with-erlang=PREFIX          prefix where Erlang is installed (optional)

Note to packagers (rpm, deb ...) All install targets support the DESTDIR
variable. Thus, if we do

    $> ./configure --prefix=/usr; make

we can subsequently do:

    $> make install DESTDIR=/foo/bar

All Yaws files will be installed under DESTDIR, but all code will assume Yaws
should be installed under /usr.

Main make targets:

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


Reproducible builds
-------------------

It is possible to build deterministically, thus enabling reproducible builds
of Yaws.

A deterministic build is enabled either by running `configure` with options
`--enable-deterministic-build` and `--with-source-date-epoch`:

    $> ./configure --enable-deterministic-build \
                   --with-source-date-epoch=$known_unix_timestamp

or by setting the environment variables `YAWS_DETERMINISTIC_BUILD` and
`SOURCE_DATE_EPOCH` before running `autoreconf` and `configure`,
respectively:

    $> export YAWS_DETERMINISTIC_BUILD=true # set to any value will enable
    %> autoreconf -fi
    $> export SOURCE_DATE_EPOCH=$known_unix_timestamp
    $> ./configure

After either of the two configurations above are done, build as usual:

    $> make all doc

The above configurations for enabling deterministic builds add the erlc flag
`+deterministic`, drop the `+debug_info` flag, generate a deterministic
`yaws_generated.beam`, and set e.g. creation date in `yaws.ps` and `yaws.pdf`
from the value of `SOURCE_DATE_EPOCH`, which is expected to be an integer
reflecting a number of seconds since the Unix epoch. (One way to get an epoch
integer value is via the command `date '+%s'` on Linux or macOS, for example.
If you're using the bash shell version 4.2 or newer, `printf '%(%s)T\n' -1`
gives an epoch integer value, and starting at version 5, the bash shell
provides the `$EPOCHSECONDS` variable.)

Note that various paths in configuration files, templates, examples etc. are
generated from the configured installation prefix config files; thus they
will vary if the installation prefix is different across builds. This can be
mitigated by using DESTDIR when installing (see the Build section above for
more details).


Test your build
---------------

With autotools, to test the build, you should install it somewhere:

    $> ./configure --prefix=$PWD/_inst && make install
    $> $PWD/_inst/bin/yaws -i

If you used rebar3 to build Yaws, you can alternatively start Yaws with

    $> rebar3 shell

Either approach will start a webserver at http://0.0.0.0:8000
Terminate through ^C, or ^G followed by q, or

    > init:stop()

NOTE: If you've used rebar3 to build the system, none of the following
directions apply.


Install and run
---------------

NOTE: following commands may require root privileges, depending of the
installation prefix.

Just run:

    $> make install

Then, to run Yaws, you should use its script:

    $> ${bindir}/yaws -i

This starts an interactive system.

With the default yaws.conf file, this will create a webserver at
http://${host} and one at https://${host}.

You can adapt Yaws configuration by editing '${sysconfig}/yaws/yaws.conf'.


Daemonize Yaws
--------------

Start it using '--daemon' and '--heart' options:

    $> ${bindir}/yaws --daemon --heart

This will start a daemon (--daemon) which will be autorestarted when/if it
crashes or hangs (--heart).
Also, for most unices, we create proper start scripts in ${sysconfdir}/init.d

Example:

    $> autoreconf -fi
    $> ./configure --sysconfdir=/etc
    $> make && make install
    $> /etc/init.d/yaws start


Security Policy
---------------

To report security vulnerabilities or other security-related issues,
please refer to the [Yaws security policy](SECURITY.md).

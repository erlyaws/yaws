dnl ------------------------------------------------------------------
dnl Autoconf initialization.
dnl ------------------------------------------------------------------
m4_define([VERSION_NUMBER], m4_esyscmd([. ./vsn.mk && printf '%s' ${YAWS_VSN}]))
AC_INIT([yaws], VERSION_NUMBER)

AC_CONFIG_SRCDIR([src/yaws_app.erl])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([ac-aux])

dnl work out who the cpu, vendor and OS are
AC_CANONICAL_SYSTEM
AC_DEFINE_UNQUOTED(CPU_VENDOR_OS, "$host")


AM_INIT_AUTOMAKE([1.11 -Wall -Wno-portability foreign])

AC_PREREQ([2.64])

ECHO=echo
COLORED_ECHO_INIT


dnl ------------------------------------------------------------------
dnl Compiler and other tools.
dnl ------------------------------------------------------------------
old_cflags="$CFLAGS"
AM_SILENT_RULES([yes])
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])

dnl AC_PREFIX_DEFAULT(/usr)
AC_PROG_CC
AC_PROG_GCC_TRADITIONAL

AC_PROG_LD
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_SED


AM_DISABLE_STATIC
AM_ENABLE_SHARED
AM_PROG_LIBTOOL

CFLAGS="-Werror -Wall -O3 $old_cflags"


dnl ------------------------------------------------------------------
dnl Handle configure options.
dnl ------------------------------------------------------------------
AC_ARG_WITH([defaultcharset],
        AC_HELP_STRING([--with-defaultcharset=CHARSET],
            [specify default charset, i.e UTF-8]),
        with_defaultcharset=${withval%/},
        with_defaultcharset="")
if test "x${with_defaultcharset}" = "x"; then
        DEFAULT_CHARSET=undefined
else
        DEFAULT_CHARSET="\"${with_defaultcharset}\""
fi
AC_SUBST(DEFAULT_CHARSET)

AC_ARG_WITH([extrainclude],
        AS_HELP_STRING([--with-extrainclude=DIR],
            [dir to include if e.g. pam is installed in some odd place]),
        with_extrainclude=${withval%/},
        with_extrainclude="")
if test "x$with_extrainclude" = "x" ; then
        EXTRAINCLUDE=
else
        EXTRAINCLUDE=-I${with_extrainclude}
fi
AC_SUBST(EXTRAINCLUDE)

AC_ARG_ENABLE([pam],
        AC_HELP_STRING([--disable-pam],
            [disable pam auth]),
        enable_pam="$enableval",
        enable_pam="yes")
AM_CONDITIONAL(WITH_EPAM, [test "x${enable_pam}" = "xyes"])

APPDEPS=
AC_ARG_ENABLE(crypto,
        AS_HELP_STRING([--enable-crypto],
                [adds crypto application to yaws.app file]))
if test "x$enable_crypto" = "xyes"; then
    APPDEPS=',crypto'
fi
AC_ARG_ENABLE(compiler,
        AS_HELP_STRING([--enable-compiler],
                [adds compiler application to yaws.app file]))
if test "x$enable_compiler" = "xyes"; then
    APPDEPS="${APPDEPS},compiler"
fi
AC_SUBST(APPDEPS)


dnl ------------------------------------------------------------------
dnl Erlang environment.
dnl ------------------------------------------------------------------
echo
COLORED_ECHO([%BCheck Erlang environment & applications%b])

tmp_cross_compiling=$cross_compiling
cross_compiling=no

dnl Available flags.
AC_ARG_WITH([erlang],
        AC_HELP_STRING([--with-erlang=PREFIX],
            [prefix where Erlang is installed (optional)]),
        with_erlang=${withval%/},
        with_erlang="")

dnl erl(1) is used to compile Erlang modules.
if test "x${with_erlang}" = "x"; then
        AC_ERLANG_PATH_ERL
        AC_ERLANG_PATH_ERLC
else
        erl_path="${with_erlang}/bin"
        AC_ERLANG_PATH_ERL(, [$erl_path$PATH_SEPARATOR$PATH])
        AC_ERLANG_PATH_ERLC(, [$erl_path$PATH_SEPARATOR$PATH])
fi

if test "x${ERL}" = "x"; then
        AC_MSG_ERROR([
Erlang not found. Fill the ERL variable with erl(1) path or provide
Erlang prefix with --with-erlang.])
fi

dnl escript(1) is used by the testsuite.
AC_ARG_VAR([ESCRIPT], [Erlang/OTP interpreter command [autodetected]])

if test "x${ESCRIPT}" = "x"; then
        if test "x${with_erlang}" = "x"; then
                AC_PATH_PROG([ESCRIPT], [escript],,)
        else
                erl_path="${with_erlang}/bin"
                AC_PATH_PROG([ESCRIPT], [escript],,
                    [$erl_path$PATH_SEPARATOR$PATH])
        fi
else
        AC_MSG_CHECKING([for escript])
        AC_MSG_RESULT([$ESCRIPT])
fi

if test "x${ESCRIPT}" = "x"; then
        AC_MSG_WARN([
escript(1) not found. Fill the ESCRIPT variable with escript(1) path if
you want to use the testsuite.])
fi


dnl dialyzer(1).
AC_ARG_VAR([DIALYZER], [Erlang/OTP discrepancy analyzer [autodetected]])

if test "x${DIALYZER}" = "x"; then
        if test "x${with_erlang}" = "x"; then
                AC_PATH_PROG([DIALYZER], [dialyzer],,)
        else
                erl_path="${with_erlang}/bin"
                AC_PATH_PROG([DIALYZER], [dialyzer],,
                 [$erl_path$PATH_SEPARATOR$PATH])
        fi
else
        AC_MSG_CHECKING([for dialyzer])
        AC_MSG_RESULT([$DIALYZER])
fi

if test "x${DIALYZER}" = "x"; then
        AC_MSG_WARN([
dialyzer(1) not found. Fill the DIALYZER variable with dialyzer(1) path if
you want to use it.])
fi

dnl Declare ERL_LIBS as precious.
AC_ARG_VAR([ERL_LIBS], [Erlang/OTP applications search path [none]])

dnl Get Erlang $ROOT dir and lib dir.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR

dnl Get ERTS version.
ERLANG_CHECK_ERTS
ERLANG_CHECK_RELEASE

dnl Require Erlang >=R14B02 (ERTS 5.8.3) is required.
AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [ge], [5.8.3],
    [],
    [AC_MSG_ERROR([Erlang R14B02 or higher is required but only Erlang $ERLANG_RELEASE was found!])])

dnl Unicode module was buggy for R14B04 and previous (ERTS <= 5.8.5)
AC_MSG_CHECKING([for buggy unicode module])
AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [le], [5.8.5],
    [bad_unicode="yes"],
    [bad_unicode="no"])
AC_MSG_RESULT([${bad_unicode}])

HAVE_BAD_UNICODE=false
if test "x${bad_unicode}" = "xyes"; then
        HAVE_BAD_UNICODE=true
fi
AM_CONDITIONAL(HAVE_BAD_UNICODE, [test "x$HAVE_BAD_UNICODE" = "xtrue"])
AC_SUBST(HAVE_BAD_UNICODE)

dnl filelib:wildcard/2 was buggy for R15B03 and previous (ERTS <= 5.9.3)
AC_MSG_CHECKING([for buggy filelib:wildcard/2 function])
AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [le], [5.9.3],
    [bad_wildcard="yes"],
    [bad_wildcard="no"])
AC_MSG_RESULT([${bad_wildcard}])

HAVE_BAD_WILDCARD=false
if test "x${bad_wildcard}" = "xyes"; then
        HAVE_BAD_WILDCARD=true
fi
AM_CONDITIONAL(HAVE_BAD_WILDCARD, [test "x$HAVE_BAD_WILDCARD" = "xtrue"])
AC_SUBST(HAVE_BAD_WILDCARD)

dnl Check for support of honor_cipher_order SSL option (ERTS >= 6.0)
AC_MSG_CHECKING([for support of honor_cipher_order SSL option])
AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [ge], [6.0],
    [ssl_honor_cipher_order="yes"],
    [ssl_honor_cipher_order="no"])
AC_MSG_RESULT([${ssl_honor_cipher_order}])

HAVE_SSL_HONOR_CIPHER_ORDER=false
if test "x${ssl_honor_cipher_order}" = "xyes"; then
        HAVE_SSL_HONOR_CIPHER_ORDER=true
fi
AM_CONDITIONAL(HAVE_SSL_HONOR_CIPHER_ORDER, [test "x$HAVE_SSL_HONOR_CIPHER_ORDER" = "xtrue"])
AC_SUBST(HAVE_SSL_HONOR_CIPHER_ORDER)

dnl Check for support of log_alert SSL option (ERTS >= 5.10.3)
AC_MSG_CHECKING([for support of log_alert SSL option])
AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [ge], [5.10.3],
    [ssl_log_alert="yes"],
    [ssl_log_alert="no"])
AC_MSG_RESULT([${ssl_log_alert}])

HAVE_SSL_LOG_ALERT=false
if test "x${ssl_log_alert}" = "xyes"; then
        HAVE_SSL_LOG_ALERT=true
fi
AM_CONDITIONAL(HAVE_SSL_LOG_ALERT, [test "x$HAVE_SSL_LOG_ALERT" = "xtrue"])
AC_SUBST(HAVE_SSL_LOG_ALERT)

dnl Determine directories for installation.
if test "x${prefix}" != "xNONE" -a "x${ERLANG_INSTALL_LIB_DIR}" = "x"; then
        dnl Under $prefix
        ERLANG_INSTALL_LIB_DIR='${prefix}/lib'
else
        dnl Under default erlang libdir
        ERLANG_INSTALL_LIB_DIR='${ERLANG_LIB_DIR}'
fi

AC_ERLANG_SUBST_INSTALL_LIB_DIR

. ${srcdir}/vsn.mk
AC_SUBST(YAWS_VSN)

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_NAME, ${YAWS_VSN})


. ${srcdir}/applications/yapp/vsn.mk
AC_SUBST(YAPP_VSN)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(yapp, ${YAPP_VSN})




dnl ------------------------------------------------------------------
dnl Erlang applicatons.
dnl ------------------------------------------------------------------
AC_ERLANG_CHECK_LIB([kernel],,   [AC_MSG_ERROR(["kernel" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([stdlib],,   [AC_MSG_ERROR(["stdlib" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([sasl],,     [AC_MSG_ERROR(["compiler" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([erts],,     [AC_MSG_ERROR(["compiler" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([compiler],, [AC_MSG_ERROR(["compiler" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([crypto],,   [AC_MSG_ERROR(["crypto" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([xmerl],,    [AC_MSG_ERROR(["xmerl" Erlang application not found.])])

AC_ERLANG_CHECK_LIB([public_key],,[AC_MSG_WARN(["public_key" Erlang application not found.
It could be required to run Yaws or its testsuites.])])
AC_ERLANG_CHECK_LIB([ssl],,       [AC_MSG_WARN(["ssl" Erlang application not found.
It could be required to run Yaws or its testsuites.])])
AC_ERLANG_CHECK_LIB([mnesia],,    [AC_MSG_WARN(["mnesia" Erlang application not found.
It could be required to run Yaws or its testsuites.])])
AC_ERLANG_CHECK_LIB([eunit],,     [AC_MSG_WARN(["eunit" Erlang application not found.
It could be required to run Yaws or its testsuites.])])
AC_ERLANG_CHECK_LIB([erlsom],,     [AC_MSG_WARN(["erlsom" Erlang application not found.
It could be required to run Yaws or its testsuites.])])
AC_ERLANG_CHECK_LIB([xmlrpc],,     [AC_MSG_WARN(["xmlrpc" Erlang application not found.
It could be required to run Yaws or its testsuites.])])
AC_ERLANG_CHECK_LIB([inets],,     [AC_MSG_WARN(["xmlrpc" Erlang application not found.
It could be required to run Yaws or its testsuites.])])

cross_compiling=$tmp_cross_compiling

dnl ------------------------------------------------------------------
dnl Check  platform-specific stuff
dnl ------------------------------------------------------------------
echo
COLORED_ECHO([%BCheck platform-specific and system requirements%b])

AC_MSG_CHECKING([for the build and host architectures])
case "$host_os" in
     *cygwin*)
        AC_MSG_RESULT([windows (native)])
        CYGWIN=true
        AC_PATH_PROG(WERL, werl)
        AC_SUBST(WERL)
        ;;

     mingw|mingw32*)
        AC_MSG_RESULT([windows (cross-compiled)])
        MINGW=true
        AC_PATH_PROG(WERL, werl)
        AC_SUBST(WERL)
        ;;

      *)
        AC_MSG_RESULT([$host_os])

        dnl Sigh, pam headers do not seem to have a standard place. On FreeBSD and
        dnl Linuces I've seen they are in /usr/include/security, but MacOS has them
        dnl in /usr/include/pam.
        EPAM_CFLAGS=
        if test "x$enable_pam" != "xno"; then
            AC_CHECK_HEADERS([security/pam_appl.h pam/pam_appl.h pam_appl.h],[break])
            if test "x$ac_cv_header_security_pam_appl_h" = "xyes" ; then
                EPAM_CFLAGS="-I/usr/include/security"
            elif test "x$ac_cv_header_pam_pam_appl_h" = "xyes" ; then
                EPAM_CFLAGS="-I/usr/include/pam"
            elif test "x$ac_cv_header_pam_appl_h" = "xno" ; then
                AC_MSG_ERROR("Could not find pam_appl.h required for pam authentication")
            fi
            AC_CHECK_LIB(pam,pam_start,[],[AC_MSG_ERROR("Could not find pam library")])
        fi
        AC_SUBST(EPAM_CFLAGS)
        ;;
esac

AM_CONDITIONAL(CYGWIN, [test "x$CYGWIN" = "xtrue"])
AM_CONDITIONAL(MINGW, [test "x$MINGW" = "xtrue"])
AM_CONDITIONAL(WINDOWS, [test "x$MINGW" = "xtrue" -o "x$CYGWIN" = "xtrue"])

dnl check sendfile support:
dnl   - HAVE_SENDFILE: say if sendfile syscall is supported
dnl   - HAVE_ERLANG_SENDFILE: say if file:sendfile/5 exists

HAVE_SENDFILE=false
HAVE_ERLANG_SENDFILE=false

dnl Disabled because the yaws driver is not compiled for windows
if test "$WINDOWS" = true; then
        AC_MSG_CHECKING([for sendfile])
        AC_MSG_RESULT([no])
else
        AC_LANG(C)
        AC_CHECK_LIB([c],[sendfile],[HAVE_SENDFILE=true])
fi


AC_MSG_CHECKING([for file:sendfile/5])
file_sendfile=`"${ERL}" -noshell -eval 'code:ensure_loaded(file), io:format("~p~n",[[erlang:function_exported(file,sendfile,5)]]), erlang:halt().' | tail -1`
if test "$file_sendfile" = true; then
        AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [ge], [5.9.1],
            [AC_MSG_RESULT([yes])
             HAVE_ERLANG_SENDFILE=true],
            [AC_MSG_RESULT([no])])
else
        AC_MSG_RESULT([no])
fi
AM_CONDITIONAL(HAVE_SENDFILE, [test "x$HAVE_SENDFILE" = "xtrue"])
AM_CONDITIONAL(HAVE_ERLANG_SENDFILE, [test "x$HAVE_ERLANG_SENDFILE" = "xtrue"])
AC_SUBST(HAVE_SENDFILE)
AC_SUBST(HAVE_ERLANG_SENDFILE)

AC_MSG_CHECKING([for crypto:hash/2])
HAVE_CRYPTO_HASH=false
crypto_hash=`"${ERL}" -noshell -eval 'code:ensure_loaded(crypto), io:format("~p~n",[[erlang:function_exported(crypto,hash,2)]]), erlang:halt().' | tail -1`
if test "$crypto_hash" = true; then
        HAVE_CRYPTO_HASH=true
        AC_MSG_RESULT([yes])
else
        AC_MSG_RESULT([no])
fi
AM_CONDITIONAL(HAVE_CRYPTO_HASH, [test "x$HAVE_CRYPTO_HASH" = "xtrue"])
AC_SUBST(HAVE_CRYPTO_HASH)

AC_MSG_CHECKING([for inet:parse_strict_address/1])
HAVE_INET_PARSE_STRICT_ADDRESS=false
inet_parse_strict_address=`"${ERL}" -noshell -eval 'code:ensure_loaded(inet), io:format("~p~n",[[erlang:function_exported(inet, parse_strict_address,1)]]), erlang:halt().' | tail -1`
if test "$inet_parse_strict_address" = true; then
        HAVE_INET_PARSE_STRICT_ADDRESS=true
        AC_MSG_RESULT([yes])
else
        AC_MSG_RESULT([no])
fi
AM_CONDITIONAL(HAVE_INET_PARSE_STRICT_ADDRESS, [test "x$INET_PARSE_STRICT_ADDRESS" = "xtrue"])
AC_SUBST(HAVE_INET_PARSE_STRICT_ADDRESS)

dnl ------------------------------------------------------------------
dnl Check for programs used during Yaws compilation/installation
dnl ------------------------------------------------------------------
echo
COLORED_ECHO([%BCheck programs support%b])

dnl Make LaTeX and related commands configurable
dnl
dnl We might want to check whether latex is pdflatex and creates PDF instead of
dnl DVI without being given -output-format=dvi.  However, this at least allows
dnl to give the working command at configure time.
AC_ARG_VAR([DVILATEX], [(pdf)latex command creating DVI file])
AC_CHECK_PROG([DVILATEX], [latex], [latex])

AC_ARG_VAR([DVIPS], [dvips command])
AC_CHECK_PROG([DVIPS], [dvips], [dvips])

AC_ARG_VAR([PDFLATEX], [(pdf)latex command creating PDF file])
AC_CHECK_PROG([PDFLATEX], [pdflatex], [pdflatex])

AC_ARG_VAR([EPSTOPDF], [epstopdf command])
AC_CHECK_PROG([EPSTOPDF], [epstopdf], [epstopdf])

AC_ARG_VAR([MAN], [man command creating manpages])
AC_CHECK_PROG([MAN], [man], [man])

AC_ARG_VAR([PS2PDF], [ps2pdf command creating PDF file])
AC_CHECK_PROG([PS2PDF], [ps2pdf], [ps2pdf])

AM_CONDITIONAL(CAN_BUILD_DOC,
  [test "x$DVILATEX" != "x" && test "x$DVIPS" != "x" && test "x$PDFLATEX" != "x" && test "x$EPSTOPDF" != "x"])

AC_ARG_VAR([CADAVER], [command-line WebDAV client for Unix])
AC_CHECK_PROG([CADAVER], [cadaver], [cadaver])

AC_ARG_VAR([CURL], [command-line tool for transferring data with URL syntax])
AC_CHECK_PROG([CURL], [curl], [curl])

AC_ARG_VAR([UNIX2DOS], [converts DOS <-> Unix text files])
AC_CHECK_PROG([UNIX2DOS], [unix2dos], [unix2dos])

AC_ARG_VAR([TODOS], [converts DOS <-> Unix text files])
AC_CHECK_PROG([TODOS], [todos], [todos])

AC_ARG_VAR([INSTALL_BUILDER], [BitRock InstallBuilder directory ['${HOME}/installbuilder-5.4.14']])
if test "x${INSTALL_BUILDER}" = "x"; then
   INSTALL_BUILDER="${HOME}/installbuilder-5.4.14"
fi

dnl ------------------------------------------------------------------
dnl Final path substitutions
dnl ------------------------------------------------------------------

AC_ARG_VAR([confdir], [Default Yaws configuration directory ['$(sysconfdir)/yaws']])
if test "x${confdir}" = "x"; then
   confdir='$(sysconfdir)/yaws'
fi

AC_ARG_VAR([wwwdir], [Default Yaws docroot ['$(localstatedir)/yaws/www']])
if test "x${wwwdir}" = "x"; then
   wwwdir='$(localstatedir)/yaws/www'
fi

AC_ARG_VAR([appsdir], [Default Yaws applications directory ['$(localstatedir)/yaws']])
if test "x${appsdir}" = "x"; then
   appsdir='$(localstatedir)/yaws'
fi

AC_ARG_VAR([logdir], [Default Yaws log directory ['$(localstatedir)/log/yaws']])
if test "x${logdir}" = "x"; then
   logdir='$(localstatedir)/log/yaws'
fi

AC_ARG_VAR([yawsdir], [Default Yaws installation directory ['$(libdir)/yaws'].
If not specified, it is a link on '$(ERLANG_INSTALL_LIB_DIR_yaws)'])
if test "x${yawsdir}" = "x"; then
   yawsdir='$(libdir)/yaws'
else
   dnl If yawsdir is defined, replace ERLANG_INSTALL_LIB_DIR_yaws too
   ERLANG_INSTALL_LIB_DIR_yaws='${yawsdir}'
fi

AC_ARG_VAR([yappdir], [Default Yapp installation directory ['$(libdir)/yapp'].
If not specified, it is a link on '$(ERLANG_INSTALL_LIB_DIR_yapp)'])
if test "x${yappdir}" = "x"; then
   yappdir='$(libdir)/yapp'
else
   dnl If yappdir is defined, replace ERLANG_INSTALL_LIB_DIR_yapp too
   ERLANG_INSTALL_LIB_DIR_yapp='${yappdir}'
fi

dnl ------------------------------------------------------------------
dnl Autoconf output.
dnl ------------------------------------------------------------------
AC_SUBST([CONFIG_STATUS_DEPENDENCIES], ['$(top_srcdir)/vsn.mk $(top_srcdir)/applications/yapp/vsn.mk'])

AC_CONFIG_FILES([
        yaws.pc

        Makefile
        c_src/Makefile
        win32/Makefile
        win32/build.xml
        man/Makefile
        doc/Makefile
        scripts/Makefile
        src/Makefile
        ebin/Makefile
        examples/src/Makefile
        examples/ebin/Makefile
        examples/include/Makefile
        www/code/Makefile
        www/shoppingcart/Makefile
        test/Makefile
        test/src/Makefile
        test/eunit/Makefile
        test/t1/Makefile
        test/t2/Makefile
        test/t3/Makefile
        test/t4/Makefile
        test/t5/Makefile
        test/t6/Makefile
        test/t7/Makefile
        test/t8/Makefile
        test/t9/Makefile
        test/t10/Makefile
        test/t11/Makefile
        applications/chat/Makefile
        applications/chat/src/Makefile
        applications/chat/ebin/Makefile
        applications/mail/Makefile
        applications/mail/src/Makefile
        applications/mail/ebin/Makefile
        applications/wiki/Makefile
        applications/wiki/src/Makefile
        applications/wiki/ebin/Makefile
        applications/yapp/Makefile
        applications/yapp/src/Makefile
        applications/yapp/ebin/Makefile

        src/yaws_charset.hrl
        src/yaws_appdeps.hrl

        test/eunit//subconfig_DATA/yaws_absolute.conf
        test/eunit//subconfig_DATA/yaws_absolute_no_wildcard.conf
        test/eunit//subconfig_DATA/global_absolute_subconfig.conf
        test/eunit//subconfig_DATA/global_absolute_no_wildcard_subconfig.conf
        test/support/yaws.conf
        test/t1/yaws.conf
        test/t2/yaws.conf
        test/t3/yaws.conf
        test/t4/yaws.conf
        test/t5/yaws.conf
        test/t6/no_docroot_appmod.conf
        test/t6/no_docroot_fwdproxy.conf
        test/t6/no_docroot_redirect.conf
        test/t6/no_docroot_revproxy.conf
        test/t7/yaws.conf
        test/t8/yaws.conf
        test/t9/yaws.conf
        test/t10/yaws.conf
        test/t11/yaws.conf
])
AC_CONFIG_FILES([scripts/make-release],                  [chmod +x scripts/make-release])
AC_CONFIG_FILES([applications/wiki/scripts/addFile],     [chmod +x applications/wiki/scripts/addFile])
AC_CONFIG_FILES([applications/wiki/scripts/getPassword], [chmod +x applications/wiki/scripts/getPassword])
AC_CONFIG_FILES([applications/wiki/scripts/importFiles], [chmod +x applications/wiki/scripts/importFiles])
AC_CONFIG_FILES([applications/wiki/scripts/updateWiki],  [chmod +x applications/wiki/scripts/updateWiki])

dnl when srcdir != builddir, we need to create some symlinks
AC_CONFIG_FILES([include/Makefile], [
  AS_IF([test -e include/yaws.hrl],
            [], [ln -s ${ac_abs_srcdir}/yaws.hrl include/yaws.hrl])
  AS_IF([test -e include/yaws_api.hrl],
            [], [ln -s ${ac_abs_srcdir}/yaws_api.hrl include/yaws_api.hrl])
])
AC_CONFIG_FILES([priv/Makefile], [
  AS_IF([test -e priv/mime.types],
        [], [ln -s ${ac_abs_srcdir}/mime.types priv/mime.types])
])

AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------
echo
COLORED_ECHO([ %B== ${PACKAGE_NAME} ${PACKAGE_VERSION} ==%b])
echo
COLORED_ECHO([Configuration:])
COLORED_ECHO([ %BApplication dir.%b: ${ERLANG_INSTALL_LIB_DIR_yaws}])
echo
COLORED_ECHO([ %BPrefix%b:  ${prefix}])
COLORED_ECHO([ %Betcdir%b:  ${sysconfdir}])
COLORED_ECHO([ %Bconfdir%b: ${confdir}])
COLORED_ECHO([ %Bbindir%b:  ${bindir}])
COLORED_ECHO([ %Bvardir%b:  ${localstatedir}])
COLORED_ECHO([ %Bwwwdir%b:  ${wwwdir}])
COLORED_ECHO([ %Bappsdir%b: ${appsdir}])
COLORED_ECHO([ %Blogdir%b:  ${logdir}])
COLORED_ECHO([ %Byawsdir%b: ${yawsdir}])
COLORED_ECHO([ %Bdocdir%b:  ${docdir}])
COLORED_ECHO([ %Bmandir%b:  ${mandir}])
echo
COLORED_ECHO([ %BErlang root dir%b:          ${ERLANG_ROOT_DIR}])
COLORED_ECHO([ %BErlang lib dir%b:           ${ERLANG_LIB_DIR}])
COLORED_ECHO([ %BErlang compiler%b:          ${ERLC}])
COLORED_ECHO([ %BErlang emulator%b:          ${ERL}])
COLORED_ECHO([ %BWindows Erlang emulator%b:  ${WERL}])
echo

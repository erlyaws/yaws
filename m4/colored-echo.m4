dnl
dnl Macro to output bold and colored text.
dnl

dnl COLORED_ECHO_INIT
dnl This macro must be called first to initialize a context.
AC_DEFUN([COLORED_ECHO_INIT],
[
  dnl Prepare fancy console output, taken from GNU shtools.
  colored_echo_bold=''
  colored_echo_norm=''

  AC_PROG_AWK
  if test x"$AWK" = x"not found"; then
    return
  fi

  case $TERM in
  xterm|xterm*|vt220|vt220*)
    dnl For the most important terminal types we directly know the
    dnl sequences.
    colored_echo_bold=`$AWK 'BEGIN { printf("%c%c%c%c", 27, 91, 49, 109); }' </dev/null 2>/dev/null`
    colored_echo_norm=`$AWK 'BEGIN { printf("%c%c%c", 27, 91, 109); }' </dev/null 2>/dev/null`
    ;;

  vt100|vt100*|cygwin)
    colored_echo_bold=`$AWK 'BEGIN { printf("%c%c%c%c%c%c", 27, 91, 49, 109, 0, 0); }' </dev/null 2>/dev/null`
    colored_echo_norm=`$AWK 'BEGIN { printf("%c%c%c%c%c", 27, 91, 109, 0, 0); }' </dev/null 2>/dev/null`
    ;;

  *)
    dnl For all others, we try to use a possibly existing `tput' or
    dnl `tcout' utility.
    paths=`$ECHO $PATH | sed -e 's/:/ /g'`
    for tool in tput tcout; do
      for dir in $paths; do
        if test -r "$dir/$tool" ; then
          for seq in bold md smso; do
            bold="`$dir/$tool $seq 2>/dev/null`"
            if test ".$bold" != . ; then
              colored_echo_bold="$bold"
              break
            fi
          done
          if test ".$colored_echo_bold" != . ; then
            for seq in sgr0 me rmso init reset; do
              norm="`$dir/$tool $seq 2>/dev/null`"
              if test ".$norm" != . ; then
                colored_echo_norm="$norm"
                break
              fi
            done
          fi
          break
        fi
      done
      if test x"$colored_echo_bold" != "x" -a x"$colored_echo_norm" != "x"; then
        break;
      fi
    done
    ;;
  esac
])

dnl COLORED_ECHO(TEXT)
dnl Output text with:
dnl   o  everything between %B and %b displayed bold.
AC_DEFUN([COLORED_ECHO],
[
  text=`$ECHO $seo "$1" | sed -e "s/%B/${colored_echo_bold}/g" -e "s/%b/${colored_echo_norm}/g" 2>/dev/null`
  $ECHO $seo "$text"

  text=`$ECHO $sec "$1" | sed -e "s/%B//g" -e "s/%b//g" 2>/dev/null`
  _AS_ECHO_LOG($text)
])

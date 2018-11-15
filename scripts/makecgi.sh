#!/bin/sh
# Script to compile a Curry program implementing a web script
# using the package `html` and the library `HTML.Base`
# into a cgi script to be placed in a server-accessible directory
# for executing cgi scripts.

# Root of Curry system must be the first argument:
CURRYROOT=$1
shift

# Standard suffix that will be added to the main script:
CGISUFFIX="_CGIMAIN_$$"
# Name of the main function in the main script (should not be in conflict
# with any exported name of the Curry program)
MAINCALL="main_cgi_9999_$$"

CPMEXEC="cypm exec"
ERROR=
HELP=no
CURRYDOPTIONS=
CURRYOPTIONS=":set -time :set -interactive"
COMPACT=no
ULIMIT="-t 120"
MAIN=main
CGIFILE=
WUIJS=no
WUIMODULES=
SERVERTIMEOUT=
STANDALONE=no
LOADBALANCE="-loadbalance standard"
ARGS=

while [ $# -gt 0 -a -z "$ERROR" ]; do
  case $1 in
   -help | -h | -\? ) HELP=yes ;;
   -D*              ) CURRYDOPTIONS="$CURRYDOPTIONS $1" ;;
   -cpm             ) echo 'Option "-cpm" deprecated.' ;;
   -cpmexec         ) shift ; CPMEXEC=$1 ;;
   -compact         ) COMPACT=yes ;;
   -servertimeout   ) shift ; SERVERTIMEOUT="-servertimeout $1" ;;
   -multipleservers ) LOADBALANCE="-loadbalance multiple" ;; # backward compt.
   -loadbalance     ) shift ; LOADBALANCE="-loadbalance $1" ;;
   -standalone      ) STANDALONE=yes ;;
   -ulimit          ) shift; ULIMIT=$1 ;;
   -wuijs           ) WUIJS=yes ;;
   -wui             ) shift; WUIMODULES="$WUIMODULES $1" ;;
   -m               ) shift; MAIN=$1 ;;
   -o               ) shift; CGIFILE=$1 ;;
   -*               ) ERROR="Unknown option: $1" ;;
   *                ) ARGS="$ARGS $1" ;; # collect non-option arguments
  esac
  shift
done
if test -n "$ARGS" ; then
  set $ARGS
fi

if [ $HELP = yes ] ; then
  set "1" ; shift # to show next usage message
fi

if test -n "$ERROR" ; then
  echo "ERROR: $ERROR"
  set "1" ; shift # to show next usage message
fi

if [ $# != 1 -a $# != 3 ] ; then
  echo "USAGE: curry-makecgi [options] <curry>"
  echo
  echo "MAIN OPTIONS:"
  echo "-o <cgi>   : name of the file (with suffix .cgi) where the cgi program should"
  echo "             be stored (default: <curry>.cgi)."
  echo "-m <form>  : Curry expression (of type IO HtmlForm) computing the HTML form"
  echo "             (default: main)."
  echo "<curry>    : name of the Curry program (without suffix) containing the script"
  echo
  echo "FURTHER OPTIONS:"
  echo '-Dname=val  : define (curry)rc property "name" as "val"'
  echo "-cpmexec <c>: set the command to execute programs with the Curry Package"
  echo "              Manager (default: 'cypm exec')"
  echo "-compact    : reduce size of generated cgi program by deleting unused functions"
  echo "-ulimit <l> : set 'ulimit <l>' when executing the cgi program"
  echo "              (default: '-t 120')"
  echo "-servertimeout <ms>: set the timeout for the cgi server process to"
  echo "                     <ms> milliseconds (default: 7200000 / two hours)"
  echo "-loadbalance <t>: start new server process if load for one server is"
  echo "                  high where <t> specifies the kind of high load."
  echo "                  Current possible values for <t>:"
  echo "        no:       no load balance"
  echo "        standard: some standard load balancing (default)"
  echo "        multiple: new server process for each initial call to"
  echo "                  a cgi script (only reasonable with short timeout)"
  echo "-standalone : generate standalone script (i.e., copy programs"
  echo "              required from package 'html-cgi' to deploy directory)"
  echo "-wuijs      : generate JavaScript support code for WUIs"
  echo "-wui <mod>  : consider also imported module <mod> (that contains WUI"
  echo "              specifications) when generating JavaScript support code"
  exit 1
fi

# Try to locate WUI/JavaScript translator:
WUIJS_PREPROCESSOR=`which curry2js`
if [ ! -x "$WUIJS_PREPROCESSOR" ] ; then
  # try to set curry2js to the CPM standard location:
  WUIJS_PREPROCESSOR=$HOME/.cpm/bin/curry2js
  if [ ! -x "$WUIJS_PREPROCESSOR" ] ; then
    WUIJS_PREPROCESSOR=
  fi
fi
if [ -z "$WUIJS_PREPROCESSOR" -a $WUIJS = yes ] ; then
  echo "No support for JavaScript possible!"
  echo "Please install the Curry->JavaScript translator curry2js by:"
  echo "> cypm update && cypm install curry2js"
  exit 1
fi

# remove possible suffix:
PROG=`expr $1 : '\(.*\)\.lcurry' \| $1`
PROG=`expr $PROG : '\(.*\)\.curry' \| $PROG`

if test -z "$CGIFILE" ; then
  CGIFILE=$PROG.cgi
fi
MAINMOD=$PROG$CGISUFFIX
MAINCURRY=$MAINMOD.curry

# compute (relative) name of cgi program:
CGIDIR=`dirname $CGIFILE`
if [ $CGIDIR = "." ] ; then
  CGIPROG=$CGIFILE
else
  CGIPROG=`expr $CGIFILE : "$CGIDIR/\(.*\)"`
fi

# name of the server:
CGIFILEPATHNAME=`(cd $CGIDIR > /dev/null ; pwd)`
CGISERVERPROG=$CGIPROG.server
CGISERVEREXEC=$CGIFILEPATHNAME/$CGIPROG.server
CGISERVERCMD=$CGISERVEREXEC.sh

# unique key for this cgi script:
CGIKEY="$CGIFILEPATHNAME/$CGIPROG `date '+%m/%d/%y/%H/%M/%S'`"

# generate server program implementing the cgi script application:
rm -f $MAINCURRY
echo "module $MAINMOD($MAINCALL) where" >> $MAINCURRY
echo "import $PROG" >> $MAINCURRY
echo "import HTML.Base" >> $MAINCURRY
echo "import HTML.CGI.Server" >> $MAINCURRY
echo "$MAINCALL :: IO ()" >> $MAINCURRY
if [ $WUIJS = no ] ; then
  echo "$MAINCALL = runFormServerWithKey \"$CGIPROG\" \"$CGIKEY\" ($MAIN)" >> $MAINCURRY
else
  CGIBASE=`expr $CGIPROG : '\(.*\)\.cgi' \| $CGIPROG`
  JSFILE=$CGIBASE\_wui.js
  $CPMEXEC $WUIJS_PREPROCESSOR -wui -o $JSFILE $PROG $WUIMODULES
  if [ $? != 0 ] ; then
    rm -f $MAINCURRY
    exit $?
  fi
  chmod 644 $JSFILE
  if [ $CGIDIR != "." ] ; then
    mv $JSFILE $CGIDIR/$JSFILE
  fi
  echo "$MAINCALL = runFormServerWithKeyAndFormParams \"$CGIPROG\" \"$CGIKEY\" [FormJScript \"$JSFILE\",FormOnSubmit \"return submissionAllowed()\"] ($MAIN)" >> $MAINCURRY
fi

# compile main module:
echo "Generating saved state for initial expression: $MAIN"
$CPMEXEC $CURRYROOT/bin/curry --nocypm $CURRYDOPTIONS $CURRYOPTIONS :l $MAINMOD :save $MAINCALL :q

# now the file $MAINMOD should contain the executable computing the HTML form:
if test ! -f $MAINMOD ; then
  echo "Error occurred, generation aborted."
  $CURRYROOT/bin/cleancurry $MAINMOD
  rm -f $MAINMOD.curry
  exit 1
fi

# set executable of cgi registry:
CGIREG=curry-cgi
CGIREGISTRY=`which $CGIREG`
if [ ! -x "$CGIREGISTRY" ] ; then
  echo "ERROR: '$CGIREG' not found in path! Install it by:"
  echo "       > cypm install html-cgi"
  rm -f $MAINMOD $MAINMOD.curry
  exit 1
else
  CGIREGISTRY=`realpath $CGIREGISTRY`
fi

# stop old server, if necessary:
if [ -f $CGISERVEREXEC ] ; then
  echo "Stop old version of the server '$CGISERVEREXEC'..."
  $CGIREGISTRY stopscript "$CGISERVEREXEC"
fi

# copy executable if required:
if [ $STANDALONE = yes ] ; then
  cp -p "$CGIREGISTRY" $CGIFILEPATHNAME/$CGIREG
  CGIREGISTRY="./$CGIREG"
fi

# generate cgi script:
rm -f $CGIFILE
echo "#!/bin/sh" >> $CGIFILE
if test -n "$LANG" ; then
  echo "LANG=$LANG" >> $CGIFILE
  echo "export LANG" >> $CGIFILE
fi
echo "$CGIREGISTRY submit $SERVERTIMEOUT $LOADBALANCE \"$CGIPROG\" \"$CGIKEY\" \"$CGISERVERCMD\" 2>> $CGIFILE.log" >> $CGIFILE
chmod 755 $CGIFILE

# set bin directory containing CPNSD
CPNSD=curry-cpnsd
CPNSDPATH=`which $CPNSD`
if [ -x "$CPNSDPATH" ] ; then
  CPNSDBINDIR=`dirname "$CPNSDPATH"`
else
  echo "ERROR: '$CPNSD' not found in path! Install it by:"
  echo "       > cypm install cpns"
  rm -f $MAINMOD $MAINMOD.curry
  exit 1
fi

# move compiled executable to final position and generate small shell
# script to call the executable with ulimit and correct path:
mv $MAINMOD $CGISERVEREXEC
chmod 755 $CGISERVEREXEC
echo "#!/bin/sh" > $CGISERVERCMD
if test -n "$ULIMIT" ; then
  echo "ulimit $ULIMIT" >> $CGISERVERCMD
fi
echo "PATH=\"\$PATH:$CPNSDBINDIR\"" >> $CGISERVERCMD
echo "exec $CGISERVEREXEC \"\$@\"" >> $CGISERVERCMD
chmod 755 $CGISERVERCMD

$CURRYROOT/bin/cleancurry $MAINMOD
rm -f $MAINMOD.curry

echo "`date`: cgi script compiled" > $CGIFILE.log

echo
echo "New files \"$CGIFILE[.server]\" with compiled cgi script generated."

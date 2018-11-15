html: Support for HTML programming
==================================

This package contains libraries to support HTML programming.

In order to execute dynamic web pages, you need to install
the Curry Port Name Server (CPNS), the HTML/CGI Registry,
and the script to generate CGI scripts from Curry programs.
This can be easily done by the commands

    > cypm install cpns
    > cypm install html-cgi
    > cypm install html

These commands install the executables `curry-cpnsd`, `curry-cgi`,
and `curry-makecgi` in the bin directory of CPM.
`curry-makecgi` is used to compiler a dynamic web page implemented
in Curry, whereas the other executables are invoked during
the execution of a dynamic web page.

Some simple examples for dynamic web pages can be found in the
directory `examples`.

--------------------------------------------------------------------------

CGI Registry
------------

The CGI registry is a table of all active CGI server processes.
Usually, all CGI server processes are automatically started
or terminated (e.g., after 120 minutes of inactivity).
In order to manage these processes manually, one can use
the CGI registry. For this purpose, one can install the
web script `registry.cgi` (from the program `WebRegistry.curry`
in subdirectoy `helpers`, see more information there).
Then one can use this web script to see and manipulate the
current registry from the local host of the web server,
e.g., by a web browser or in a terminal via `curl`.

For instance, to see a list of active CGI server processes, execute

    curl http://localhost/.../registry.cgi?show

To clean the registry from old CGI server processes, execute

    curl http://localhost/.../registry.cgi?clean

--------------------------------------------------------------------------

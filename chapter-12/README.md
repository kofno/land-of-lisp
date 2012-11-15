# CHAPTER 12: Sockets with USOCKETS

Since I'm using sbcl instead of CLISP, I decided to work the socket
exercises using usockets.

The easiest way to get USOCKETS (or most CL libraries, as near as I
can tell) is to use
[QuickLisp](http://www.quicklisp.org/beta/). Follow that link to get
instructions for installing QuickLisp. I recommend running
(ql:add-to-init-file) as well.

Then, to install usockets:

    (ql:quickload "usockets")

Assuming that all went well, the code here should work fine for this
chapter.
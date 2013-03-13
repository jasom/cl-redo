About
-----

This is a re-implementation of Daniel J. Bernstein's redo_

Huge credit to apenwarr, for making such a nice set of tests in `his
implementation`_.  That made writing this almost trivial, as he did the hard
part.

Bugs
----

#) Everything uses absolute paths.
#) The "server" branch implements a simple client/server that saves the lisp
   memory image startup time.  This reduces the overhead of lisp to essentially
   zero.  Right now, however, "redo" does not do anything, you need to say
   "redo all" It's an easy fix but fixing #1 is higher priority right now.
#) I would like to try using elephant as the backing store; this could make a
   parallel implementation easier as locks and transactions come for free with
   it.  Right now I don't have the time to get elephant actually working on my
   machine though (it doesn't seem to work out of the box)

Questions
---------

Why?
  Mainly because I wanted to play around with the posixy stuff in iolib.
  
  This involves forking and spawning shells and all that jazz, so it was a good
  way to get familiar with it.  Also, I find redo to be quite nice.  It does
  most of the things that make does with far less complexity.


.. _redo: http://cr.yp.to/redo.html
.. _his implementation: https://github.com/apenwarr/redo

Implementation notes
====================

Control flow
------------

Control tokens. Events can break control flow.

JavaScript has trouble with synchronous requests, does the UI block or not? Unclear. In any case, we have to go asynchronous, then.

Garbage collection
------------------

StablePtr. UnstablePtr.

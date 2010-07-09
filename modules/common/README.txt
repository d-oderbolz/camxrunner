$Id$ 

What is this directory about?
_____________________________


This directory contains the common scripts, which are automatically
included (sourced) into the CAMxRunner Scripts.
Common functions are mostly CAMx Version specific, so each one in its own directory
named like the CAMx Version.
CAMxRunner allows to overload functions (because bash supports it), 
so for example the function <get_chemparam_file> may be defined more than once.
If you do this, please give each of the files a UNIQUE name, otherwise the test subsystem 
will find only one definition (usually the one in the deepest level).



See also http://people.web.psi.ch/oderbolz/CAMxRunner
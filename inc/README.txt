$Id$ 

What is this directory about?
_____________________________


This directory contains important include files,
which are called using  *source* (the use of the 
. operator is not recommended as it is not very obvious).

The reason these files are here are:
- we can include them independent of CAMxRunner.sh (great for testing)
- we can do stuff we can't do in a function 
  (like in load_common_modules.inc, where we source functions)
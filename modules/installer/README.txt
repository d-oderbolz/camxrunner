$Id$ 

What is this directory about?
_____________________________


This directory contains the installer files for both CAMxRunner, CAMx and the
testcase.

Be aware that these modules are called in this order:
- General installers
- Model scecific installers
- Version specific installers

The order of these modules is determined lexically, so it is advised to use a
numeric prefix 00_ - 99_.
This is in contrast to all other modules, where the order is dermined by dependencies.

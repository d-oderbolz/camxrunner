$Id: README.txt 172 2008-10-01 09:20:59Z oderbolz $ 

What is this directory about?
_____________________________


This directory contains patches for the installation.
The files have the same relative path as in the bin/$CXR_CAMX_VERSION/src/CAMx directory,
the installer automatically knows about those files (because they are present).

But really make sure their relative path is OK!

Note that patches are organised in directories according to the platform they belong to.
The directory all_platforms contains general patches that will be applied before any other patches,
then follow the ones for the platform in question.

Each <file.patch> file will be applied as a patch to <file>, so be careful.

Create patches like this:

$ diff -Naur old/file new/file > file.patch






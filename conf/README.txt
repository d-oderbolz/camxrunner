$Id$ 

What is this directory about?
_____________________________


This directory contains the configure files for the CAMxRunner.

The base.cenf acts like a template, its settings are overridden 
by settings of the specific configur files.
The CAMxRunner checks the version and makes sure it matches.

When changing the base.conf, make sure to commit it to the 
SVN Repository using

	svn commit -m"This is wath I did ..." base.conf 

See also http://people.web.psi.ch/oderbolz/CAMxRunner 
$Id$ 

What is this directory about?
_____________________________


This directory contains the preprocessors, which process or deliver CAMx input. 
Preprocessors are mostly CAMx Version specific, so each one in its own directory
named like the CAMx Version.

Unlike the postprocessors, all postprocessors must have run before CAMx will be run,
so there are no per-day preprocessors.

The order in which the processors are run depends ENTIRELY on the naming convention.
All processors must have a name in the form 

	XX_what_ever.sh 

Where XX is a 2-Digit number. To disable a processor, rename the extension to something like

	XX_what_ever.sh.disabled

See also http://people.web.psi.ch/oderbolz/CAMxRunner
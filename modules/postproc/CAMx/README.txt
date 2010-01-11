$Id$ 

What is this directory about?
_____________________________


This directory contains the postprocessors, which process CAMx output. 
Postprocessors are mostly CAMx Version specific, so each one in its own directory
named like the CAMx Version.

Unlike the preprocessors, there are postprocessors which can run on a daily basis,
allowing faster output (as soon as a day is simulated, these postprocessors can run).
These are located in the "single" subsubdirectory.
Those in the "finish" subsubdirectory are run once all days have been simulated.

The order in which the processors are run depends ENTIRELY on the naming convention.
All processors must have a name in the form 

	XX_what_ever.sh 

Where XX is a 2-Digit number. To disable a processor, rename the extension to something like

	XX_what_ever.sh.disabled


See also http://people.web.psi.ch/oderbolz/CAMxRunner
# CAMxRunner

CAMxRunner is an environment to perform complex multi-day/multi-domain air quality modelling using CAMx
as described in [this paper](http://www.researchgate.net/publication/232724998_CAMxRunner_a_modular_framework_for_efficient_chemical_transport_modelling).

I wrote it as part of my [PhD thesis](http://e-collection.library.ethz.ch/view/eth:6759) to run [CAMx](http://www.camx.com/) air quality simulations.
The longest simulations I ran where a one-year run, which typically requires some stamina and even more time.

The general idea of the system is to identify parallelizable parts of the preprocessing/simulation/postprocessing chain and is able to run these independently.
(Of course, the air quality model itself also does this, but I noticed that I spent a lot of time in pre- and postprocessing).
It does this by analyzing the dependency between the different steps and the queues them so that worker threads can execute them.

Somewhat anachronistically, the system is written in good old [bash](http://www.tldp.org/LDP/abs/html/) - these days, I would write it in [Python](https://www.python.org/).

Note that I am no longer working in the field of atmospheric chemistry and my limited spare time does not allow me to further maintain this project.


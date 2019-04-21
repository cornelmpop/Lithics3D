[![DOI](https://zenodo.org/badge/147242315.svg)](https://zenodo.org/badge/latestdoi/147242315)

# Lithics3D

Lithics3D provides a toolbox for working with 3D scans of archaeological lithics
within the R environment. The functions included herein are designed to
work with clean triangular meshes and existing landmarks. For the time being at
least, mesh generation, pre-processing (e.g. cleaning), and landmarking must be
done with different tools (e.g. [Meshlab](http://www.meshlab.net/) and
[Landmark Editor](http://graphics.idav.ucdavis.edu/research/EvoMorph))


The included functions can be divided into two broad categories:

* High-level functions for (semi-)automatic segmentation and analysis of meshes
*qua* archaeological artifacts (e.g. utilities for automatically
measuring edge angles, mapping artifact thickness, etcetera).

* Low-level functions for working with meshes as geometric objects. These
functions are meant to provide the building blocks required to implement more
complex functionality.

#### **Warning**: A note on current status

This package incorporates functions written over a period of several years. Some
of these functions are far more experimental than others, and there are many
naming inconsistencies which are slowly being resolved. You can expect function
names to change without notice, but all functions added in or since the
v0.4.0 release will continue working as originally documented until version 1 of
the package.

Given the experimental nature of some of the functions included in this package
you are strongly encouraged to double check your results and contact me for help or
clarification. More generally, be mindful of the fact that this is very much a
work in progress.

## Getting started


### Installation

If you haven't done so already, install devtools in R:

``` r
install.packages("devtools")
```

Once devtools is available you can install Lithics3D from github with the
following R commands:

``` r
require(devtools)
install_github("cornelmpop/Lithics3D", dependencies=T, build_vignettes=T)
```

### Using Lithics3D

For basic information about the package, including a list of available functions
and data, please load the package and consult the manual:

``` r
library(Lithics3D)
?Lithics3D
```

Please also check the wiki at: https://github.com/cornelmpop/Lithics3D/wiki, and
feel free to explore the available vignettes (a work in progress!). For example:

``` r
vignette("Lithics3D")
```

# Lithics3D

Lithics3D provides a toolbox for working with 3D scans of archaeological lithics
within the R environment. The functions included herein are designed to
work with clean triangular meshes and existing landmarks. For the time being at
least mesh generation, pre-processing (e.g. cleaning), and landmarking must be
done with different tools. The provided functions can be divided into two
categories:

* High-level functions for (semi-)automatic segmentation and analysis of meshes
*qua* archaeological artifacts (e.g. utilities for automatically
measuring edge angles, mapping artifact thickness, etcetera).

* Low-level functions for working with meshes as geometric objects. These
functions are meant to provide the building blocks required to implement more
complex functionality.

#### A note on current status

Lithics3D currently incorporates functions written over a period
of several years, and some of these functions are far more experimental than
others. For now, please be mindful of the fact that this is very much a work in
progress, and test your results or contact me for help or clarification.

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
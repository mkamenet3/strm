# strm

`strm`

`strm` is an `R` package that fits spatio-temporal regression model based on Chi & Zhu *Spatial Regression Models for the Social Sciences* (2019). The approach here fits a simultaneous spatial error model (SAR) while incorporating a temporally lagged response variable and temporally lagged explanatory variables.

This package builds on the `errorsarlm()` function from the `spatialreg` package.

This package is still under development. Please report bugs or constructive tips to issues [here](https://github.com/mkamenet3/strm/issues).

## Installation

`strm` was built on R version 4.0.2 ("Taking Off Again").

Package dependencies include:

    - R (>= 3.6),
    - spatialreg (>= 1.1-5),
    - dplyr (>= 1.0.0)

Package imports include:

    - rlang (>= 0.4.6),
    - tidyr (>= 1.0.0),
    - purrr (>= 0.3.4),
    - magrittr (>= 1.5),
    - rgdal (>= 1.5.10),
    - spdep (>= 1.1.3),
    - lazyeval,
    - stats,
    - grDevices,
    - methods,
    - graphics,
    - utils,
    - knitr,
    - testthat (>= 2.3.2),
    - rmarkdown (>= 2.3)
    
Package suggests include:

    - splm (>= 1.4.11),
    - spdep (>= 1.1-3),
    - sf (>= 0.9-4),
    - Ecdat (>= 0.3-7),
    - tidycensus (>= 0.9.9),
    - ggplot2 (3.3.2),
    - patchwork (>= 1.0.1),
    - broom (>= 0.7.0)


To download the latest version of `strm`:

```R
library("devtools")
devtools::install_github("mkamenet3/strm")

```

  <!-- badges: start -->
  [![Travis build status](https://travis-ci.com/mkamenet3/strm.svg?branch=master)](https://travis-ci.com/mkamenet3/strm)
  
  <!-- badges: end -->
<!--[![Build Status](https://travis-ci.com/mkamenet3/strm.svg?token=aPo4kopCe3udvbX77YvH&branch=master)](https://travis-ci.com/mkamenet3/strm)-->

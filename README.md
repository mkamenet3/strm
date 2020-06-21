# strm

`strm`

`strm` is an `R` package that fits spatio-temporal regression model based on Chi & Zhu *Spatial Regression Models for the Social Sciences* (2019). The approach here fits a simultaneous spatial error model (SAR) while incorporating a temporally lagged response variable and temporally lagged explanatory variables.

This package builds on the `errorsarlm()` function from the `spatialreg` package.

This package is still under development. Please report bugs or constructive tips to issues [here](https://github.com/mkamenet3/strm/issues).

## Installation

`strm` was built on R version 3.6.1 ("Action of the Toes").

Package dependencies include:

- dplyr (>= 0.8.3)
- tidyr (>= 1.0.0)
- purrr (>= 0.3.3)
- spatialreg (>= 1.1.5)


To download the latest version of `strm`:

```R
library("devtools")
devtools::install_github("mkamenet3/strm")

```
  <!-- badges: start -->
  [![Travis build status](https://travis-ci.com/mkamenet3/spatemperr.svg?branch=master)](https://travis-ci.com/mkamenet3/spatemperr)
  <!-- badges: end -->

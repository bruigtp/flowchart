# flowchart

[![CRAN
status](https://www.r-pkg.org/badges/version/flowchart)](https://cran.r-project.org/package=flowchart)
  
[![R-CMD-check](https://github.com/bruigtp/flowchart/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bruigtp/flowchart/actions/workflows/R-CMD-check.yaml)
   [![Codecov test
coverage](https://codecov.io/gh/bruigtp/flowchart/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bruigtp/flowchart?branch=main)
[![](https://cranlogs.r-pkg.org/badges/flowchart)](https://cran.r-project.org/package=flowchart)
  
[![](https://cranlogs.r-pkg.org/badges/grand-total/flowchart)](https://cran.r-project.org/package=flowchart)
[![DOI:
10.5334/jors.649](https://img.shields.io/badge/DOI-10.5334%2Fjors.649-blue)](https://doi.org/10.5334/jors.649)

## Tidy Flowchart Generator

`flowchart` is an R package for drawing participant flow diagrams
directly from a dataframe using tidyverse. It provides a set of
functions that can be combined with a pipe operator to create all kinds
of flowcharts from a dataframe in an easy way.

You can see the package in action in:
<https://bruigtp.github.io/flowchart/>

## How to install it?

The package is available on CRAN:
<https://cran.r-project.org/web/packages/flowchart/index.html>.

``` r

install.packages("flowchart")
```

We can download the development version from the github repository:

``` r

# install.packages("remotes")
remotes::install_github('bruigtp/flowchart')
```

## How it works?

The following GIF provides an example of the tidy process of drawing a
flowchart for a clinical trial:

![animated](https://github.com/bruigtp/flowchart/raw/main/data-raw/flowchart_example.gif)

## About

Package: flowchart

Type: Package

Version: 1.0.0 (CRAN)

Authors: Pau Satorra, João Carmezim, Natàlia Pallarès, Cristian Tebé,
Kenneth A. Taylor.

Maintainer: Pau Satorra

License: GPL (\>= 3)

Encoding: UTF-8

Depends: R (\>= 4.1.0)

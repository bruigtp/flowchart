# flowchart

## Tidy Flowchart Generator

`flowchart` is an R package for drawing participant flow diagrams directly from a dataframe using tidyverse. It provides a set of functions that can be combined with a pipe operator to create all kinds of flowcharts from a dataframe in an easy way.

## How to install it?

The package is available on CRAN: https://cran.r-project.org/web/packages/flowchart/index.html.
``` r
install.packages("flowchart")
```
We can download the development version from the github repository:
``` r
# install.packages("remotes")
remotes::install_github('bruigtp/flowchart')
```

## How it works?

The following GIF provides an example of the tidy process of drawing a flowchart for a clinical trial:

<img src = "https://github.com/bruigtp/flowchart/raw/main/data-raw/flowchart_example.gif" alt = "animated" width = "100%">

A detailed overview of the package can be found in the [vignette](https://cran.r-project.org/web/packages/flowchart/vignettes/flowchart.html).

## About

Package: flowchart

Type: Package

Version: 0.1.0

Authors: Pau Satorra, João Carmezim, Natàlia Pallarés, Cristian Tebé.

Maintainer: Pau Satorra

License: MIT + file LICENSE

Encoding: UTF-8

Depends: R (>= 4.1.0)

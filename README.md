
<!-- README.md is generated from README.Rmd. Please edit that file -->

# easyumap

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/easyumap)](https://CRAN.R-project.org/package=easyumap)
<!-- badges: end -->

Easyumap simplifies umap visualisation and production of interactive
visualisations that can be composed and cross-linked with other
visualisations.

## Installation

You can install the development version of easyumap like so:

``` r
if (!require("remotes"))
    install.packages("remotes")

remotes::install_github("selkamand/easyumap")
```

## Quick Start

``` r
library(easyumap)

umap_result <- umap(iris, n_neighbors = 30, seed = 111)
#> ℹ Dropping 1 categorical columns: [Species]
#> ℹ Running UMAP from [4] numeric columns
umap_plot(umap_result, col_colour = "Species")
```

<img src="man/figures/README-example-1.png" width="100%" />

## Acknowledgements

easyumap is powered by the uwot package. If you find easyumap useful
please cite in accordance with `citation("uwot")`

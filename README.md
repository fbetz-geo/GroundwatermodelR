
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GroundwatermodelR

<!-- badges: start -->
<!-- badges: end -->

GroundwatermodelR is a collection of functions to work with
spatial-temporal interpolation of groundwater levels or similar data.
For creating interpolation models, it relies on the machine learning
workaround presented in the [CAST
package](https://hannameyer.github.io/CAST/).

## Installation

You can install the development version of GroundwatermodelR unsing the
following two lines of code:

``` r
library(remotes)
remotes::install_github("fbetz-geo/GroundwatermodelR")
```

## Tutorials
- [Tutorial on accessing existing data](https://fbetz-geo.github.io/GroundwatermodelR/articles/Analysis.html); this tutorial describes the usage of the package for working with existing, locally stored raster time series.

- [Tutorial on making new predictions based on existing model](https://fbetz-geo.github.io/GroundwatermodelR/articles/PredictoR.html); describes the prediction of new time steps based on a previously trained model stored as .rds file. 

## Reference
Betz, F., Fischer, P. (in prep.): Assessing Spatial-Temporal Dynamics of Groundwater in a Restored Floodplain at the German Danube using Machine Learning. In Preparation.

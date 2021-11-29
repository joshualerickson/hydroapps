
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydroapps

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of {hydroapps} is to to create apps for resource management in
the context of hydrology monitoring and reporting. This package relies
heavily on [USGS API](https://www.usgs.gov/products/data-and-tools/apis)
and wouldn’t be possible with out it (much thanks). However, this
package is highly experimental and is used mostly by myself but I think
it’s worth sharing! That being said, Montana is really where I’ve
focused time and energy; however, it wouldn’t take much to add other
states. Please enjoy and contributions are welcome!

## Installation

This is only in development right now.

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joshualerickson/hydroapps")
```

## Contributions

This package uses the [golem](https://github.com/ThinkR-open/golem)
framework and thus uses modules to bring everything together. If you
want to help contribute please be aware of this framework, i.e. see
[here](https://engineering-shiny.org/golem.html) for more details. Other
than that, contributions are welcomed!

## Basic Example

As of right now this only has one app running (streamstats)

``` r
run_app('streamstats')
```

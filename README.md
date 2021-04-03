# Jettel

<!-- badges: start -->
<!--[![CRAN status](https://www.r-pkg.org/badges/version/roxygen2)](https://CRAN.R-project.org/package=roxygen2)-->
[![R build status](https://github.com/dreryos/Jettel/workflows/R-CMD-check/badge.svg)](https://github.com/dreryos/Jettel/actions)
<!--[![codecov](https://codecov.io/gh/dreryos/Jettel/branch/master/graph/badge.svg?token=8SVZ0FOPRJ)](https://codecov.io/gh/dreryos/Jettel)-->
<!-- badges: end -->

Jettel is small package to provide shallow API for Trefle.io

## Installation

<!-- 
```R
# Install devtools from CRAN
install.packages("Jettel")
-->

```R
# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("dreryos/Jettel")
```

<!--
## Usage

The premise of roxygen2 is simple: describe your functions in comments next to their definitions and roxygen2 will process your source code and comments to produce Rd files in the `man/` directory.  Here's a [simple example](https://stringr.tidyverse.org/reference/str_length.html) from the stringr package:

```R
#' The length of a string
#'
#' Technically this returns the number of "code points", in a string. One
#' code point usually corresponds to one character, but not always. For example,
#' an u with a umlaut might be represented as a single character or as the
#' combination a u and an umlaut.
#'
#' @inheritParams str_detect
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @seealso [stringi::stri_length()] which this function wraps.
#' @export
#' @examples
#' str_length(letters)
#' str_length(NA)
#' str_length(factor("abc"))
#' str_length(c("i", "like", "programming", NA))
str_length <- function(string) {
}
```

When you `roxygenise()` (or `devtools::document()`) your package these comments will be automatically transformed to the `.Rd` that R uses to generate the documentation you see when you type `?str_length`. 

## Learn more

To get started, first read `vignette("roxygen2")`. Then read more about the specific package component that you want to generate:

* For `.Rd` documentation files, read `vignette("rd")`.
* For the `NAMESPACE`, read `vignette("namespace")`.
* For the `Collate` field in the `DESCRIPTION`, read `update_collate()`.
-->

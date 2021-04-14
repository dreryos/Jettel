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
devtools::install_github("dreryos/Jettel", dependencies = TRUE)
```

## Usage

`jettel.gui()` starts GUI for Trefle API written in TclTk in R.
Also you have to acquire your own API key at <https://trefle.io/users/sign_up>.

## Screenshot
![mainscreenshot](https://raw.githubusercontent.com/dreryos/Jettel/main/screen.png)

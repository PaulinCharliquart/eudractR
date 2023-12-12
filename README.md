
# eudractR

<!-- badges: start -->
<!-- badges: end -->

The goal of eudractR is package to help you find clinical trials on EUDRACT.

## Installation

You can install the development version of eudractR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PaulinCharliquart/eudractR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(eudractR)

fetch_study("2015-001314-10") # to retrieve clinical trial info by Eudract ID

search("covid") # to search clinical trials about covid
```



# eudractR

The goal of eudractR is package to help you find clinical trials on EUDRACT.

## Installation

You can install the development version of eudractR from [GitHub](https://github.com/PaulinCharliquart/eudractR) with:

``` r
# install.packages("devtools")
devtools::install_github("PaulinCharliquart/eudractR")
```

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(eudractR)

fetch_study("2015-001314-10") # to retrieve clinical trial info by Eudract ID

search_studies("covid") # to search clinical trials about covid
```

## Issues

Report issue [here](https://github.com/PaulinCharliquart/eudract-py/issues).

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[MIT](https://choosealicense.com/licenses/mit/)

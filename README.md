
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Greatrex.Functions

The goal of Greatrex.Functions is to collate functions I find useful or
that underpin many of my scripts. These might be tiny things from stack
overflow answers, little ways to make coding easier, or weird custom
functions that only get used by me in specific ways

## Installation

You can install the development version of Greatrex.Functions like so:

``` r
# GITHUB?
```

## What functions are there?

Let’s just list them

``` r
library(Greatrex.Functions)
ls("package:Greatrex.Functions")
#> [1] "conditionalcreate"     "makedates"             "palettemaker"         
#> [4] "rasterstack_mean_fast"
```

## note to self

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# ktools <img src='man/figures/logo.png' align="right" height="120" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Miscellaneous function, color palettes, collected doing modelling, many
trivial ones. ## Installation

The development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("kklot/ktools")
```

Examples of what inside:

-   `pick`: wrapper of `grep` tp pick element with regex
-   `query_label`: search variable labels on labelled data (e.g.
    `haven::read_dta`)

``` r
# query KAIS surveys
query_label(b1, "cmc")                                                                                                  
    name                              label
1: qhint            Date of interview (CMC)
2:  q102                Date of birth (CMC)
3:  q212 Date of birth for last child (CMC)
4:  q318 Date of birth for last child (CMC)
```

-   `query_name`: search name on a data (e.g. `haven::read_dta`)

``` r
query_name(b2, "weight")                                                                                                
         name label
1: rawiweight      
2: rawbweight      
3:   aiweight      
4:   abweight      
```

-   `allot`: `->` right assign with pipe (**experimental - use
    interactive only**)

``` r
# these two are equivalent
a <- tibble(x = 1)
tibble(x = 1) %>% allot(a)
```

-   `recode_if`: like `case_when` when you want to keep original data
    (`TRUE ~ original`)
-   `rename`: rename a column in data.frame
-   `take_note`: Write a note to file
-   `unkount`: Uncounting data frame using a weights
-   `surv_split` to split survival time for immediate death model
-   `screen_to_file`: write object on screen to a file, e.g. get
    contents of a function
-   `char`: quoting automatic

``` r
> char(a, b, c)
[1] "a" "b" "c"
```

-   `bracket`: put bracket around a text
-   `browse`: Show data frame in browser
-   `cd`: replicate some features of bash cd

``` r
> cd()
Moved from: /Users/knguyen/Code/R/ktools
 to ~
> cd('-')
Moved from: /Users/knguyen
 to /Users/knguyen/Code/R/ktools
```

-   `MakeADFunSafe`: MakeADFun safely terminated if there is a bound
    error
-   `double_logistic`: Double logistic function
-   `findInterval2`: findInterval and return factor with label
-   `fractional_poly`: find best fit fractional polynomial using GLM
-   `kut`: Cut but automatically include min and max data’s value
-   `kompile`: compile TMB with extra `ktools`’s `C++` headers
-   `lsSize`: list objects with size
-   `napply`: n(amed)apply: lapply but automatic add names to output
-   `pfrankCopula`: Frank copulas prob
-   `tmb_fixit`: TMB fix parameters

Let search engines do the work of documenting.

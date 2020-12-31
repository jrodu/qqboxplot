
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qqboxplot

<!-- badges: start -->
<!-- badges: end -->

The qqboxplot package implements q-q boxplots as an extension to
ggplot2.

## Installation

[CRAN](https://CRAN.R-project.org) installation coming soon.

The most recent version of qqboxplot can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jrodu/qqboxplot")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.2     v purrr   0.3.4
#> v tibble  3.0.3     v dplyr   1.0.2
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.5.0
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(scales)
#> 
#> Attaching package: 'scales'
#> The following object is masked from 'package:purrr':
#> 
#>     discard
#> The following object is masked from 'package:readr':
#> 
#>     col_factor
library(qqboxplot)

data <- tibble(y=c(rnorm(1000, mean=2), rt(1000, 16), rt(500, 4), rt(1000, 8), rt(1000, 32)), group=c(rep("normal, mean=2", 1000), rep("t distribution, df=16", 1000), rep("t distribution, df=4", 500), rep("t distribution, df=8", 1000), rep("t distribution, df=32", 1000)))

data %>%
ggplot(aes(factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")), y=y)) +
geom_qqboxplot(notch=TRUE, varwidth = TRUE, reference_dist="norm") +
xlab("reference: normal distribution") +
ylab(NULL) +
guides(color=FALSE) +
theme(axis.text.x = element_text(angle = 23, size = 15), axis.title.y = element_text(size=15),
axis.title.x = element_text(size=15),
panel.border = element_blank(), panel.background = element_rect(fill="white"), panel.grid.major = element_line(colour = "grey70"), panel.grid.minor = element_line(colour="grey80"))
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

## basic example code
```

# shinyrecipes <img src='man/figures/logo.png' align="right" height="139"/>

> The objective of this package is to facilitate the preprocessing tasks offered by the [{recipes}](https://tidymodels.github.io/recipes/) package in an interactive way.

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build status](https://travis-ci.org/AlbertoAlmuinha/shinyrecipes.svg?branch=master)](https://travis-ci.org/AlbertoAlmuinha/shinyrecipes)
<!-- badges: end -->


`shinyrecipes` allows you to carry out preprocessing tasks in a simple and intuitive way. Create your recipe, visualize your data to decide what steps to use, add these steps, prepare your recipe and use it on new data without writing a single line of code.

Find more information in the [shinyrecipes](https://albertoalmuinha.github.io/shinyrecipes/) web page.

## Installation

You can install the development version from Github:

```r
remotes::install_github("AlbertoAlmuinha/shinyrecipes")
```

## Example

In this example we can see a simple case in which we impute the missing values of the variable 'Ozone' with the knn algorithm.

![](man/figures/shinyrecipes.gif)

## Getting Help

If you encounter a bug, please file an issue with a minimal reproducible example on [Issues](https://github.com/AlbertoAlmuinha/shinyrecipes/issues). You can also make an inquiry if you wish.




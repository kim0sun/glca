
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `glca`: An **R** Package for Multiple-Group Latent Class Analysis <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/glca)](https://CRAN.R-project.org/package=glca)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/glca?color=blue)](https://r-pkg.org/pkg/glca)
[![R-CMD-check](https://github.com/kim0sun/glca/workflows/R-CMD-check/badge.svg)](https://github.com/kim0sun/glca/actions)
<!-- badges: end -->

Fits multiple-group latent class analysis (LCA) for exploring
differences between populations in the data with a multilevel structure.
There are two approaches to reflect group differences in glca:
fixed-effect LCA (Bandeen-Roche et al, 1997
<doi:10.1080/01621459.1997.10473658>; Clogg and Goodman, 1985
<doi:10.2307/270847>) and nonparametric random-effect LCA (Vermunt, 2003
<doi:10.1111/j.0081-1750.2003.t01-1-00131.x>).

## Introduction

Latent class analysis (LCA) is one of the most popular discrete mixture
models for classifying individuals based on their responses to multiple
manifest items. When there are existing subgroups in the data
representing different populations, researchers are often interested in
comparing certain aspects of latent class structure across these groups
in LCA approach. In multiple-group LCA models, individuals are dependent
owing to multilevel data structure, where observation units (i.e.,
individuals) are nested within a higher-level unit (i.e., group). This
paper describes the implementation of multiple-group LCA in the **R**
package `glca` for exploring differences in latent class structure
between populations, taking multilevel data structure into account. The
package `glca` deals with the fixed effect LCA and the random effect
LCA; the former can be applied in the situation where populations are
segmented by the observed group variable itself, whereas the latter can
be used when there are too many levels in the group variable to make a
meaningful group comparisons.

## Installation

You can install the released version of glca from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("glca")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kim0sun/glca")
```

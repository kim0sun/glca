
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glca

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/glca)](https://CRAN.R-project.org/package=glca)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/glca?color=blue)](https://r-pkg.org/pkg/glca)
[![Codecov test
coverage](https://codecov.io/gh/kim0sun/glca/branch/master/graph/badge.svg)](https://codecov.io/gh/kim0sun/glca?branch=master)
[![R build
status](https://github.com/kim0sun/glca/workflows/R-CMD-check/badge.svg)](https://github.com/kim0sun/glca/actions)
[![Travis build
status](https://travis-ci.com/kim0sun/glca.svg?branch=master)](https://travis-ci.com/kim0sun/glca)
<!-- badges: end -->

Fits latent class analysis (LCA) including group variable and
covariates. The group variable can be handled either by multilevel LCA
described in Vermunt (2003) <DOI:10.1111/j.0081-1750.2003.t01-1-00131.x>
or standard LCA at each level of group variable. The covariates can be
incorporated in the form of logistic regression (Bandeen-Roche et al.
(1997) <DOI:10.1080/01621459.1997.10473658>).

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

## Example

``` r
library(glca)
data("gss08")
# LCA
lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
            data = gss08, nclass = 3, verbose = FALSE)
summary(lca)
#> 
#> Call:
#> glca(formula = item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 
#>     1, data = gss08, nclass = 3, verbose = FALSE)
#> 
#> Manifest items :
#>  DEFECT HLTH RAPE POOR SINGLE NOMORE 
#> 
#> Model : Standard LCA 
#> Number of latent classes : 3 
#> 
#> Mean Prevalence for latent classes for each group:
#>     Class 1 Class 2 Class 3
#> ALL 0.34467 0.46396 0.19138
#> 
#> Number of parameters : 20 
#> 
#> log-likelihood : -687.4486 
#>      G-squared : 29.82695 
#>            AIC : 1414.897 
#>            BIC : 1492.17
#> 
#> Response numbering:
#>        Y = 1 Y = 2
#> DEFECT   YES    NO
#> HLTH     YES    NO
#> RAPE     YES    NO
#> POOR     YES    NO
#> SINGLE   YES    NO
#> NOMORE   YES    NO
#> 
#> Estimated model parameters :
#> Gamma :
#>     Class 1 Class 2 Class 3
#> ALL  0.3447   0.464  0.1914
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.8275 0.9453 0.7960 0.0638 0.0390 0.1344
#> Class 2 1.0000 1.0000 1.0000 0.9813 0.9284 0.9657
#> Class 3 0.0466 0.3684 0.0949 0.0000 0.0000 0.0000
#> Rho (Y = 2) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.1725 0.0547 0.2040 0.9362 0.9610 0.8656
#> Class 2 0.0000 0.0000 0.0000 0.0187 0.0716 0.0343
#> Class 3 0.9534 0.6316 0.9051 1.0000 1.0000 1.0000
plot(lca)
```

<img src="figures/README-example-1.png" width="49%" /><img src="figures/README-example-2.png" width="49%" />

``` r

# Multiple-group LCA (MGLCA)
mglca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
             group = DEGREE, data = gss08, nclass = 3, verbose = FALSE)
plot(mglca)
```

<img src="figures/README-example-3.png" width="49%" /><img src="figures/README-example-4.png" width="49%" />

``` r

# Multiple-group LCA with covariate(s) (MGLCR)
mglcr = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ SEX,
             group = DEGREE, data = gss08, nclass = 3, verbose = FALSE)
coef(mglcr)
#> Coefficients :
#> 
#> Class 1 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SEXFEMALE    3.04651     1.11400     0.09062    12.29    <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)
#> SEXFEMALE    1.08693     0.08335     0.06815    1.223     0.222

data("nyts18")
# Multilevel LCA (MLCA)
mlca = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 1, group = SCH_ID, 
            data = nyts18, nclass = 3, ncluster = 2, verbose = FALSE)
summary(mlca)
#> 
#> Call:
#> glca(formula = item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 
#>     1, group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2, 
#>     verbose = FALSE)
#> 
#> Manifest items :
#>  ECIGT ECIGAR ESLT EELCIGT EHOOKAH 
#> Grouping variable : SCH_ID 
#> 
#> Model : Multilevel LCA 
#> Number of latent classes : 3 
#> Number of latent clusters : 2 
#> 
#> Mean Prevalence for latent clusters:
#> Cluster 1 Cluster 2 
#>    0.3793    0.6207 
#> 
#> Mean Prevalence for latent classes:
#>           Class 1 Class 2 Class 3
#> Cluster 1 0.14137 0.34686 0.51177
#> Cluster 2 0.00876 0.06129 0.92995
#> 
#> 
#> Number of parameters : 20 
#> Number of groups : 45 
#> 
#> log-likelihood : -1955.487 
#>      G-squared : 768.5035 
#>            AIC : 3950.973 
#>            BIC : 4060.137
#> 
#> Response numbering:
#>         Y = 1 Y = 2
#> ECIGT     Yes    No
#> ECIGAR    Yes    No
#> ESLT      Yes    No
#> EELCIGT   Yes    No
#> EHOOKAH   Yes    No
#> 
#> Estimated model parameters :
#> Delta :
#> Cluster 1 Cluster 2 
#>    0.3793    0.6207 
#> 
#> Gamma :
#>           Class 1 Class 2 Class 3
#> Cluster 1  0.1414  0.3469  0.5118
#> Cluster 2  0.0088  0.0613  0.9300
#> 
#> Rho (Y = 1) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.9112 0.9750 0.5651  0.9778  0.5364
#> Class 2 0.3489 0.2006 0.1236  0.7783  0.0443
#> Class 3 0.0062 0.0043 0.0088  0.0413  0.0057
#> Rho (Y = 2) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.0888 0.0250 0.4349  0.0222  0.4636
#> Class 2 0.6511 0.7994 0.8764  0.2217  0.9557
#> Class 3 0.9938 0.9957 0.9912  0.9587  0.9943
plot(mlca)
```

<img src="figures/README-example-5.png" width="49%" /><img src="figures/README-example-6.png" width="49%" />

``` r

# MLCA with covariate(s) (MLCR)
# (SEX: level-1 covariate, SCH_LEV: level-2 covariate)
mlcr = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ SEX + SCH_LEV, group = SCH_ID,
            data = nyts18, nclass = 3, ncluster = 2, verbose = FALSE)
coef(mlcr)
#> 
#> Level 1 Coefficients :
#> 
#> Class 1 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)   
#> SEXFemale     0.5322     -0.6307      0.2189   -2.881   0.00402 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)  
#> SEXFemale     0.5976     -0.5148      0.2180   -2.362    0.0183 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Level 2 Coefficients :
#> 
#> Class 1 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SCH_LEVHigh School    13.0080      2.5656      0.5924     4.33  1.57e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)
#> SCH_LEVHigh School     1.8467      0.6134      0.6587    0.931     0.352
```

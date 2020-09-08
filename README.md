
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glca

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/glca)](http://cran.r-project.org/web/packages/glca)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/glca?color=blue)](https://r-pkg.org/pkg/glca)

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
#> ALL 0.46396 0.34467 0.19137
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
#> ALL   0.464  0.3447  0.1914
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 1.0000 1.0000 1.0000 0.9813 0.9284 0.9657
#> Class 2 0.8275 0.9453 0.7959 0.0638 0.0390 0.1344
#> Class 3 0.0466 0.3684 0.0949 0.0000 0.0000 0.0000
#> Rho (Y = 2) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.0000 0.0000 0.0000 0.0187 0.0716 0.0343
#> Class 2 0.1725 0.0547 0.2041 0.9362 0.9610 0.8656
#> Class 3 0.9534 0.6316 0.9051 1.0000 1.0000 1.0000
plot(lca)
```

<img src="figures/README-example-1.png" width="49%" /><img src="figures/README-example-2.png" width="49%" />

``` r

# Multiple-group LCA (MGLCA)
mglca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
             group = DEGREE, data = gss08, nclass = 3, verbose = FALSE)
summary(mglca)
#> 
#> Call:
#> glca(formula = item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 
#>     1, group = DEGREE, data = gss08, nclass = 3, verbose = FALSE)
#> 
#> Manifest items :
#>  DEFECT HLTH RAPE POOR SINGLE NOMORE 
#> Grouping variable : DEGREE 
#> 
#> Model : Multigroup LCA 
#> Number of latent classes : 3 
#> 
#> Mean Prevalence for latent classes for each group:
#>             Class 1 Class 2 Class 3
#> <= HS       0.31196 0.16850 0.51954
#> HIGH SCHOOL 0.20745 0.44393 0.34862
#> COLLEGE     0.14806 0.55348 0.29846
#> GRADUATE    0.08375 0.71186 0.20439
#> 
#> Number of parameters : 26 
#> Number of groups : 4 
#> 
#> log-likelihood : -672.4138 
#>      G-squared : 87.85135 
#>            AIC : 1396.828 
#>            BIC : 1497.282
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
#>             Class 1 Class 2 Class 3
#> <= HS        0.3120  0.1685  0.5195
#> HIGH SCHOOL  0.2075  0.4439  0.3486
#> COLLEGE      0.1481  0.5535  0.2985
#> GRADUATE     0.0837  0.7119  0.2044
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.0507 0.3724 0.0995 0.0000 0.0000 0.0000
#> Class 2 1.0000 1.0000 1.0000 0.9835 0.9309 0.9681
#> Class 3 0.8318 0.9470 0.7999 0.0690 0.0433 0.1389
#> Rho (Y = 2) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.9493 0.6276 0.9005 1.0000 1.0000 1.0000
#> Class 2 0.0000 0.0000 0.0000 0.0165 0.0691 0.0319
#> Class 3 0.1682 0.0530 0.2001 0.9310 0.9567 0.8611
plot(mglca)
```

<img src="figures/README-example-3.png" width="49%" /><img src="figures/README-example-4.png" width="49%" />

``` r

# Multiple-group LCA with covariate(s) (MGLCR)
mglcr = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ SEX,
             group = DEGREE, data = gss08, nclass = 3, verbose = FALSE)
summary(mglcr)
#> 
#> Call:
#> glca(formula = item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 
#>     SEX, group = DEGREE, data = gss08, nclass = 3, verbose = FALSE)
#> 
#> Manifest items :
#>  DEFECT HLTH RAPE POOR SINGLE NOMORE 
#> Grouping variable : DEGREE 
#> Covariates : 
#>  SEX 
#> 
#> Model : Multigroup LCA with Covariates 
#> Number of latent classes : 3 
#> 
#> Mean Prevalence for latent classes for each group:
#>             Class 1 Class 2 Class 3
#> <= HS       0.32143 0.16848 0.51010
#> HIGH SCHOOL 0.21275 0.44386 0.34339
#> COLLEGE     0.15036 0.55347 0.29616
#> GRADUATE    0.08580 0.71183 0.20237
#> 
#> Number of parameters : 28 
#> Number of groups : 4 
#> 
#> log-likelihood : -666.7097 
#>      G-squared : 149.9656 
#>            AIC : 1389.419 
#>            BIC : 1497.601
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
#> Beta :
#> Intercepts :
#>                    Class 1/3 Class 2/3
#> Group :<= HS         -1.1445   -1.0634
#> Group :HIGH SCHOOL   -1.1261    0.2979
#> Group :COLLEGE       -1.3487    0.6686
#> Group :GRADUATE      -1.3620    1.2872
#> 
#> Coefficients :
#> Class 1/3 Class 2/3 
#>    1.0306   -0.0834 
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.0649 0.3825 0.0989 0.0000 0.0000 0.0000
#> Class 2 1.0000 1.0000 1.0000 0.9836 0.9309 0.9682
#> Class 3 0.8342 0.9488 0.8086 0.0700 0.0440 0.1409
#> Rho (Y = 2) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.9351 0.6175 0.9011 1.0000 1.0000 1.0000
#> Class 2 0.0000 0.0000 0.0000 0.0164 0.0691 0.0318
#> Class 3 0.1658 0.0512 0.1914 0.9300 0.9560 0.8591
coef(mglcr)
#> Coefficients :
#> 
#> Class 1 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SEXFEMALE    2.80287     1.03064     0.09971    10.34    <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)
#> SEXFEMALE    0.92003    -0.08335     0.06815   -1.223     0.222

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
#>   0.62012   0.37988 
#> 
#> Mean Prevalence for latent classes:
#>           Class 1 Class 2 Class 3
#> Cluster 1 0.92994 0.00877 0.06129
#> Cluster 2 0.50889 0.14125 0.34985
#> 
#> 
#> Number of parameters : 20 
#> Number of groups : 45 
#> 
#> log-likelihood : -1962.45 
#>      G-squared : 773.581 
#>            AIC : 3964.9 
#>            BIC : 4074.167
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
#>    0.6201    0.3799 
#> 
#> Gamma :
#>           Class 1 Class 2 Class 3
#> Cluster 1  0.9299  0.0088  0.0613
#> Cluster 2  0.5089  0.1413  0.3499
#> 
#> Rho (Y = 1) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.0062 0.0043 0.0086  0.0405  0.0057
#> Class 2 0.9120 0.9755 0.5621  0.9776  0.5345
#> Class 3 0.3436 0.1970 0.1265  0.7780  0.0439
#> Rho (Y = 2) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.9938 0.9957 0.9914  0.9595  0.9943
#> Class 2 0.0880 0.0245 0.4379  0.0224  0.4655
#> Class 3 0.6564 0.8030 0.8735  0.2220  0.9561
plot(mlca)
```

<img src="figures/README-example-5.png" width="49%" /><img src="figures/README-example-6.png" width="49%" />

``` r

# MLCA with covariate(s) (MLCR)
# (SEX: level-1 covariate, SCH_LEV: level-2 covariate)
mlcr = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ SEX + SCH_LEV, group = SCH_ID,
            data = nyts18, nclass = 3, ncluster = 2, verbose = FALSE)
summary(mlcr)
#> 
#> Call:
#> glca(formula = item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 
#>     SEX + SCH_LEV, group = SCH_ID, data = nyts18, nclass = 3, 
#>     ncluster = 2, verbose = FALSE)
#> 
#> Manifest items :
#>  ECIGT ECIGAR ESLT EELCIGT EHOOKAH 
#> Grouping variable : SCH_ID 
#> Covariates (Level 2) : 
#>  SCH_LEV 
#> Covariates (Level 1) : 
#>  SEX 
#> 
#> Model : Multilevel LCA with Covariates 
#> Number of latent classes : 3 
#> Number of latent clusters : 2 
#> 
#> Mean Prevalence for latent clusters:
#> Cluster 1 Cluster 2 
#>   0.54083   0.45917 
#> 
#> Mean Prevalence for latent classes:
#>           Class 1 Class 2 Class 3
#> Cluster 1  0.0199 0.08978 0.89032
#> Cluster 2  0.1175 0.29044 0.59206
#> 
#> 
#> Number of parameters : 24 
#> Number of groups : 45 
#> 
#> log-likelihood : -1919.937 
#>      G-squared : 1052.541 
#>            AIC : 3887.874 
#>            BIC : 4018.87
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
#>    0.5408    0.4592 
#> 
#> Beta (level 1) :
#> $Cluster1
#>             Class 1/3 Class 2/3
#> (Intercept)   -2.6345   -1.0654
#> SEXFemale      0.6307    0.1159
#> 
#> $Cluster2
#>             Class 1/3 Class 2/3
#> (Intercept)   -0.8997    0.1671
#> SEXFemale      0.6307    0.1159
#> 
#> Beta (level 2) :
#>                    Class 1/3 Class 2/3
#> SCH_LEVHigh School   -2.5656   -1.9522
#> 
#> Rho (Y = 1) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.8914 0.9657 0.5507  0.9782  0.5049
#> Class 2 0.3227 0.1696 0.1127  0.7266  0.0394
#> Class 3 0.0033 0.0034 0.0074  0.0372  0.0056
#> Rho (Y = 2) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.1086 0.0343 0.4493  0.0218  0.4951
#> Class 2 0.6773 0.8304 0.8873  0.2734  0.9606
#> Class 3 0.9967 0.9966 0.9926  0.9628  0.9944
coef(mlcr)
#> 
#> Level 1 Coefficients :
#> 
#> Class 1 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)   
#> SEXFemale     1.8788      0.6307      0.2189    2.881   0.00402 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)
#> SEXFemale     1.1228      0.1159      0.1716    0.675       0.5
#> 
#> 
#> Level 2 Coefficients :
#> 
#> Class 1 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SCH_LEVHigh School    0.07687    -2.56558     0.59246    -4.33  1.57e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SCH_LEVHigh School     0.1420     -1.9522      0.4185   -4.665  3.33e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

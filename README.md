
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glca

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/glca)](http://cran.r-project.org/web/packages/glca)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/glca?color=blue)](https://r-pkg.org/pkg/glca)
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
#> ALL 0.19138 0.46396 0.34467
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
#> ALL  0.1914   0.464  0.3447
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.0466 0.3684 0.0949 0.0000 0.0000 0.0000
#> Class 2 1.0000 1.0000 1.0000 0.9813 0.9284 0.9657
#> Class 3 0.8275 0.9453 0.7960 0.0638 0.0390 0.1344
#> Rho (Y = 2) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.9534 0.6316 0.9051 1.0000 1.0000 1.0000
#> Class 2 0.0000 0.0000 0.0000 0.0187 0.0716 0.0343
#> Class 3 0.1725 0.0547 0.2040 0.9362 0.9610 0.8656
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
#> <= HS       0.16850 0.51954 0.31196
#> HIGH SCHOOL 0.44393 0.34862 0.20745
#> COLLEGE     0.55348 0.29846 0.14806
#> GRADUATE    0.71186 0.20439 0.08375
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
#> <= HS        0.1685  0.5195  0.3120
#> HIGH SCHOOL  0.4439  0.3486  0.2075
#> COLLEGE      0.5535  0.2985  0.1481
#> GRADUATE     0.7119  0.2044  0.0837
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 1.0000 1.0000 1.0000 0.9835 0.9309 0.9681
#> Class 2 0.8318 0.9470 0.7999 0.0690 0.0433 0.1389
#> Class 3 0.0507 0.3724 0.0995 0.0000 0.0000 0.0000
#> Rho (Y = 2) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.0000 0.0000 0.0000 0.0165 0.0691 0.0319
#> Class 2 0.1682 0.0530 0.2001 0.9310 0.9567 0.8611
#> Class 3 0.9493 0.6276 0.9005 1.0000 1.0000 1.0000
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
#>    0.3793    0.6207 
#> 
#> Mean Prevalence for latent classes:
#>           Class 1 Class 2 Class 3
#> Cluster 1 0.34686 0.14137 0.51177
#> Cluster 2 0.06129 0.00876 0.92995
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
#> Cluster 1  0.3469  0.1414  0.5118
#> Cluster 2  0.0613  0.0088  0.9300
#> 
#> Rho (Y = 1) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.3489 0.2006 0.1236  0.7783  0.0443
#> Class 2 0.9112 0.9750 0.5651  0.9778  0.5364
#> Class 3 0.0062 0.0043 0.0088  0.0413  0.0057
#> Rho (Y = 2) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.6511 0.7994 0.8764  0.2217  0.9557
#> Class 2 0.0888 0.0250 0.4349  0.0222  0.4636
#> Class 3 0.9938 0.9957 0.9912  0.9587  0.9943
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
#> Cluster 1 0.89032  0.0199 0.08979
#> Cluster 2 0.59206  0.1175 0.29045
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
#> (Intercept)    1.0654   -1.5691
#> SEXFemale     -0.1159    0.5148
#> 
#> $Cluster2
#>             Class 1/3 Class 2/3
#> (Intercept)   -0.1671   -1.0668
#> SEXFemale     -0.1159    0.5148
#> 
#> Beta (level 2) :
#>                    Class 1/3 Class 2/3
#> SCH_LEVHigh School    1.9522   -0.6134
#> 
#> Rho (Y = 1) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.0033 0.0034 0.0074  0.0372  0.0056
#> Class 2 0.8914 0.9657 0.5507  0.9782  0.5049
#> Class 3 0.3227 0.1696 0.1127  0.7266  0.0394
#> Rho (Y = 2) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.9967 0.9966 0.9926  0.9628  0.9944
#> Class 2 0.1086 0.0343 0.4493  0.0218  0.4951
#> Class 3 0.6773 0.8304 0.8873  0.2734  0.9606
coef(mlcr)
#> 
#> Level 1 Coefficients :
#> 
#> Class 1 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)
#> SEXFemale     0.8906     -0.1159      0.1716   -0.675       0.5
#> 
#> Class 2 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)  
#> SEXFemale     1.6733      0.5148      0.2180    2.362    0.0183 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Level 2 Coefficients :
#> 
#> Class 1 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SCH_LEVHigh School     7.0438      1.9522      0.4185    4.665  3.33e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)
#> SCH_LEVHigh School     0.5415     -0.6134      0.6587   -0.931     0.352
```

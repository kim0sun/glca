
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glca

<!-- badges: start -->

[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/glca?color=blue)](https://r-pkg.org/pkg/glca)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/glca?color=orange)](https://r-pkg.org/pkg/glca)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-week/glca?color=red)](https://r-pkg.org/pkg/glca)

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
data("gss12")
# LCA
lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
            data = gss12, nclass = 3, verbose = FALSE)
summary(lca)
#> 
#> Call:
#> glca(formula = item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 
#>     1, data = gss12, nclass = 3, verbose = FALSE)
#> 
#> Model : Standard LCA 
#> Number of latent classes : 3 
#> 
#> Mean Prevalence for latent classes for each group:
#>     Class 1 Class 2 Class 3
#> ALL 0.36303 0.19471 0.44226
#> 
#> Number of parameters : 20 
#> 
#> log-likelihood : -1349.348 
#>      G-squared : 52.53695 
#>            AIC : 2738.697 
#>            BIC : 2828.693
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
#> ALL   0.363  0.1947  0.4423
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.8281 0.9782 0.8522 0.1115 0.0514 0.0985
#> Class 2 0.0626 0.4481 0.0622 0.0000 0.0000 0.0157
#> Class 3 0.9845 1.0000 0.9901 0.9431 0.9394 0.9984
plot(lca)
```

<img src="figures/README-example-1.png" width="100%" /><img src="figures/README-example-2.png" width="100%" />

``` r

# Multiple-group LCA (MGLCA)
mglca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
             group = DEGREE, data = gss12, nclass = 3, verbose = FALSE)
summary(mglca)
#> 
#> Call:
#> glca(formula = item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 
#>     1, group = DEGREE, data = gss12, nclass = 3, verbose = FALSE)
#> 
#> Model : Multigroup LCA 
#> Number of latent classes : 3 
#> 
#> Mean Prevalence for latent classes for each group:
#>             Class 1 Class 2 Class 3
#> <= HS       0.27011 0.42510 0.30479
#> HIGH SCHOOL 0.38661 0.40865 0.20474
#> COLLEGE     0.51168 0.33797 0.15035
#> GRADUATE    0.74073 0.10239 0.15689
#> 
#> Number of parameters : 26 
#> Number of groups : 4 
#> 
#> log-likelihood : -1324.764 
#>      G-squared : 132.2912 
#>            AIC : 2701.527 
#>            BIC : 2818.522
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
#> <= HS        0.2701  0.4251  0.3048
#> HIGH SCHOOL  0.3866  0.4086  0.2047
#> COLLEGE      0.5117  0.3380  0.1503
#> GRADUATE     0.7407  0.1024  0.1569
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.9848 1.0000 0.9902 0.9451 0.9417 0.9978
#> Class 2 0.8348 0.9792 0.8592 0.1140 0.0527 0.1015
#> Class 3 0.0715 0.4613 0.0699 0.0000 0.0000 0.0175
plot(mglca)
```

<img src="figures/README-example-3.png" width="100%" /><img src="figures/README-example-4.png" width="100%" />

``` r

# Multiple-group LCA with covariate(s) (MGLCR)
mglcr = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ SEX,
             group = DEGREE, data = gss12, nclass = 3, verbose = FALSE)
summary(mglcr)
#> 
#> Call:
#> glca(formula = item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 
#>     SEX, group = DEGREE, data = gss12, nclass = 3, verbose = FALSE)
#> 
#> Model : Multigroup LCA with Covariates 
#> Number of latent classes : 3 
#> 
#> Mean Prevalence for latent classes for each group:
#>             Class 1 Class 2 Class 3
#> <= HS       0.31364 0.26926 0.41709
#> HIGH SCHOOL 0.21045 0.38580 0.40375
#> COLLEGE     0.15513 0.51154 0.33333
#> GRADUATE    0.16203 0.74040 0.09758
#> 
#> Number of parameters : 28 
#> Number of groups : 4 
#> 
#> log-likelihood : -1321.795 
#>      G-squared : 194.9587 
#>            AIC : 2699.589 
#>            BIC : 2825.583
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
#> Group :<= HS         -0.6159   -0.5249
#> Group :HIGH SCHOOL   -1.0117   -0.1416
#> Group :COLLEGE       -1.1454    0.3260
#> Group :GRADUATE       0.1707    1.9376
#> 
#> Coefficients :
#> Class 1/3 Class 2/3 
#>    0.6067    0.1774 
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.0803 0.4714 0.0793 0.0000 0.0000 0.0190
#> Class 2 0.9848 1.0000 0.9902 0.9457 0.9422 0.9981
#> Class 3 0.8424 0.9811 0.8662 0.1164 0.0544 0.1030
coef(mglcr)
#> Intercept :
#> 
#> Group <= HS:
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)  
#> Class 1 / 3     0.5401     -0.6159      0.3133   -1.966    0.0499 *
#> Class 2 / 3     0.5916     -0.5249      0.2798   -1.876    0.0613 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group HIGH SCHOOL:
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3     0.3636     -1.0117      0.2561   -3.950  8.97e-05 ***
#> Class 2 / 3     0.8680     -0.1416      0.1713   -0.827     0.409    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group COLLEGE:
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3     0.3181     -1.1454      0.3158   -3.627  0.000317 ***
#> Class 2 / 3     1.3854      0.3260      0.2061    1.582  0.114300    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group GRADUATE:
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3     1.1861      0.1707      0.6251    0.273     0.785    
#> Class 2 / 3     6.9418      1.9376      0.4819    4.021  6.73e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Coefficients :
#> 
#> Class 1 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SEXFEMALE    1.83434     0.60668     0.06391    9.493    <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SEXFEMALE    1.19411     0.17740     0.04866    3.646  0.000296 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
#> Cluster 1 0.06128 0.00877 0.92995
#> Cluster 2 0.34985 0.14125 0.50891
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
#> Cluster 1  0.0613  0.0088  0.9299
#> Cluster 2  0.3498  0.1412  0.5089
#> 
#> Rho (Y = 1) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.3436 0.1970 0.1265  0.7780  0.0439
#> Class 2 0.9120 0.9755 0.5621  0.9776  0.5345
#> Class 3 0.0062 0.0043 0.0086  0.0405  0.0057
plot(mlca)
```

<img src="figures/README-example-5.png" width="100%" /><img src="figures/README-example-6.png" width="100%" />

``` r

# MLCA with covariate(s) (MLCR)
# (HOME: level-1 covariate, School_lev: level-2 covariate)
mlcr = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ SCH_LEV, group = SCH_ID,
            data = nyts18, nclass = 3, ncluster = 2, verbose = FALSE)
summary(mlcr)
#> 
#> Call:
#> glca(formula = item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 
#>     SCH_LEV, group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2, 
#>     verbose = FALSE)
#> 
#> Model : Multilevel LCA with Covariates 
#> Number of latent classes : 3 
#> Number of latent clusters : 2 
#> 
#> Mean Prevalence for latent clusters:
#> Cluster 1 Cluster 2 
#>   0.44637   0.55363 
#> 
#> Mean Prevalence for latent classes:
#>           Class 1 Class 2 Class 3
#> Cluster 1 0.11856 0.29388 0.58756
#> Cluster 2 0.02161 0.09574 0.88265
#> 
#> 
#> Number of parameters : 22 
#> Number of groups : 45 
#> 
#> log-likelihood : -1930.752 
#>      G-squared : 710.2738 
#>            AIC : 3905.503 
#>            BIC : 4025.634
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
#>    0.4464    0.5536 
#> 
#> Beta (level 1) :
#> $Cluster1
#>                    Class 1/3 Class 2/3
#> (Intercept)          -3.1480   -1.6793
#> SCH_LEVHigh School    2.6465    1.9399
#> 
#> $Cluster2
#>                    Class 1/3 Class 2/3
#> (Intercept)          -4.8628   -2.9073
#> SCH_LEVHigh School    2.6465    1.9399
#> 
#> 
#> Rho (Y = 1) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.8959 0.9690 0.5382  0.9779  0.5014
#> Class 2 0.3144 0.1631 0.1187  0.7292  0.0388
#> Class 3 0.0034 0.0037 0.0072  0.0352  0.0056
coef(mlcr)
#> Intercept :
#> 
#> Cluster 1 :
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3    0.04294    -3.14800     0.41903   -7.513  1.01e-13 ***
#> Class 2 / 3    0.18650    -1.67930     0.39240   -4.280  2.00e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Cluster 2 :
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3   0.007729   -4.862820    0.754814   -6.442  1.60e-10 ***
#> Class 2 / 3   0.054621   -2.907331    0.466160   -6.237  5.85e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Level 1 Coefficients :
#> 
#> Class 1 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SCH_LEVHigh School    14.1039      2.6465      0.3061    8.646    <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SCH_LEVHigh School     6.9577      1.9399      0.2192     8.85    <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

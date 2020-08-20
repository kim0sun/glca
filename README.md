
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
#> ALL 0.36303 0.44226  0.1947
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
#> ALL   0.363  0.4423  0.1947
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.8281 0.9782 0.8522 0.1114 0.0514 0.0985
#> Class 2 0.9845 1.0000 0.9901 0.9431 0.9394 0.9984
#> Class 3 0.0626 0.4481 0.0622 0.0000 0.0000 0.0157
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
#> <= HS       0.42511 0.27011 0.30478
#> HIGH SCHOOL 0.40865 0.38661 0.20473
#> COLLEGE     0.33797 0.51168 0.15034
#> GRADUATE    0.10239 0.74073 0.15688
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
#> <= HS        0.4251  0.2701  0.3048
#> HIGH SCHOOL  0.4087  0.3866  0.2047
#> COLLEGE      0.3380  0.5117  0.1503
#> GRADUATE     0.1024  0.7407  0.1569
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.8348 0.9792 0.8592 0.1139 0.0527 0.1015
#> Class 2 0.9848 1.0000 0.9902 0.9451 0.9417 0.9978
#> Class 3 0.0715 0.4612 0.0699 0.0000 0.0000 0.0175
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
#> <= HS       0.41709 0.26927 0.31364
#> HIGH SCHOOL 0.40375 0.38580 0.21045
#> COLLEGE     0.33333 0.51154 0.15513
#> GRADUATE    0.09758 0.74040 0.16202
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
#> Group :<= HS          0.6159    0.0910
#> Group :HIGH SCHOOL    1.0117    0.8701
#> Group :COLLEGE        1.1454    1.4714
#> Group :GRADUATE      -0.1707    1.7669
#> 
#> Coefficients :
#> Class 1/3 Class 2/3 
#>   -0.6067   -0.4293 
#> 
#> Rho (Y = 1) :
#>         DEFECT   HLTH   RAPE   POOR SINGLE NOMORE
#> Class 1 0.8424 0.9811 0.8662 0.1164 0.0544 0.1030
#> Class 2 0.9848 1.0000 0.9902 0.9457 0.9422 0.9981
#> Class 3 0.0803 0.4714 0.0792 0.0000 0.0000 0.0190
coef(mglcr)
#> Intercept :
#> 
#> Group <= HS:
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)  
#> Class 1 / 3    1.85141     0.61595     0.31334    1.966    0.0499 *
#> Class 2 / 3    1.09528     0.09101     0.30770    0.296    0.7675  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group HIGH SCHOOL:
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3     2.7502      1.0117      0.2561     3.95  8.97e-05 ***
#> Class 2 / 3     2.3872      0.8701      0.2437     3.57  0.000392 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group COLLEGE:
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3     3.1436      1.1454      0.3158    3.627  0.000317 ***
#> Class 2 / 3     4.3552      1.4714      0.2901    5.073   5.6e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group GRADUATE:
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3     0.8431     -0.1707      0.6251   -0.273     0.785    
#> Class 2 / 3     5.8526      1.7669      0.3932    4.493   8.8e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Coefficients :
#> 
#> Class 1 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SEXFEMALE    0.54516    -0.60668     0.06391   -9.493    <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>           Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SEXFEMALE    0.65098    -0.42928     0.05974   -7.186  2.55e-12 ***
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
#> Cluster 1 0.92994 0.06129 0.00877
#> Cluster 2 0.50889 0.34985 0.14126
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
#> Cluster 1  0.9299  0.0613  0.0088
#> Cluster 2  0.5089  0.3499  0.1413
#> 
#> Rho (Y = 1) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.0062 0.0043 0.0086  0.0405  0.0057
#> Class 2 0.3436 0.1970 0.1265  0.7780  0.0439
#> Class 3 0.9120 0.9755 0.5621  0.9776  0.5345
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
#> Cluster 1 0.58756 0.11856 0.29388
#> Cluster 2 0.88265 0.02161 0.09574
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
#> (Intercept)           1.6793   -1.4687
#> SCH_LEVHigh School   -1.9399    0.7066
#> 
#> $Cluster2
#>                    Class 1/3 Class 2/3
#> (Intercept)           2.9073   -1.9555
#> SCH_LEVHigh School   -1.9399    0.7066
#> 
#> 
#> Rho (Y = 1) :
#>          ECIGT ECIGAR   ESLT EELCIGT EHOOKAH
#> Class 1 0.0034 0.0037 0.0072  0.0352  0.0056
#> Class 2 0.8959 0.9690 0.5382  0.9779  0.5014
#> Class 3 0.3144 0.1631 0.1187  0.7292  0.0388
coef(mlcr)
#> Intercept :
#> 
#> Cluster 1 :
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3     5.3618      1.6793      0.3924    4.280     2e-05 ***
#> Class 2 / 3     0.2302     -1.4687      0.6269   -2.343    0.0193 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Cluster 2 :
#>             Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> Class 1 / 3    18.3079      2.9073      0.4662    6.237  5.85e-10 ***
#> Class 2 / 3     0.1415     -1.9555      0.9124   -2.143    0.0323 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Level 1 Coefficients :
#> 
#> Class 1 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)    
#> SCH_LEVHigh School     0.1437     -1.9399      0.2192    -8.85    <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Class 2 / 3 :
#>                    Odds Ratio Coefficient  Std. Error  t value  Pr(>|t|)  
#> SCH_LEVHigh School     2.0271      0.7066      0.3474    2.034    0.0422 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

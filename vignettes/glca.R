## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(glca)

## ---- eval = FALSE------------------------------------------------------------
#  glca(formula, group, data, nclass, ncluster, std.err, measure.inv, coeff.inv,
#       init.param, n.init, decreasing, testiter, maxiter, eps,
#       na.rm, seed, verbose)

## ----eval = FALSE-------------------------------------------------------------
#  formula <- item(Y1, Y2, Y3, Y4) ~ 1

## ----eval = FALSE-------------------------------------------------------------
#  formula <- item(starts.with = "Y") ~ 1

## ----eval = FALSE-------------------------------------------------------------
#  formula <- item(starts.with = "Y") ~ X1 + Z1

## ----eval = FALSE-------------------------------------------------------------
#  gofglca(x, ..., criteria, test, nboot, maxiter, eps, seed, verbose)

## ----data1--------------------------------------------------------------------
data("gss08")
str(gss08)

## ----lca----------------------------------------------------------------------
f <- item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1
lca2 <- glca(f, data = gss08, nclass = 2, seed = 1)
lca3 <- glca(f, data = gss08, nclass = 3, seed = 1)
lca4 <- glca(f, data = gss08, nclass = 4, seed = 1)

## ----lcagof-------------------------------------------------------------------
gofglca(lca2, lca3, lca4, test = "boot", seed = 1)

## ----lcagof2, include = FALSE-------------------------------------------------
gof1 <- gofglca(lca2, lca3, lca4, test = "boot", seed = 1)

## ----lca3---------------------------------------------------------------------
summary(lca3)
plot(lca3)

## ----mglca--------------------------------------------------------------------
mglca1 <- glca(f, group = DEGREE, data = gss08, nclass = 3, seed = 1)
mglca2 <- glca(f, group = DEGREE, data = gss08, nclass = 3, measure.inv = FALSE, seed = 1)
gofglca(lca3, mglca1, mglca2, test = "chisq")

## ----mglca2, include = FALSE--------------------------------------------------
gof2 <- gofglca(lca3, mglca1, mglca2, test = "chisq")

## ----mglcr--------------------------------------------------------------------
fx <- item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ SEX
mglcr1 <- glca(fx, group = DEGREE, data = gss08, nclass = 3, seed = 3)
mglcr2 <- glca(fx, group = DEGREE, data = gss08, nclass = 3, 
              coeff.inv = FALSE, seed = 1)
gofglca(mglca1, mglcr1, mglcr2, test = "chisq")

## ----mglca4f, include = FALSE-------------------------------------------------
gof3 <- gofglca(mglca1, mglcr1, mglcr2, test = "chisq")

## -----------------------------------------------------------------------------
coef(mglcr1)
summary(mglcr1)

## ----data2--------------------------------------------------------------------
data("nyts18")
str(nyts18)

## ----lcas---------------------------------------------------------------------
f <- item(starts.with = "E") ~ 1
lca2 <- glca(f, data = nyts18, nclass = 2, seed = 1)
lca3 <- glca(f, data = nyts18, nclass = 3, seed = 1)
lca4 <- glca(f, data = nyts18, nclass = 4, seed = 1)

gofglca(lca2, lca3, lca4, test = "boot", seed = 1)

## ----lcas2, include = FALSE---------------------------------------------------
gof1 <- gofglca(lca2, lca3, lca4, test = "boot", seed = 1)

## ----mlca---------------------------------------------------------------------
nplca2 <- glca(f, group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2, seed = 1)
nplca3 <- glca(f, group = SCH_ID, data = nyts18, nclass = 3, ncluster = 3, seed = 1)
nplca4 <- glca(f, group = SCH_ID, data = nyts18, nclass = 3, ncluster = 4, seed = 1)

gofglca(lca3, nplca2, nplca3, nplca4, test = "boot", seed = 1)

## ----mlca2, include = FALSE---------------------------------------------------
gof2 <- gofglca(lca3, nplca2, nplca3, nplca4, test = "boot", seed = 1)

## ----mlcr---------------------------------------------------------------------
fx <- item(starts.with = "E") ~ SEX
nplcr1 <- glca(fx, group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2, seed = 1)
nplcr2 <- glca(fx, group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2, seed = 1, coeff.inv = FALSE)
nplcr.gof <- gofglca(nplca3, nplcr1, nplcr2, test = "chisq")
nplcr.gof$dtable

## ----mlcr2--------------------------------------------------------------------
coef(nplcr1)
plot(nplcr1)

## ----mlca3--------------------------------------------------------------------
f.2 <- item(starts.with = "E") ~ SEX + SCH_LEV
nplcr3 <- glca(f.2, group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2, seed = 3)
nplcr4 <- glca(f.2, group = SCH_ID, data = nyts18, nclass = 3, ncluster = 3, seed = 6)
gofglca(nplcr3, nplcr4, test = "boot", seed = 1)

## ----mlca_f, include = FALSE--------------------------------------------------
gof3 <- gofglca(nplcr3, nplcr4, test = "boot", seed = 1)

## ----mlca4--------------------------------------------------------------------
summary(nplcr3)

## ----impute-------------------------------------------------------------------
tmp1 <- unique(nyts18[c("SCH_ID", "SCH_LEV")])
tmp2 <- nplcr3$posterior$cluster
tmp3 <- data.frame(SCH_ID = rownames(tmp2), Cluster = factor(apply(tmp2, 1, which.max)))
ndata <- merge(tmp1, tmp3)
head(ndata)

## ----impute2------------------------------------------------------------------
fit <- glm(Cluster ~ SCH_LEV, family = binomial, data = ndata)
summary(fit)


\name{anova.glca}
\alias{anova.glca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Goodness of Fit Tests for Fitted \code{glca} Model.
}
\description{
Basically, show AIC and BIC for model fit criteria. When \code{nboot} given, using parametric bootstrap, compute bootstrap p-value for "relative" and "absolute" model fit.
}
\usage{
## S3 method for class 'glca'
anova(object, object2 = NULL, nboot = 0,
      maxiter = 500, eps = 1e-5, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of "\code{glca}", usually, a result of a call to \code{glca}.}
  \item{object2}{an object of "\code{glca}" to be compared with former object.}
  \item{nboot}{number of bootstrap samples.}
  \item{maxiter}{an integer for maximum number of iteration for bootstrap sample.}
  \item{eps}{positive convergence tolerance for bootstrap sample.}
  \item{verbose}{an logical value for whether or not to print the result of a function's execution.}
}
\details{
\code{anova.glca} gives 2 types of bootstrap p-value. one is about absolute model fit (\code{Boot Pr(Abs)}), and other is about relative model fit (\code{Boot Pr(Rel)}).

The absolute model fit gives p-value for H0 : model is adequate. and the relative model fit gives p-value for H0 : among given 2 models, simpler model is adequate.
}
\value{
This function returns a table with model fit criteria and bootstrap p-value.
}
\references{
%% ~put references to the literature/web site here ~
Akaike H (1974). \emph{A New Look at the Statistical Model Identification.} IEEE Transactions on Automatic Control, 19, 716–723. \url{http://doi.org/10.1109/tac.1974.1100705}

Schwarz G (1978). \emph{Estimating the Dimensions of a Model.} The Annals of Statistics, 6, 461–464. \url{http://doi.org/10.1214/aos/1176344136}

bootstrap p-value reference
}
\author{
Youngsun Kim
}
\note{
%%  ~~further notes~~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{glca}}
}
\examples{
data(gss)
class2 = glca(item(CAPPUN, PUNSIN, BLKWHITE, PERMORAL, LETDIE1) ~ 1,
              data = gss, nclass = 2)
class3 = glca(item(CAPPUN, PUNSIN, BLKWHITE, PERMORAL, LETDIE1) ~ 1,
              data = gss, nclass = 3)

anova(class2, class3)
anova(class2, class3, nboot = 100)


data(brfss)
cluster2 = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
                STATE, data = brfss, nclass = 3, ncluster = 2)
cluster3 = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
                STATE, data = brfss, nclass = 3, ncluster = 3)

anova(cluster2, cluster3)
anova(cluster2, cluster3, nboot = 100)


data(nhanes)
measInv = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ 1,
               GENDER, data = nhanes, nclass = 3)
measVar = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ 1,
               GENDER, data = nhanes, nclass = 3, measure_inv = FALSE)

anova(measInv, measVar)
anova(measInv, measVar, nboot = 100)
}

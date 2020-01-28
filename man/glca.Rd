\name{glca}
\alias{glca}
\title{
Fitting Latent Class Analysis with Grouped Data
}
\description{
Function for fitting LCA models with multiple group.
Multiple Group Latent Class Analysis and
Multilevel Latent Class Analysis can be fitted.
}
\usage{
glca(formula, group = NULL, data,
     nclass = 3, ncluster = 0,
     measure_inv = TRUE, std_err = TRUE,
     init_param = NULL, n_init = 1,
     verbose = TRUE,
     maxiter = 1000, eps = 1e-6)
}
\arguments{
\item{formula}{a formula for specifying manifest items using the "\code{item}" function and covariates.}
\item{group}{an optional vector specifying a group of observations, which can be include group covariates using "\code{group}" function}
\item{data}{a data frame containing the manifest item, covariates and group.}
\item{nclass}{number of latent classes. default is 3.}
\item{ncluster}{number of level 2 latent classes. default is 0.}
\item{measure_inv}{a logical value of the assumption of measurement invariance across groups.}
\item{std_err}{a logical value wheter calculating standard error of estimates. default is TRUE.}
\item{init_param}{a list which contains user-defined initial parameter.}
\item{n_init}{number of random initial parameter set.}
\item{verbose}{an logical value for whether or not to print the result of a function's execution.}
\item{maxiter}{an integer for maximum number of iteration.}
\item{eps}{positive convergence tolerance.}
}

\details{
The formula should consist of an \code{~} operator between two sides. Manifest items should be indicated in LHS of formula using \code{item} function and covariates should be specified in RHS of formula. For example, \cr
\code{item(y1, y2, y3) ~ 1}\cr
\code{item(y1, y2, y3) ~ x1 + x2}

The \code{glca} models are latent class models which assumes latent categorical variable (i.e latent cluster or latent class). Since those latent variabels are categorical, there are parameters indicating probability of each category and it is called "prevalence". According to latent variable, each manifest item behaves differently. Since for \code{glca} models, manifest items should be categorical variable, behaviors of manifest items can be depicted with multinomial distribution. And the probability of each category (differs according to latent class) is called "item response probability".

The parameters to be estimated are \code{delta}, \code{gamma}, \code{beta}, and \code{rho}. \code{delta} are prevalences of latent clusters, a latent categorical variables of groups, \code{gamma} are prevalences of latent classes (according to latent cluster for multilevel LCA), a latent categorical variables of individuals, \code{beta} are covariates coefficient for \code{gamma}, and \code{rho} are item response probabilities.
}

\value{
\code{glca} returns an object of class "\code{glca}".

The function \code{summary} prints estimates for parameters and \code{anova} function gives goodness of fit measures for the model.

An object of class "\code{glca}" is a list containing at least the following components:

\item{call}{the matched call.}
\item{model}{a list which contains model descriptions.}
\item{datalist}{a list of data used for fitting.}
\item{param}{a list of parameter estimates.}
\item{std.err}{a list of standard error for estimates}
\item{coefficient}{a list of model coefficients for prevalence.}
\item{posterior}{a data frame with posterior probablity of each individaul for latent classes}
\item{count}{a data frame with unique patterns of manifest items and corresponding observed and predicted counts.}
\item{gof}{a list of goodness of fit measures, i.e. AIC, BIC, and log-likelihood.}
\item{convergence}{a list about convergence which contains whether or not it has been converged, number of iterations and scores.}
}

\references{
Jeroen K. Vermunt (2003). \emph{7. Multilevel Latent Class Models}. Sociological Methodology, 33(1), 213–239. \url{https://doi.org/10.1111/j.0081-1750.2003.t01-1-00131.x}

Linda M. Collins, Stephanie T. Lanza (2009). \emph{Latent Class and Latent Transition Analysis: With Applications in the Social, Behavioral, and Health Sciences}. John Wiley & Sons Inc.
}

\examples{
# NHANES data (LCA)
data("nhanes")
lca1 = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ 1,
            data = nhanes, nclass = 2)
summary(lca1)
lca2 = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ 1,
            data = nhanes, nclass = 3)
anova(lca1, lca2)
anova(lca1, lca2, nboot = 100)

# GSS data (LCA with covariates)
data("gss")
lcr = glca(item(CAPPUN, PUNSIN, BLKWHITE, PERMORAL, LETDIE1) ~ HAPPY,
              data = gss, nclass = 2)
summary(lcr)
coef(lcr)

# GSS data (MGLCA)
mglca = glca(item(CAPPUN, PUNSIN, BLKWHITE, PERMORAL, LETDIE1) ~ 1,
             group = RACE, data = gss, nclass = 3)
summary(mglca)

# BRFSS data (MLCA)
data("brfss")
mlca = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
            group = STATE, data = brfss, nclass = 3, ncluster = 3)
summary(mlca)

# BRFSS data (MLCA with group covariates)
mlcr = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ SEX + STATE_GDP,
            group = STATE, data = brfss, nclass = 3, ncluster = 3)
summary(mlcr)
coef(mlcr)
}


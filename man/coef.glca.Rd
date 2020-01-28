\name{coef.glca}
\alias{coef.glca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract \code{glca} Model Coefficients.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
## S3 method for class 'glca'
coef(object, digits = max(3, getOption("digits") - 3),
     show.signif.stars = getOption("show.signif.stars"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of "\code{glca}", usually, a result of a call to \code{glca}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{show.signif.stars}{logical. If TRUE, ‘significance stars’ are printed for each coefficient.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Coefficient matrix from the \code{glca} model.

If the model has been calculated standard error, coefficient matrix contains standard error and t, p-value.
}
\references{
%% ~put references to the literature/web site here ~
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
## For examples see example(glca)
}

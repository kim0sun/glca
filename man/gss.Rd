\name{gss}
\alias{gss}
\docType{data}
\title{
General Social Study 2010
}
\description{
  Position on ethical issues and several covariates among 2044 respondents to the 2010 General Social Survey. Respondents answer the question about ethical problems such as abortion, death penalty, euthanasia, and so on. As covariates, The covariates include the sex, race of respondents and his/her residence at the age of 16.
}
\usage{data("gss")}
\format{
  A data frame with 2044 observations on 9 variables. Names of variables and corresponding question is following.
  \describe{
    \item{\code{ABANY}}{Please tell me whether or not you think it should be possible for a pregnant woman to obtain a legal abortion if the woman wants it for any reason?}
    \item{\code{CAPPUN}}{Do you favor or oppose the death penalty for persons convicted of murder?}
    \item{\code{BLKWHITE}}{Right and wrong are not usually a simple matter of black and white; there are many shades of gray.}
    \item{\code{ROTAPPLE}}{Immoral actions by one person can corrupt society in general.}
    \item{\code{PERMORAL}}{Morality is a personal matter and society should not force everyone to follow one standard.}
    \item{\code{LETDIE1}}{When a person has a disease that cannot be cured, do you think Doctors should be allowed by law to end the patient's life by some painless means if the patient and his family request it?}
    \item{\code{SEX}}{respondent's sex}
    \item{\code{RACE}}{What race do you consider yourself?}
    \item{\code{RES16}}{Which of the categories on this card comes closest to the type of place you were living in when you were 16 years old?}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\url{http://gss.norc.org}
}
\references{
Smith, Tom W, Peter Marsden, Michael Hout, and Jibum Kim. General Social Surveys, 2010/Principal Investigator, Tom W. Smith; Co-Principal Investigator, Peter V. Marsden; Co-Principal Investigator, Michael Hout; Sponsored by National Science Foundation. -NORC ed.- Chicago: NORC at the University of Chicago
}
\examples{
data("gss")
lca = glca(item(CAPPUN, ROTAPPLE, BLKWHITE, PERMORAL, LETDIE1) ~ 1,
           data = gss, nclass = 3)
summary(lca)
lcr = glca(item(CAPPUN, ROTAPPLE, BLKWHITE, PERMORAL, LETDIE1) ~ HAPPY,
           data = gss, nclass = 3)
summary(lcr)
coef(lcr)

mglca = glca(item(CAPPUN, ROTAPPLE, BLKWHITE, PERMORAL, LETDIE1) ~ 1,
             group = RACE, data = gss, nclass = 3)
summary(mglca)
mglcr = glca(item(CAPPUN, ROTAPPLE, BLKWHITE, PERMORAL, LETDIE1) ~ HAPPY,
             group = SEX, data = gss, nclass = 3)
summary(mglcr)
coef(mglcr)
}
\keyword{datasets}

\name{nhanes}
\alias{nhanes}
\docType{data}
\title{
National Health and Nutrition Examination Survey 2015-2016
}
\description{
There is a nine-item depression screening instrument of National Health and Nutrition Examination Survey, which can indicating mental health of respondents. Among thoes items, former 5 items are selected. And the covariates include gender, age, and race of respondents.
}
\usage{data("nhanes")}
\format{
  A data frame with 5735 observations on the following 8 variables.
  \describe{
    \item{\code{DPQ010}}{Have little interest in doing things}
    \item{\code{DPQ020}}{Feeling down, depressed, or hopeless}
    \item{\code{DPQ030}}{Trouble sleeping or sleeping too much}
    \item{\code{DPQ040}}{Feeling tired or having little energy}
    \item{\code{DPQ050}}{Poor appetite or overeating}
    \item{\code{GENDER}}{Respondent's Gender}
    \item{\code{AGE}}{Age in years at screening}
    \item{\code{RACE}}{Race/Hispanic origin w/ NH Asian}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\url{https://wwwn.cdc.gov/Nchs/Nhanes/}
}
\references{
Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: U.S. Department of Health and Human Services, Centers for Disease Control and Prevention, [2015-2016][\url{https://wwwn.cdc.gov/nchs/nhanes/ContinuousNhanes/Default.aspx?BeginYear=2015}]
}
\examples{
data("nhanes")

lca = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ 1,
           data = nhanes, nclass = 2)
summary(lca)
lcr = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ AGE,
           data = nhanes, nclass = 3)
summary(lcr)
coef(lcr)

mglca = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ 1,
             group = GENDER, data = nhanes, nclass = 3)
summary(mglca)
mglcr = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ AGE,
             group = GENDER, data = nhanes, nclass = 3)
summary(mglcr)
coef(mglcr)
}
\keyword{datasets}

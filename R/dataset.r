#' General Social Study (GSS) 2008
#'
#' This dataset includes 6 manifest items about abortion and several covariates from 355 respondents to the 2008 General Social Survey. Respondents answer the questions whether or not think it should be possible for a pregnant woman to obtain a legal abortion. The covariates include age, sex, race, region, and degree of respondents.
#'
#' @name gss08
#' @docType data
#' @keywords datasets
#' @format A data frame with 355 observations on 11 variables.
#' \describe{
#' \item{\code{DEFECT}}{If there is a strong chance of serious defect in the baby?}
#' \item{\code{HLTH}}{If the womans own health is seriously endangered by the pregnancy?}
#' \item{\code{RAPE}}{If she became pregnant as a result of rape?}
#' \item{\code{POOR}}{If the family has a very low income and cannot afford any more children?}
#' \item{\code{SINGLE}}{If she is not married and does not want to marry the man?}
#' \item{\code{NOMORE}}{If she is married and does not want any more children?}
#' \item{\code{AGE}}{Respondent's age}
#' \item{\code{SEX}}{Respondent's race}
#' \item{\code{RACE}}{Respondent's sex}
#' \item{\code{REGION}}{Region of interview}
#' \item{\code{DEGREE}}{Respondent's degree}
#' }
#' @source \url{http://gss.norc.org}
#' @references
#' Smith, Tom W, Peter Marsden, Michael Hout, and Jibum Kim. General Social Surveys, 2008/Principal Investigator, Tom W. Smith; Co-Principal Investigator, Peter V. Marsden; Co-Principal Investigator, Michael Hout; Sponsored by National Science Foundation. -NORC ed.- Chicago: NORC at the University of Chicago
#' @examples
#' data("gss")
#' # Model 1: LCA
#' lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'            data = gss08, nclass = 3)
#' summary(lca)
#'
#' # Model 2: LCA with a covariate
#' lcr = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ SEX,
#'            data = gss08, nclass = 3)
#' summary(lcr)
#' coef(lcr)
#'
#' # Model 3: MGLCA
#' mglca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'              group = REGION, data = gss08, nclass = 3)
#'
#' # Model 4: MGLCA with covariates
#' summary(mglca)
#' mglcr = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ AGE,
#'              group = SEX, data = gss08, nclass = 3)
#' summary(mglcr)
#' coef(mglcr)
NULL

#' National Youth Tobacco Survey (NYTS) 2018
#'
#' This dataset includes 5 manifest items about abortion and several covariates. From the original 2018 National Youth Tobacco Survey data, the Non Hispanic, white students are selected and schools with 30-50 students were selected. Thus, the dataset has 1743 respondents. The covariates include the sex of the respondents and the school ID to which the respondnets belong, and the level of the corresponding school.
#'
#' @name nyts18
#' @docType data
#' @keywords datasets
#' @format A data frame with 1734 observations on the following 8 variables.
#' \describe{
#' \item{\code{ECIGT}}{Whether to have tried cigarette smoking, even one or two puffs}
#' \item{\code{ECIGAR}}{Whether to have ever tried cigar smoking, even one or two puffs}
#' \item{\code{ESLT}}{Whether to have used chewing tobacco, snuff, or dip}
#' \item{\code{EELCIGT}}{Whether to have used electronic cigarettes or e-cigarettes}
#' \item{\code{EHOOKAH}}{Whether to have tried smoking tobacco from a hookah or a waterpipe}
#'
#' \item{\code{SEX}}{Respondent's Sex}
#' \item{\code{SCH_ID}}{School ID to which the respondent belongs}
#' \item{\code{SCH_LEV}}{Level of the corresponding school}
#' }
#' @source \url{https://www.cdc.gov/tobacco/data_statistics/surveys/nyts/index.htm}
#'
#' @examples
#' data("nyts18")
#' \donttest{
#' # Model 1: LCA
#' lca = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 1,
#'            data = nyts18, nclass = 3)
#' summary(lca)
#'
#' # Model 2: LCR
#' lca = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ SEX,
#'            data = nyts18, nclass = 3)
#' summary(lca)
#' coef(lca)
#'
#' # Model 3: MGLCA
#' mglca = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 1,
#'              group = SEX, data = nyts18, nclass = 3)
#' summary(mglca)
#'
#' # Model 4: MLCA
#' mlca = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 1,
#'    group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2)
#' summary(mlca)
#'
#' # Model 5: MLCA with level-1 covariate(s) only
#' mlcr = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ SEX,
#'             group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2)
#' summary(mlcr)
#' coef(mlcr)
#'
#' # Model 6: MLCA with level-1 and level-2 covariate(s)
#' # (SEX: level-1 covariate, PARTY: level-2 covariate)
#' mlcr2 = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ SEX + SCH_LEV,
#'              group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2)
#' summary(mlcr2)
#' coef(mlcr2)
#' }
NULL

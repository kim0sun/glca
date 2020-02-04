#' General Social Study 2018
#'
#'  and several covariates among 2044 respondents to the 2018 General Social Survey. Respondents answer the questions whether or not think it should be possible for a pregnant woman to obtain a legal abortion. As covariates, The covariates include the age, sex,
#'
#' @name gss
#' @docType data
#' @keywords datasets
#' @format A data frame with 2044 observations on 9 variables.
#' \describe{
#' \item{\code{ABDEFECT}}{If there is a strong chance of serious defect in the baby?}
#' \item{\code{ABNOMORE}}{If she is married and does not want any more children?}
#' \item{\code{ABHLTH}}{If the womans own health is seriously endangered by the pregnancy?}
#' \item{\code{ABPOOR}}{ If the family has a very low income and cannot afford any more children?}
#' \item{\code{ABRAPE}}{If she became pregnant as a result of rape?}
#' \item{\code{ABSINGLE}}{If she is not married and does not want to marry the man?}
#' \item{\code{ABANY}}{The woman wants it for any reason?}
#' \item{\code{AGE}}{Respondent's age}
#' \item{\code{SEX}}{Respondent's sex}
#' \item{\code{REGION}}{Region of interview}
#' \item{\code{DEGREE}}{Respondent's degree}
#' }
#' @source \url{http://gss.norc.org}
#' @references
#' Smith, Tom W, Peter Marsden, Michael Hout, and Jibum Kim. General Social Surveys, 2010/Principal Investigator, Tom W. Smith; Co-Principal Investigator, Peter V. Marsden; Co-Principal Investigator, Michael Hout; Sponsored by National Science Foundation. -NORC ed.- Chicago: NORC at the University of Chicago
#' @examples
#' data("gss")
#' lca = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'            data = gss, nclass = 3)
#' summary(lca)
#' lcr = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ AGE,
#'            data = gss, nclass = 3)
#' summary(lcr)
#' coef(lcr)
#'
#' mglca = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'              group = DEGREE, data = gss, nclass = 3)
#' summary(mglca)
#' mglcr = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ AGE,
#'              group = SEX, data = gss, nclass = 3)
#' summary(mglcr)
#' coef(mglcr)
#'
#' mlca =  glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'              group = REGION, data = gss, nclass = 3, ncluster = 2)
#' summary(mlca)
NULL

#' National Health and Nutrition Examination Survey 2015-2016
#'
#' There is a nine-item depression screening instrument of National Health and Nutrition Examination Survey, which can indicating mental health of respondents. Among thoes items, former 5 items are selected. And the covariates include gender, age, and race of respondents.
#'
#' @name nhanes
#' @docType data
#' @keywords datasets
#' @format A data frame with 5735 observations on the following 8 variables.
#' \describe{
#' \item{\code{DPQ010}}{Have little interest in doing things}
#' \item{\code{DPQ020}}{Feeling down, depressed, or hopeless}
#' \item{\code{DPQ030}}{Trouble sleeping or sleeping too much}
#' \item{\code{DPQ040}}{Feeling tired or having little energy}
#' \item{\code{DPQ050}}{Poor appetite or overeating}
#' \item{\code{AGE}}{Age in years at screening}
#' \item{\code{GENDER}}{Respondent's Gender}
#' \item{\code{RACE}}{Race/Hispanic origin w/ NH Asian}
#' }
#' @source \url{https://wwwn.cdc.gov/Nchs/Nhanes/}
#' @references Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: U.S. Department of Health and Human Services, Centers for Disease Control and Prevention, [2015-2016][\url{https://wwwn.cdc.gov/nchs/nhanes/ContinuousNhanes/Default.aspx?BeginYear=2015}]
#' @examples
#' data("nhanes")
#'
#' lca = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ 1,
#'            data = nhanes, nclass = 2)
#' summary(lca)
#' lcr = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ AGE,
#'            data = nhanes, nclass = 3)
#' summary(lcr)
#' coef(lcr)
#'
#' mglca = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ 1,
#'              group = GENDER, data = nhanes, nclass = 3)
#' summary(mglca)
#' mglcr = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ AGE,
#'              group = GENDER, data = nhanes, nclass = 3)
#' summary(mglcr)
#' coef(mglcr)
#'
#' mlca = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ 1,
#'             group = RACE, data = nhanes, nclass = 3, ncluster = 2)
#' summary(mlca)
NULL

#' Behavioral Risk Factor Surveillance System 2017
#'
#' This data is comprised of questions related to healthy lifestyles, such as obesity, exercise time, eating habits, smoking and drinking, among the 2017 BRFSS survey, and includes covariates such as respondents’ gender and income levels. There are also states in which the respondent resides as a group variable, regions in which the state belongs, and parties in which the state won the 2016 presidential election.
#'
#' @name brfss
#' @docType data
#' @keywords datasets
#' @format A data frame with 444023 observations on the following 10 variables.
#' \describe{
#' \item{\code{OBESE}}{(original : \code{_RFBMI5}) Adults who have a body mass index greater than 25.00 (Overweight or Obese)
#'
#' (1) No \cr (2) Yes}
#' \item{\code{PA300}}{(original : \code{_PA300R2}) Adults that participated in 300 minutes (or vigorous equivalent minutes) of physical activity per week.
#'
#' (1) 301+ minutes \cr (2) 1-300 minutes \cr (3) 0 minutes
#' \cr
#' }
#' \item{\code{FRTLT1A}}{(original : \code{_FRTLT1A}) Consume Fruit 1 or more times per day
#'
#' (1) Consumed fruit one or more times per day \cr
#' (2) Consumed fruit less than one time per day}
#' \item{\code{VEGLT1A}}{(original : \code{_VEGLT1A}) Consume Vegetables 1 or more times per day
#'
#' (1) Consumed vegetables one or more times per day \cr
#' (2) Consumed vegetables less than one time per day}
#' \item{\code{SMOKER}}{(original : \code{_SMOKER3}) Four-level smoker status: Everyday smoker, Someday smoker, Former smoker, Non-smoker
#'
#' (1) Current smoker - now smokes every day\cr
#' (2) Current smoker - now smokes some days\cr
#' (3) Former smoker\cr
#' (4) Never smoked
#' }
#' \item{\code{DRNK30}}{(original : \code{DRNKANY5}) Adults who reported having had at least one drink of alcohol in the past 30 days.
#'
#' (1) Yes\cr
#' (2) No}
#' \item{\code{SEX}}{Respondents Sex}
#' \item{\code{INCOME}}{(original : \code{INCOME2}) Income Level}
#' \item{\code{STATE}}{(original : \code{_STATE}) State FIPS Code}
#' \item{\code{REGION}}{Region to which the states belongs}
#' \item{\code{PARTY}}{The party that won the 2016 presidential election by state}
#' }
#' @source \url{https://www.cdc.gov/brfss/}
#'
#' \url{https://doi.org/10.7910/DVN/42MVDX}
#' @references
#' Centers for Disease Control and Prevention (CDC). Behavioral Risk Factor Surveillance System Survey Data. Atlanta, Georgia: U.S. Department of Health and Human Services, Centers for Disease Control and Prevention, [2017].
#'
#' MIT Election Data and Science Lab, 2017, "U.S. President 1976–2016", Harvard Dataverse, V5.
#' @examples
#' data("brfss")
#' brfss2000 = brfss[sample(1:nrow(brfss), 2000),]

#' lca = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
#'    data = brfss2000, nclass = 3)
#' summary(lca)
#'
#' lcr = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ SEX,
#'    group = INCOME, data = brfss2000, nclass = 3)
#' summary(lcr)
#' coef(lcr)
#'
#' mglca = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
#'    group = SEX, data = brfss2000, nclass = 3)
#' summary(mglca)
#'
#' mlca = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
#'    group = STATE, data = brfss2000, nclass = 3, ncluster = 2)
#' summary(mlca)
#'
#' mlcr = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ SEX,
#'    group = STATE, data = brfss2000, nclass = 3, ncluster = 2)
#' summary(mlcr)
#' coef(mlcr)
#'
#' mlcr2 = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ SEX + PARTY,
#'    group = STATE, data = brfss2000, nclass = 3, ncluster = 2)
#' summary(mlcr2)
#' coef(mlcr2)
NULL

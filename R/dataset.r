#' General Social Study 2018
#'
#' This dataset includes 6 manifest items about abortion and several covariates from 2044 respondents to the 2018 General Social Survey. Respondents answer the questions whether or not think it should be possible for a pregnant woman to obtain a legal abortion. The covariates include age, sex, region, and degree of respondents.
#'
#' @name gss
#' @docType data
#' @keywords datasets
#' @format A data frame with 2044 observations on 9 variables.
#' \describe{
#' \item{\code{ABDEFECT}}{If there is a strong chance of serious defect in the baby?}
#' \item{\code{ABNOMORE}}{If she is married and does not want any more children?}
#' \item{\code{ABHLTH}}{If the womans own health is seriously endangered by the pregnancy?}
#' \item{\code{ABPOOR}}{If the family has a very low income and cannot afford any more children?}
#' \item{\code{ABRAPE}}{If she became pregnant as a result of rape?}
#' \item{\code{ABSINGLE}}{If she is not married and does not want to marry the man?}
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
#' # Model 1: LCA
#' lca = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABPOOR, ABSINGLE, ABNOMORE) ~ 1,
#'            data = gss, nclass = 3)
#' summary(lca)
#'
#' # Model 2: LCA with a covariate
#' lcr = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABPOOR, ABSINGLE, ABNOMORE) ~ SEX,
#'            data = gss, nclass = 3)
#' summary(lcr)
#' coef(lcr)
#'
#' # Model 3: MGLCA
#' mglca = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABPOOR, ABSINGLE, ABNOMORE) ~ 1,
#'              group = REGION, data = gss, nclass = 3)
#'
#' # Model 4: MGLCA with covariates
#' summary(mglca)
#' mglcr = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABPOOR, ABSINGLE, ABNOMORE) ~ AGE,
#'              group = SEX, data = gss, nclass = 3)
#' summary(mglcr)
#' coef(mglcr)
#'
#' # Model 5: MLCA
#' mlca =  glca(item(ABDEFECT, ABHLTH, ABRAPE, ABPOOR, ABSINGLE, ABNOMORE) ~ 1,
#'              group = REGION, data = gss, nclass = 3, ncluster = 2)
#' summary(mlca)
NULL

#' National Health and Nutrition Examination Survey 2015-2016
#'
#' There is a 9 manifest items about frequency of depression symptoms over the past 2 weeks from National Health and Nutrition Examination Survey (NHANES). All of 9 items are measured 4 point-scale, "Not at all", "Several days", "More than half the days", and "Nearly every day". Those four categories are converted to binary as "Yes" ("Nearly every day", "More than half the days", "Several days") and "No" ("Not at all"). The dataset includes gender, age, and race of respondents as covariates.
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
#' \item{\code{DPQ060}}{Feeling bad about yourself}
#' \item{\code{DPQ070}}{Trouble concentrating on things}
#' \item{\code{DPQ080}}{Moving or speaking slowly or too fast}
#' \item{\code{DPQ090}}{Thought you would be better off dead}

#' \item{\code{AGE}}{Age in years at screening}
#' \item{\code{GENDER}}{Respondent's Gender}
#' \item{\code{RACE}}{Race/Hispanic origin w/ NH Asian}
#' }
#' @source \url{https://wwwn.cdc.gov/Nchs/Nhanes/}
#' @references Centers for Disease Control and Prevention (CDC). National Center for Health Statistics (NCHS). National Health and Nutrition Examination Survey Data. Hyattsville, MD: U.S. Department of Health and Human Services, Centers for Disease Control and Prevention, [2015-2016][\url{https://wwwn.cdc.gov/nchs/nhanes/ContinuousNhanes/Default.aspx?BeginYear=2015}]
#' @examples
#' data("nhanes")
#' # Model 1: LCA
#' lca = glca(item(starts.with = "DPQ") ~ 1, data = nhanes, nclass = 2)
#' summary(lca)
#'
#' # Model 2: LCA with covariate(s)
#' lcr = glca(item(starts.with = "DPQ") ~ AGE, data = nhanes, nclass = 3)
#' summary(lcr)
#' coef(lcr)
#'
#' # Model 3: MGLCA
#' mglca = glca(item(starts.with = "DPQ") ~ 1, group = GENDER,
#'              data = nhanes, nclass = 3)
#' summary(mglca)
#'
#' # Model 4: MGLCA with covariate(s)
#' mglcr = glca(item(starts.with = "DPQ") ~ AGE, group = GENDER,
#'              data = nhanes, nclass = 3)
#' summary(mglcr)
#' coef(mglcr)
#'
#' # Model 5: MLCA
#' mlca = glca(item(starts.with = "DPQ") ~ 1, group = RACE,
#'             data = nhanes, nclass = 3, ncluster = 2)
#' summary(mlca)
NULL

#' Behavioral Risk Factor Surveillance System 2017 (BRFSS 2017)
#'
#' This data is comprised of questions related to healthy lifestyles, such as obesity, exercise time, eating habits, and smoking and drinking behaviors from the BRFSS 2017 survey. State is a group variable indicating the respondent's residential state. The data includes both individual-level (level-1) and group-level (level-2) covariates. The level-1 covariates include respondents' gender and income level. The level-2 covariates are regions of the United States and political parties which won the 2016 presidential election in the state.
#'
#' @name brfss
#' @docType data
#' @keywords datasets
#' @format A data frame with 444023 observations on the following 10 variables.
#' \describe{
#' \item{\code{OBESE}}{(original : \code{_RFBMI5}) Adults who have a body mass index greater than 25.00 (Overweight or Obese)
#'
#' (1) No \cr (2) Yes}
#' \item{\code{PA300}}{(original : \code{_PA300R2}) Adults that participated in 300 minutes (or vigorous equivalent minutes) of physical activity per week
#'
#' (1) 301+ minutes \cr (2) 1-300 minutes \cr (3) 0 minutes
#' \cr
#' }
#' \item{\code{FRTLT1A}}{(original : \code{_FRTLT1A}) Consume fruit 1 or more times per day
#'
#' (1) Yes \cr
#' (2) No}
#' \item{\code{VEGLT1A}}{(original : \code{_VEGLT1A}) Consume vegetables 1 or more times per day
#'
#' (1) Yes\cr
#' (2) No}
#' \item{\code{SMOKER}}{(original : \code{_SMOKER3}) Four-level smoker status
#'
#' (1) Never smoked\cr
#' (2) Former smoker\cr
#' (3) Occational smoker\cr
#' (4) Everyday smoker
#' }
#' \item{\code{DRNK30}}{(original : \code{DRNKANY5}) Adults who reported having had at least one drink of alcohol in the past 30 days
#'
#' (1) No\cr
#' (2) Yes}
#' \item{\code{SEX}}{Respondents Sex}
#' \item{\code{INCOME}}{(original : \code{INCOME2}) Income level}
#' \item{\code{STATE}}{(original : \code{_STATE}) State FIPS code}
#' \item{\code{REGION}}{Region to which the state belongs}
#' \item{\code{PARTY}}{The party that won the 2016 presidential election by state}
#' }
#' @source \url{https://www.cdc.gov/brfss/}
#'
#' \url{https://doi.org/10.7910/DVN/42MVDX}
#' @references
#' Centers for Disease Control and Prevention. (2017). 2017 Behavioral Risk Factor Surveillance System Survey Data. Retrieved from \url{https://www.cdc.gov/brfss/}.
#'
#' MIT Election Data and Science Lab, (2017). U.S. President 1976-2016. Retrieved from \doi{10.7910/DVN/42MVDX}.
#' @examples
#' data("brfss")
#' \donttest{
#' brfss1000 = brfss[sample(1:nrow(brfss), 1000),]
#'
#' # Model 1: LCA
#' lca = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
#'    data = brfss1000, nclass = 3)
#' summary(lca)
#'
#' # Model 2: MGLCA
#' mglca = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
#'    group = SEX, data = brfss1000, nclass = 3)
#' summary(mglca)
#'
#' # Model 3: MGLCA with covariate(s)
#' mglcr = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ REGION,
#'    group = SEX, data = brfss1000, nclass = 3)
#' summary(mglcr)
#' coef(mglcr)
#'
#' # Model 4: MLCA
#' mlca = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
#'    group = STATE, data = brfss1000, nclass = 3, ncluster = 2)
#' summary(mlca)
#'
#' # Model 5: MLCA with level-1 covariate(s) only
#' mlcr = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ SEX,
#'             group = STATE, data = brfss1000, nclass = 3, ncluster = 2)
#' summary(mlcr)
#' coef(mlcr)
#'
#' # Model 6: MLCA with level-1 and level-2 covariate(s)
#' # (SEX: level-1 covariate, PARTY: level-2 covariate)
#' mlcr2 = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ SEX + PARTY,
#'              group = STATE, data = brfss1000, nclass = 3, ncluster = 2)
#' summary(mlcr2)
#' coef(mlcr2)
#' }
NULL

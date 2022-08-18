#' Manifest Anxiety Scale
#'
#' The Taylor Manifest Anxiety Scale was first developed in 1953 to identify
#' individuals who would be good subjects for studies of stress and other
#' related psychological phenomenon. Since then it has been used as a measure of
#' anxiety as general personality trait. Anxiety is a complex psychological
#' construct that includes a multiple of different facets related to extensive
#' worrying that may impair normal functioning. The test has been widely studied
#' and used in research, however there are some concerns that it does not measure
#' a single trait, but instead, measures a basket of loosely related ones and so
#' the score is not that meaningful.
#'
#' The data comes from an online offering of the Taylor Manifest Anxiety Scale.
#' At the end of the test users were asked if their answers were accurate and
#' could be used for research, 76% said yes and have been published on
#' \url{https://openpsychometrics.org/}.
#'
#' #' items 1 to 50 were rated 1=True and 2=False. gender, chosen from
#' a drop down menu (1=male, 2=female, 3=other) and age was
#' entered as a free response (ages<14 have been removed)
#'
#' @format A data frame with 4469 rows and 52 variables:
#' \describe{
#'   \item{gender}{participants' gender}
#'   \item{age}{participants' age in years}
#'   \item{Q1}{I do not tire quickly.}
#'   \item{Q2}{I am troubled by attacks of nausea.}
#'   \item{Q3}{I believe I am no more nervous than most others.}
#'   \item{Q4}{I have very few headaches.}
#'   \item{Q5}{I work under a great deal of tension.}
#'   \item{Q6}{I cannot keep my mind on one thing.}
#'   \item{Q7}{I worry over money and business.}
#'   \item{Q8}{I frequently notice my hand shakes when I try to do something.}
#'   \item{Q9}{I blush no more often than others.}
#'   \item{Q10}{I have diarrhea once a month or more.}
#'   \item{Q11}{I worry quite a bit over possible misfortunes.}
#'   \item{Q12}{I practically never blush.}
#'   \item{Q13}{I am often afraid that I am going to blush.}
#'   \item{Q14}{I have nightmares every few nights.}
#'   \item{Q15}{My hands and feet are usually warm.}
#'   \item{Q16}{I sweat very easily even on cool days.}
#'   \item{Q17}{Sometimes when embarrassed, I break out in a sweat.}
#'   \item{Q18}{I hardly ever notice my heart pounding and I am seldom short of breath.}
#'   \item{Q19}{I feel hungry almost all the time.}
#'   \item{Q20}{I am very seldom troubled by constipation.}
#'   \item{Q21}{I have a great deal of stomach trouble.}
#'   \item{Q22}{I have had periods in which I lost sleep over worry.}
#'   \item{Q23}{My sleep is fitful and disturbed.}
#'   \item{Q24}{I dream frequently about things that are best kept to myself.}
#'   \item{Q25}{I am easily embarrassed.}
#'   \item{Q26}{I am more sensitive than most other people.}
#'   \item{Q27}{I frequently find myself worrying about something.}
#'   \item{Q28}{I wish I could be as happy as others seem to be.}
#'   \item{Q29}{I am usually calm and not easily upset.}
#'   \item{Q30}{I cry easily.}
#'   \item{Q31}{I feel anxiety about something or someone almost all the time.}
#'   \item{Q32}{I am happy most of the time.}
#'   \item{Q33}{It makes me nervous to have to wait.}
#'   \item{Q34}{I have periods of such great restlessness that I cannot sit long I a chair.}
#'   \item{Q35}{Sometimes I become so excited that I find it hard to get to sleep.}
#'   \item{Q36}{I have sometimes felt that difficulties were piling up so high that I could not overcome them.}
#'   \item{Q37}{I must admit that I have at times been worried beyond reason over something that really did not matter.}
#'   \item{Q38}{I have very few fears compared to my friends.}
#'   \item{Q39}{I have been afraid of things or people that I know could not hurt me.}
#'   \item{Q40}{I certainly feel useless at times.}
#'   \item{Q41}{I find it hard to keep my mind on a task or job.}
#'   \item{Q42}{I am usually self-conscious.}
#'   \item{Q43}{I am inclined to take things hard.}
#'   \item{Q44}{I am a high-strung person.}
#'   \item{Q45}{Life is a trial for me much of the time.}
#'   \item{Q46}{At times I think I am no good at all.}
#'   \item{Q47}{I am certainly lacking in self-confidence.}
#'   \item{Q48}{I sometimes feel that I am about to go to pieces.}
#'   \item{Q49}{I shrink from facing crisis of difficulty.}
#'   \item{Q50}{I am entirely self-confident.}
#' }
#' @source \url{https://openpsychometrics.org/tests/TMAS/}
#' @references Taylor, J. (1953). "A personality scale of manifest anxiety".
#'             The Journal of Abnormal and Social Psychology, 48(2), 285-290.
"manifest"

# TMA <- read_csv("TMA.csv", col_types = cols(score = col_skip()))
# TMA <- as.data.frame(TMA)
# head(TMA$gender)
# TMA$gender[TMA$gender == 0] <- NA
# TMA$gender <- factor(TMA$gender,
#                      levels = c(1,2,3),
#                      labels = c("Male","Female","Other"))
#
# # code the missing responses
# for (i in 1:50) {
#   index <- TMA[, paste0("Q",i)] == 0
#   TMA[index, paste0("Q",i)] <- NA
#   TMA[, paste0("Q",i)] <-factor(TMA[, paste0("Q",i)],
#                                 levels = c(1,2),
#                                 labels = c("TRUE","FALSE"))
# }
# manifest <- na.omit(manifest)



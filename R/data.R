#' @title Personality risk factors for drug consumption
#' @description Personality variables and drug consumption habits of 1885
#'   individuals in mostly English speaking countries. Twelve personality
#'   or demographic variables were measured, as was the respondent's consumption
#'   of 18 mostly illegal drugs. 
#' @name drugriskfactors
#' @format A data frame with 1885 rows and 32 variables: \describe{
#'   \item{\code{id}}{(factor) A unique identifier of the survey respondent.}
#'   \item{\code{age}}{(ordinal) Respondent's age group, e.g. 18-24, 35-44,
#'   etc.} \item{\code{gender}}{(factor) Gender of respondent (Female/Male).}
#'   \item{\code{education}}{(ordinal) Respondent's education level.}
#'   \item{\code{country}}{(factor) The country where respondent lives.}
#'   \item{\code{ethnicity}}{(factor) Ethnicity of respondent.}
#'   \item{\code{nscore}}{(numeric) Respondent's NEO-FFI-R Neuroticism score.}
#'   \item{\code{escore}}{(numeric) Respondent's NEO-FFI-R Extraversion score.}
#'   \item{\code{oscore}}{(numeric) Respondent's NEO-FFI-R Openness score.}
#'   \item{\code{ascore}}{(numeric) Respondent's NEO-FFI-R Agreeableness score.}
#'   \item{\code{cscore}}{(numeric) Respondent's NEO-FFI-R Conscientiousness score.}
#'   \item{\code{impuls}}{(numeric) Respondent's BIS-11 impulsiveness score.}
#'   \item{\code{senseek}}{(numeric) Respondent's ImpSS sensation seeking score.}
#'   \item{\code{alcohol}}{(ordinal) Respondent's alcohol consumption. For this
#'   and every other drug consumption variable, consumption was measured on
#'   a seven point ordinal scale: 1) "Never Used", 2) "Used over a Decade Ago",
#'   3) "Used in Last Decade", 4) "Used in Last Year", 5) "Used in Last Month",
#'   6) "Used in Last Week", 7) "Used in Last Day". In the analysis described in
#'   Fehrman et al (2017), a respondent was labelled a "non-user" of a given
#'   drug if they chose "Never Used" or "Used over a Decade Ago" as their
#'   response describing their consumption of that drug, and were labelled a
#'   "user" for all other five values, i.e. "Used in Last Decade" to "Used in
#'   Last Day".}
#'   \item{\code{amphet}}{(ordinal) Respondent's amphetamines consumption. }
#'   \item{\code{amyl}}{(ordinal) Respondent's amyl nitrite consumption. }
#'   \item{\code{benzos}}{(ordinal) Respondent's benzodiazepine consumption. }
#'   \item{\code{caff}}{(ordinal) Respondent's caffeine consumption. }
#'   \item{\code{cannabis}}{(ordinal) Respondent's cannabis consumption. }
#'   \item{\code{choc}}{(ordinal) Respondent's chocolate consumption. }
#'   \item{\code{coke}}{(ordinal) Respondent's cocaine consumption. }
#'   \item{\code{crack}}{(ordinal) Respondent's crack cocaine consumption. }
#'   \item{\code{ecstasy}}{(ordinal) Respondent's ecstasy (Methylenedioxymethamphetamine, MDMA) consumption. }
#'   \item{\code{heroin}}{(ordinal) Respondent's heroin consumption. }
#'   \item{\code{ketamine}}{(ordinal) Respondent's ketamine consumption. }
#'   \item{\code{legalh}}{(ordinal) Respondent's legal high (designer psychoactive drugs) consumption. }
#'   \item{\code{lsd}}{(ordinal) Respondent's LSD (lysergic acid diethylamide) consumption. }
#'   \item{\code{meth}}{(ordinal) Respondent's methadone consumption. Note that is methadone, and not methamphetamine.}
#'   \item{\code{mushrooms}}{(ordinal) Respondent's psilocybin mushrooms consumption. }
#'   \item{\code{nicotine}}{(ordinal) Respondent's nicontine consumption. }
#'   \item{\code{semer}}{(ordinal) Respondent's "semeron" consumption. Semeron is a fictitious drug that was used to identify over-claimers. }
#'   \item{\code{vsa}}{(ordinal) Respondent's  volatile substance abuse (solvents etc) consumption.} 
#'}
#' @details This dataset is based on the dataset entitled
#'   \href{https://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29#}{"Drug
#'    consumption (quantified)" in the UCI Machine Learning Repository}. That
#'   dataset is the dataset that accompanies the Fehrman et al (2017) paper.
#'   However, most of the variables in that dataset were recoded from their
#'   original values by converting them into standardized real numbers. Details
#'   of the original values of all variables
#'   \href{https://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29#}{UCI
#'   Machine Learning Repository webpage}, and also in the codebook named
#'   "36536-0001-Codebook.pdf" available from
#'   \href{https://www.icpsr.umich.edu/web/ICPSR/studies/36536/}{https://www.icpsr.umich.edu/web/ICPSR/studies/36536/}.
#' Here, we have de-coded all variables back to their original values.
#' @source The original source of this data is described in the paper Fehrman,
#'   E., Muhammad, A. K., Mirkes, E. M., Egan, V., & Gorban, A. N. (2017). The
#'   five factor model of personality and evaluation of drug consumption risk.
#'   In \emph{Data science} (pp. 231-242). Springer, Cham, which is available
#'   \href{https://link.springer.com/chapter/10.1007/978-3-319-55723-6_18}{here}.
#'   The arXiv-ed preprint is available at
#'   \href{https://arxiv.org/abs/1506.06297}{https://arxiv.org/abs/1506.06297}.
NULL



#' @title YouGov 2017 Poll of Illegal Drug Use
#' @description Data concerning the question "Have you ever taken an illegal
#'   drug?" that was in a 2017 poll of 1300 British young adults. Individual
#'   respondents' data are not available but respondents were grouped into
#'   various demographic groups, including age group and gender. Here, we report
#'   the number of respondents in each combination of gender and age group who
#'   gave each of the 8 possible responses to the question.
#' @name yougov17drugs
#' @format A data frame with 48 rows and 4 variables: 
#' \describe{
#'   \item{\code{gender}}{(factor) Respondents' gender (male/female)}
#'   \item{\code{age}}{(ordinal) Age group (16-18, 19-21, 22-24)}
#'   \item{\code{response}}{(factor) Each possible response to the question
#'   "Have you ever taken an illegal drug". There were 8 possible responses: 1) "I
#'   have taken illegal drugs frequently and still do", 2) "I have taken illegal
#'   drugs frequently but have now stopped completely", 3) "I have taken illegal
#'   drugs occasionally and still do", 4) "I have taken illegal drugs occasionally
#'   but have now stopped completely", 5) "I have only taken illegal drugs once or
#'   twice", 6) "I’ve never taken any illegal drugs", 7) "Not sure", 8) "Prefer not to
#'   say".} 
#'   \item{\code{count}}{(numeric) The number of respondents who gave each
#'   particular response to the question.}
#'   }
#' @details The poll in which this question occurred was conducted between the
#'   27th June and the 5th July 2017. A total of 1300 young adults were
#'   surveyed, all of whom were residents of the Britain (England, Wales, or
#'   Scotland), and whose ages ranged from 16 to 24. The individual respondent
#'   level data were not made available. However, the number of individuals in
#'   various demographic categories, including age, gender, etc., were provided.
#'   In addition, the percentage of individuals in each category that chose each
#'   possible response to the question was provided. For example, the report of
#'   the poll results states that there were 162 males in the 16-18 age group.
#'   It also states that 12% of respondents in this male 16-18 group responded
#'   to the question with "I have taken illegal drugs occasionally and still
#'   do". If we assume that this entails that 12% of the 162 individuals in this
#'   group responded in this way, then this is approximately 19 individuals in
#'   this gender and age group who responded this way. It should be noted,
#'   however, that it is not obvious that the count of the number of individuals
#'   in each group who responded in any given way can be obtained simply by
#'   multiplying the reported percentage by the reported total, as we have just
#'   done. The percentages may have been calculated using some weighting
#'   formula. Although it is not explicitly stated that this was done, the
#'   report does provide unweighted as well as weighted totals in each
#'   demographic category, and thus it may be that some weighting was applied to
#'   obtain the reported percentage. Notwithstanding this possibility, here we
#'   report counts for each category and response type obtaining by simply
#'   multiplying the stated unweighted total for each category by the stated
#'   percentage for each possible response.
#' @source The results of the YouGov poll are available in pdf form at
#'   \href{https://d25d2506sfb94s.cloudfront.net/cumulus_uploads/document/h5xr6v0nr4/VICEResults_170706_Drugs_16-24_W.pdf}{https://d25d2506sfb94s.cloudfront.net/cumulus_uploads/document/h5xr6v0nr4/VICEResults_170706_Drugs_16-24_W.pdf}.
#'   
NULL


#' @title Scores from a Mathematics Placement Exam
#' @description Scores on a mathematics placement exam in an unnaned liberal arts college, presumably in the US.
#' @name mathplacement
#' @format A data frame with 2696 rows and 1 variable
#' \describe{
#' \item{\code{score}}{(numeric) Score on exam}
#' }
#' 
#'  
#' @source This data set is a subset of the `MathPlacement` data set in the \href{https://cran.r-project.org/package=Stat2Data}{`Stats2Data` R package}.
#' Specifically, the `score` variable is the the `PlcmtScore` variable from `MathPlacement`.
NULL

#' @title Reaction times from one condition of a keyboard typing experiment
#' @description Participants in an experiment were tasked to type the words or
#'   nonwords that they were shown on a computer screen. The nonwords were
#'   either completely random strings or else they consisted of letter bigrams
#'   that are typical in written English. The words and nonwords were five
#'   letters in length. In the experimental condition recorded in this data set, the
#'   keyboard was visible as normal. On each trial, the time in milliseconds taken to type the first
#'   letter was recorded. Only trials where the word was correctly typed are
#'   recorded in this data set.
#' @name behmercrump_vis
#' @format A data frame with 114 rows and 3 variables 
#' \describe{
#'   \item{\code{subject}}{(factor) Identifier of participant in experiment.}
#'   \item{\code{condition}}{(factor) The experimental condition that determined the type of stimulus that was displayed on each trial
#'   of the condition where the keyboard was visible. Either a normal English word (normal) or a
#'   random string of letters (random) or a random string of letters with
#'   typical English letter bigrams.} 
#'   \item{\code{log_rt}}{(numeric) The natural
#'   logarithm of the time to type the first letter on each trial.} }
#'   
#' @details This data is taken from the keyboard visible condition of Experiment 1 that is described in the 2017 paper "Spatial Knowledge during Skilled
#' Action Sequencing: Hierarchical versus Nonhierarchical Representations" \insertCite{behmer2017spatial}{isdsr}.
#' @references 
#' \insertRef{behmer2017spatial}{isdsr}
#' @source The original raw data are available on \href{https://github.com/CrumpLab/statistics/blob/master/data/exp1_BehmerCrumpAPP.csv}{GitHub} under a CC-BY-SA-4.0 licence. The published paper is available at \href{https://link.springer.com/article/10.3758/s13414-017-1389-3}{https://link.springer.com/article/10.3758/s13414-017-1389-3}.
NULL

#' @title Reaction times from two conditions of a keyboard typing experiment
#' @description Participants in an experiment were tasked to type the words or
#'   nonwords that they were shown on a computer screen. The nonwords were
#'   either completely random strings or else they consisted of letter bigrams
#'   that are typical in written English. The words and nonwords were five
#'   letters in length. For all participants, there were two conditions. In one condition, the
#'   keyboard was visible as normal. In the other condition, the keyboard was
#'   occluded. On each trial, the time in milliseconds taken to type the first
#'   letter was recorded. Only trials where the word was correctly typed are
#'   recorded in this data set.
#'   
#' @name behmercrump
#' @format A data frame with 228 rows and 4 variables 
#' \describe{
#'   \item{\code{subject}}{(factor) Identifier of participant in experiment.}
#'   \item{\code{keyboard}}{(factor) Experiment condition indicating if the keyboard visible or occluded.}
#'   \item{\code{condition}}{(factor) The experimental condition that determined the type of stimulus that was displayed: either a normal English word (normal) or a
#'   random string of letters (random) or a random string of letters with
#'   typical English letter bigrams.} 
#'   \item{\code{log_rt}}{(numeric) The natural
#'   logarithm of the time to type the first letter on each trial.} }
#' 
#' @details This data is taken from Experiment 1 that is described in the 2017 paper "Spatial Knowledge during Skilled
#' Action Sequencing: Hierarchical versus Nonhierarchical Representations" \insertCite{behmer2017spatial}{isdsr}.
#' @references 
#' \insertRef{behmer2017spatial}{isdsr}
#' @source The original raw data are available on \href{https://github.com/CrumpLab/statistics/blob/master/data/exp1_BehmerCrumpAPP.csv}{GitHub} under a CC-BY-SA-4.0 licence. The published paper is available at \href{https://link.springer.com/article/10.3758/s13414-017-1389-3}{https://link.springer.com/article/10.3758/s13414-017-1389-3}.
NULL


#' @title Category knowledge and change detection
#' @description Infants (n=24) were divided into two experimental condition
#'   groups. One group (n=12) were shown objects of familiar categories (e.g.,
#'   balls, bottles, cars). The other group (n=12) were shown objects of
#'   unfamiliar categories (e.g., feather, guitar, hedgehog). In both groups, on
#'   each experimental trial, the object was occluded and when it reappeared it
#'   was either unchanged, changed for a different object of the same category,
#'   or change for an object of a different category. The research focus of the
#'   experiment was the extent to which the change (or lack of change) affected
#'   the amplitude of the negative central wave (Nc) event related potential
#'   (ERP), which is believed to be a signal of change detection in infants.
#' @name catknowledge
#' @format A data frame with 72 rows and 4 variables
#' \describe{
#'   \item{\code{id}}{(factor) Identifier of participant (a 12 month old infant) in experiment.}
#'   
#'   \item{\code{category}}{(factor) Between subjects experimental condition indicating if the
#'   objects were visible or occluded.}
#'
#'   \item{\code{change}}{(factor) The within-subjects experimental condition
#'   that describes how the stimulus object changes, with values `no` (no change
#'   to stimulus), `within` (the stimulus changes to another exemplar of the
#'   same category), `across` (the stimulus changes to another category).}
#'   
#'   \item{\code{nc_erp}}{(numeric) The negative central wave (Nc) event related
#'   potential (ERP) amplitude. The NC ERP is an ERP that occurs in infants at
#'   frontocentral sites following modifications in a stimulus's appearance.}
#'   }
#'
#' @details This data is from Experiment 1 in the paper "Nonverbal category
#'     knowledge limits the amount of information encoded in object
#'     representations: EEG evidence from 12-month-old infants" by Pomoiechowska
#'     and Gliga (2021). \insertCite{pomiechowska2021nonverbal}{isdsr}.
#' @references
#'   \insertRef{pomiechowska2021nonverbal}{isdsr}
#' @source The original raw data set is publicly available on the Open Science Foundation at \href{https://osf.io/652cp/}{https://osf.io/652cp/}.
NULL

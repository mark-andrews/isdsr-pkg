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
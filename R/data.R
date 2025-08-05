#' PISA test scores by country, year, and gender (2000–2022)
#'
#' A tidy summary of the OECD Programme for International Student Assessment
#' (PISA) student data. For every combination of survey year, country, and
#' gender, this dataset contains the mean mathematics, reading, and science
#' scores.
#'
#' @format A tibble with 1,020 rows and 7 variables:
#' \describe{
#'   \item{year}{Integer. PISA cycle year (e.g., 2000, 2003, …, 2022).}
#'   \item{country}{Character. Three‑letter country/jurisdiction code (ISO3 where possible; some PISA special codes).}
#'   \item{country_name}{Character. English country/jurisdiction name.}
#'   \item{gender}{Factor with levels \code{"female"} and \code{"male"}.}
#'   \item{math}{Numeric. Mean mathematics score for that group (OECD scale, mean ≈ 500, SD ≈ 100).}
#'   \item{read}{Numeric. Mean reading score.}
#'   \item{science}{Numeric. Mean science score.}
#' }
#'
#' @details
#' The dataset was created from the public PISA student microdata using the
#' \pkg{learningtower} package. We then aggregate
#' student-level scores with \code{dplyr::summarise()} after dropping records
#' with missing \code{year}, \code{country}, or \code{gender}.
#'
#' @source OECD. Programme for International Student Assessment (PISA)
#' public-use microdata; accessed via \pkg{learningtower}
#' (\url{https://github.com/kevinwang09/learningtower}).
#'
#' @references
#' OECD (various years). \emph{PISA Databases}. Paris: OECD.
#'
#' @seealso \code{\link{pisa2022uk}}
#'
#' @examples
#' summarize(pisa, across(math:science, mean), .by = year)
#'
#' @keywords datasets
"pisa"

#' PISA 2022 United Kingdom student-level scores
#'
#' Individual student records from the 2022 PISA cycle for the United Kingdom
#' (country code \code{"GBR"}). Contains mathematics, reading, and science
#' scores plus gender.
#'
#' @format A tibble with 12,972 rows and 4 variables:
#' \describe{
#'   \item{math}{Numeric. Mathematics score.}
#'   \item{read}{Numeric. Reading score.}
#'   \item{science}{Numeric. Science score.}
#'   \item{gender}{Factor with levels \code{"female"} and \code{"male"}.}
#' }
#'
#' @details
#' Rows with missing values in any of the four variables were removed.
#' Scores are the PISA plausible-value based estimates as provided by
#' \pkg{learningtower}.
#'
#' @source OECD PISA 2022 student file; subset obtained via
#' \pkg{learningtower} and filtered to \code{country == "GBR"}.
#'
#' @seealso \code{\link{pisa}} for aggregated country-by-year summaries.
#'
#' @examples
#' @keywords datasets
"pisa2022uk"


#' Visual versus Verbal Perception and Responses
#'
#' An experiment studying the interaction between visual versus perception and visual versus verbal responses.
#'
#' Subjects carried out two kinds of tasks. One task was visual (describing a
#' diagram), and the other was classed as verbal (reading and describing a
#' sentence sentences). They reported the results either by pointing (a "visual"
#' response), or speaking (a verbal response). Time to complete each task was
#' recorded in seconds.
#'
#' @name vizverb
#' @docType data
#' @format A data frame with 80 observations on the following 5 variables.
#' \describe{
#' \item{subject}{Subject identifying number (\code{s1} to \code{s20})}
#' \item{task}{Describe a diagram (`visual`) or a sentence (`verbal`)}
#' \item{response}{Point response (`visual`) or say response (`verbal`)}
#' \item{time}{Response time (in seconds)}
#' }
#' @source This data set was taken from the
#'   \href{https://cran.r-project.org/package=Stat2Data}{`Stats2Data`
#'   R package}. From the description in that package, the original data appear
#'   to have been collected in a Mount Holyoke College psychology class based
#'   replication of an experiment by Brooks, L., R. (1968) "Spatial and verbal
#'   components of the act of recall," Canadian J. Psych. V 22, pp. 349 - 368.
#' @keywords datasets
"vizverb"


#' Faithfulness from a Photo?
#'
#' Ratings from a facial photo and actual faithfulness.
#'
#' College students were asked to look at a photograph of an opposite-sex adult
#' face and to rate the person, on a scale from 1 (low) to 10 (high), for
#' attractiveness. They were also asked to rate trustworthiness, faithfulness,
#' and sexual dimorphism (i.e., how masculine a male face is and how feminine a
#' female face is). Overall, 68 students (34 males and 34 females) rated 170
#' faces (88 men and 82 women).
#'
#' @name faithfulfaces
#' @docType data
#' @format A data frame with 170 observations on the following 7 variables.
#'   \describe{
#'   \item{sex_dimorph}{Rating of sexual dimorphism (masculinity for males, femininity for females)}
#'   \item{attractive}{Rating of attractiveness}
#'   \item{cheater}{Was the face subject unfaithful to a partner?}
#'   \item{trustworthy}{Rating of trustworthiness}
#'   \item{faithful}{Rating of faithfulness}
#'   \item{face_sex}{Sex of face (female or male)}
#'   \item{rater_sex}{Sex of rater (female or male)}
#'   }
#' @source This data set was taken from the
#'   \href{https://cran.r-project.org/package=Stat2Data}{`Stats2Data`
#'    R package}. From the description in that package, the original is based on
#'   G. Rhodes et al. (2012), "Women can judge sexual unfaithfulness from
#'   unfamiliar men's faces," Biology Letters, November 2012. All of the 68
#'   raters were heterosexual Caucasians, as were the 170 persons who were
#'   rated. (We have deleted 3 subjects with missing values and 16 subjects who
#'   were over age 35.)
#' @keywords datasets
"faithfulfaces"


#' Age of Onset of Schizophrenia Data
#'
#'
#' Data on sex differences in the age of onset of schizophrenia.
#'
#'
#' A sex difference in the age of onset of schizophrenia was noted by Kraepelin
#' (1919). Subsequently epidemiological studies of the disorder have
#' consistently shown an earlier onset in men than in women. One model that has
#' been suggested to explain this observed difference is known as the subtype
#' model which postulates two type of schizophrenia, one characterised by early
#' onset, typical symptoms and poor premorbid competence, and the other by late
#' onset, atypical symptoms, and good premorbid competence.  The early onset
#' type is assumed to be largely a disorder of men and the late onset largely a
#' disorder of women.
#'
#' @name schizophrenia
#' @docType data
#' @format A data frame with 251 observations on the following 2 variables.
#' \describe{
#'    \item{age}{Age at the time of diagnosis.}
#'    \item{gender}{A categorical variable with values `female` and `male`}
#' }
#' @source This data set was taken from the
#'   \href{https://cran.r-project.org/package=HSAUR}{`HSAUR` R
#'   package}. From the description in that package, the original is E.
#'   Kraepelin (1919), \emph{Dementia Praecox and Paraphrenia}.  Livingstone,
#'   Edinburgh.
#' @keywords datasets
"schizophrenia"



#' Country–year panel of life-satisfaction and income (2011 – 2024)
#'
#' `owidwhr` combines the *Cantril ladder* measure of life-satisfaction
#' (published in the *World Happiness Report*) with GDP per capita (PPP,
#' constant 2021 international $) for every country in the Our World in Data
#' “Self-reported life satisfaction vs GDP per capita” data package.
#' The raw CSV was downloaded on **5 July 2025** and processed in
#' `data-raw/whr2025.R`: rows with missing values or the aggregate
#' observation “World” were removed and the log of GDP was added.
#'
#' @format A tibble with **1 765 rows** (country–year observations) and **6 variables**
#' \describe{
#'   \item{country}{Country or territory name (OWID *Entity* field).}
#'   \item{iso3c}{Three-letter ISO-3166 country code.}
#'   \item{year}{Calendar year, 2011 – 2024.}
#'   \item{happiness}{Mean Cantril-ladder score, 0 = worst possible life … 10 = best.}
#'   \item{gdp}{GDP per capita, PPP-adjusted, constant-2021 international $.}
#'   \item{lgdp}{Logarithm, base 10, of `gdp`.}
#' }
#'
#' @source Wellbeing Research Centre (2025) &ndash; *World Happiness Report 2025*
#'         and World Bank (2025) &ndash; *World Development Indicators*; processed by
#'         Our World in Data, <https://ourworldindata.org/>, see <https://ourworldindata.org/grapher/gdp-vs-happiness>.
#'
#' @examples
#' # Correlation between happiness and (log) income over time
#' library(dplyr)
#' owidwhr %>%
#'   group_by(year) %>%
#'   summarise(r = cor(happiness, lgdp)) %>%
#'   arrange(desc(r))
"owidwhr"

#' World Happiness Report 2025 cross-section (survey year 2023)
#'
#' `whr2025` is a single-year slice of \code{\link{owidwhr}}, keeping the observations
#' labelled **2023** that feed the headline ranking in the *World Happiness
#' Report 2025*.  An ordered factor `income` classifies countries into GDP
#' terciles (*low < medium < high*) for quick, colour-friendly graphics.
#'
#' @format A tibble with **139 rows** (one per country) and **6 variables**
#' \describe{
#'   \item{country}{Country or territory name.}
#'   \item{iso3c}{Three-letter ISO-3166 country code.}
#'   \item{happiness}{Average Cantril-ladder score for 2022 – 2024, reported by WHR as 2023.}
#'   \item{gdp}{GDP per capita, PPP, constant-2021 international $.}
#'   \item{lgdp}{Logarithm, base 10, of `gdp`.}
#'   \item{income}{Ordered factor with three levels: `low`, `medium`, `high`
#'                 (terciles of `gdp`).}
#' }
#'
#'
#' @source See \code{\link{owidwhr}}.
#'
#' @examples
#' library(ggplot2)
#' ggplot(whr2025, aes(lgdp, happiness, colour = income)) +
#'   geom_point() +
#'   geom_smooth(se = FALSE, method = "lm")
"whr2025"




#' @title The effect of a mindfulness course on stress scores
#'
#' @name mindfulness
#'
#' @description
#' A small longitudinal data set ( *n* = 100) that tracks perceived stress before
#' and after completion of a 6-week online mindfulness course.
#' Each participant completed the 10-item Perceived Stress Scale (PSS; range
#' 0–40, higher = more stress) three times:
#' * immediately before starting the course,
#' * immediately after finishing the course,
#' * one month later.
#' Gender was recorded at enrolment; no other personal identifiers are present.
#'
#' @format A tibble with 100 rows and 5 variables:
#' \describe{
#'   \item{\code{id}}{(factor) Anonymous participant identifier (1–100).}
#'   \item{\code{gender}}{(factor) Self-reported gender (\code{"Male"} /\code{"Female"}).}
#'   \item{\code{pss_before}}{(numeric) PSS score \emph{before} the course.}
#'   \item{\code{pss_after}}{(numeric) PSS score \emph{immediately after} the course.}
#'   \item{\code{pss_followup}}{(numeric) PSS score \emph{one month} after course completion.}
#' }
#'
#' @details
#' The data originate from Krusche *et al.* (2012),
#' “Mindfulness online: a preliminary evaluation of the feasibility of a
#' web-based mindfulness course and the impact on stress,” *BMJ Open* 2:e000803.
#' Participants self-referred to the course and
#' completed the PSS online.
#' In the published paper the authors report a significant reduction in stress
#' from baseline to post-course that remained stable at follow-up.
#' The original data XLSX file was downloaded from the Dryad repository (DOI
#' 10.5061/dryad.f4688) on 29 June 2025 and lightly cleaned: only the columns
#' shown above were kept, names were standardised, and an \code{id} column was
#' added.
#'
#' @source
#' Krusche A, Cyhlarova E, King S, Williams J MG (2012).
#' *Mindfulness online: a preliminary evaluation of the feasibility of a
#' web-based mindfulness course and the impact on stress.*
#' \emph{BMJ Open} 2:e000803.
#' Raw data file retrieved from Dryad: <https://doi.org/10.5061/dryad.f4688>.
NULL



#' @title World Development Indicators (1990 – 2024) panel
#'
#' @name wdi_panel
#' @description
#' `wdi_panel` is a country–year panel of some economic or development
#'  statistics (1990–2024).  Indicators come from the **World Development
#'  Indicators (WDI)** web-API maintained by the World Bank. This data was pulled with
#'  the **WDI** R package.
#'
#' @format A tibble with 6582 rows (one per country × year) and 8 variables:
#' \describe{
#'   \item{\code{country}}{English name of the country.}
#'   \item{\code{iso2c}}{ISO-3166 alpha-2 country code (e.g. “US”).}
#'   \item{\code{year}}{Calendar year of observation.}
#'   \item{\code{gdp}}{Gross Domestic Product, PPP-adjusted, in millions of constant 2017 international dollars (total economy size, millions of 2017 international $).}
#'   \item{\code{gdp_growth}}{Real GDP growth rate (% change from previous year).}
#'   \item{\code{inflation}}{Consumer-price inflation (annual %).}
#'   \item{\code{lfp_male}}{Male labour-force participation rate (%).}
#'   \item{\code{lfp_female}}{Female labour-force participation rate (%).}
#' }
#'
#' @details
#' Downloaded via World Bank data API on **30 June 2025** using `data-raw/wdi_panel.R`.
#'
#' @source World Bank (2025), *World Development Indicators* (database),
#' retrieved 27 Jun 2025.  CC BY 4.0. <https://databank.worldbank.org/source/world-development-indicators>
#'
#' @seealso The \code{data-raw/wdi_panel.R} script in the source code of this
#' package was used to download the data using the World Bank's API.
NULL


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
#' }
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


#' Anthropometric data from US Army Personnel
#'
#' Data on the height, weight, handedness from men and women of different ages and different races.
#'
#' @name ansur
#' @docType data
#' @format A data frame with 6068 observations from 9 variables.
#' \describe{
#'    \item{subjectid}{Unique ID of the person}
#'    \item{gender}{Binary variable indicating the subject's sex: `male` or `female`.}
#'    \item{height}{Height in centimeters.}
#'    \item{weight}{Weight in kilograms.}
#'    \item{handedness}{Categorical variable indicating if the person is left, or right handed, or both.}
#'    \item{age}{Age in years}
#'    \item{race}{Race, with categories like `white`, `black`, `hispanic`.}
#'    \item{height_tercile}{The tercile of the person's height.}
#'    \item{age_tercile}{The tercile of the person's weight.}
#' }
#' @source This data is a transformed version of data sets obtained the \href{https://www.openlab.psu.edu/ansur2/}{Anthropometric Survey of US Army Personnel (ANSUR 2 or ANSUR II)}.
NULL


#' World Happiness Report 2024 Data
#'
#' A dataset comprising the 2024 country-level happiness scores and associated predictor variables,
#' based on the Gallup World Poll and other international data sources, as used in the 2024 World Happiness Report.
#'
#' @format A tibble with 131 rows and 9 variables:
#' \describe{
#'   \item{country}{Country name.}
#'   \item{happiness}{Average national response to the Cantril life ladder question, which asks respondents to evaluate their current life on a scale from 0 (worst possible life) to 10 (best possible life). This is the main measure of subjective well-being.}
#'   \item{gdp}{GDP per capita in purchasing power parity (PPP), constant 2017 international dollars. Sourced primarily from the World Bank World Development Indicators and Penn World Table.}
#'   \item{lgdp}{log10 of gdp}
#'   \item{support}{National average of responses (0 or 1) to the Gallup World Poll (GWP) question: "If you were in trouble, do you have relatives or friends you can count on to help you whenever you need them, or not?"}
#'   \item{hle}{Healthy life expectancy at birth, measured in years. Based on WHO estimates for 2000, 2010, 2015, and 2019, with interpolation and extrapolation used to match the report’s period.}
#'   \item{freedom}{National average of responses to the GWP question: "Are you satisfied or dissatisfied with your freedom to choose what you do with your life?"}
#'   \item{generosity}{A measure of prosocial behavior, defined as the residual from a regression of the national average response to the GWP question "Have you donated money to a charity in the past month?" on GDP per capita.}
#'   \item{corruption}{Perception of corruption, defined as the average of two binary (0 or 1) GWP questions: "Is corruption widespread throughout the government or not?" and "Is corruption widespread within businesses or not?" If one is missing, the available one is used.}
#' }
#'
#' @details
#' The dataset was constructed by extracting figures from the World Happiness Report 2024 Statistical Appendix.
#' GDP, life expectancy, and other predictors are sourced or derived from international data repositories and surveys.
#' Happiness scores are based on "Data for Figure 2.1" for the 2024 report \url{https://files.worldhappiness.report/WHR24_Data_Figure_2.1.xlx}.
#'
#' @source \url{https://www.worldhappiness.report/ed/2024/}
#'
#' @examples
#' summary(whr2024)
"whr2024"

#' 2021 General Social Survey subset
#'
#' A tibble drawn from the 2021 cross-section of the US
#' General Social Survey (GSS). The data frame keeps just a few variables that
#' are useful for investing predictors of personal earnings (`income`).
#'
#' @format A tibble with 2444 rows and 10 variables:
#' \describe{
#'   \item{`income`}{\code{double}. Respondent’s \emph{personal} earnings in
#'     \strong{constant 1986 US dollars} (variable \code{REALRINC} in the
#'     original GSS). Zero and negative values were
#'     dropped.}
#'   \item{`lincome`}{\code{double}. \code{log10(income)} – a log-scaled
#'     version that is closer to normal and is the recommended dependent
#'     variable for normal-linear models.}
#'   \item{`degree`}{\code{factor} with 5 levels
#'     \code{"<HS"}, \code{"HS"}, \code{"JrColl"}, \code{"BA"},
#'     \code{"Grad"}; highest educational credential (GSS variable
#'     \code{DEGREE}).}
#'   \item{`sex`}{\code{factor} with levels \code{"Male"}, \code{"Female"}
#'     (GSS \code{SEX}).}
#'   \item{`age`}{\code{double}. Age in years (\code{AGE}).}
#'   \item{`marital`}{\code{factor}. Marital status with the original GSS
#'     labels (\code{MARITAL}).}
#'   \item{`hours`}{\code{double}. Usual weekly hours of work
#'     (\code{HRS1}); missing when not working.}
#'   \item{`prestige`}{\code{double}. Occupational prestige score
#'     (\code{PRESTG10}; range 16–86).}
#'   \item{`childs`}{\code{double}. Number of children ever had
#'     (\code{CHILDS}).}
#'   \item{`fulltime`}{\code{factor} with levels \code{"Full-time"},
#'     \code{"Other"}, derived from \code{WRKSTAT}.  “Full-time” corresponds
#'     to \code{WRKSTAT == 1}.}
#' }
#'
#' @details
#' \itemize{
#'   \item Only respondents with non-missing \code{income}, \code{degree},
#'         \code{sex}, and \code{age} are kept.
#'   \item The original “dbl + lbl” (labelled) SPSS columns were converted
#'         to plain numerics or factors with \code{haven::as_factor()}.
#'   \item Constant-dollar scaling means you can compare earnings across
#'         survey years if you wish; for interpretations in 2021 dollars
#'         multiply by \eqn{270.97 / 109.6 \approx 2.47} (CPI-U ratio).
#' }
#'
#' @source NORC at the University of Chicago, \emph{General Social
#'   Survey 1972–2024}: \url{https://gss.norc.org}.  SPSS release
#'   \code{gss7224_r1.sav}; filtered and recoded by the package author
#'   (see \code{data-raw/gss2021.R}).
#'
#' @references
#' Smith, Tom W., Peter V. Marsden, Michael Hout, and Jibum Kim. 2022.
#' *General Social Survey, 1972–2021*. NORC at the University of Chicago.
#'
#' @examples
#' data(gss2021)
#' # Gender × education on log income
#' summary(aov(lincome ~ sex * degree, data = gss2021))
#'
#' # Add age as a covariate (ANCOVA)
#' summary(aov(lincome ~ sex * degree + age, data = gss2021))
"gss2021"

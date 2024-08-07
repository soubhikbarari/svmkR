% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_survey.R
\name{code_race_vars}
\alias{code_race_vars}
\title{Code demographic variables in responses}
\usage{
code_race_vars(data, append = TRUE, verbose = TRUE)
}
\arguments{
\item{data}{Input data frame of survey responses.}

\item{append}{If TRUE, append to existing dataframe otherwise return new variables in a fresh dataframe.}

\item{verbose}{Print helpful messages.}
}
\value{
Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{race and/or ethnicity} \itemize{
 \item \code{race5}: 5-point standard Census race category (i.e. Non-Hispanic White, Non-Hispanic Black, Hispanic, Asian, Other)
 \item \code{race4}: 4-point categorization collapsing "Asian" into "Other" (needed for some weighting schemes)
 \item \code{nonwhite}: 2-point race category for white/non-white
}
}
\description{
Given a dataframe of survey responses, look for standard SurveyMonkey demographic questions and code/recode them into new variables that are more banner-, plot-, and analysis-friendly. Use \code{code_std_demo_vars()} to perform all standard demographic recodes, or select the specific function you need. Run \code{std_demo_vars()} to return the full index of SurveyMonkey's standard demographic variables.
}

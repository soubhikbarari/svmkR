% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_survey.R
\name{code_std_demo_vars}
\alias{code_std_demo_vars}
\title{Code demographic variables in responses}
\usage{
code_std_demo_vars(data, append = TRUE, verbose = TRUE)
}
\arguments{
\item{data}{Input data frame of survey responses.}

\item{append}{If TRUE, append to existing dataframe otherwise return new variables in a fresh dataframe.}

\item{verbose}{Print helpful messages.}
}
\value{
Survey data frame with new (or updated) columns corresponding to all discoverable demographic variables for respondents.
}
\description{
Given a dataframe of survey responses, look for standard SurveyMonkey demographic questions and code/recode them into new variables that are more banner-, plot-, and analysis-friendly. Use \code{code_std_demo_vars()} to perform all standard demographic recodes, or select the specific function you need. Run \code{std_demo_vars()} to return the full index of SurveyMonkey's standard demographic variables.
}

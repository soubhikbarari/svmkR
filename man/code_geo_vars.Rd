% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_survey.R
\name{code_geo_vars}
\alias{code_geo_vars}
\title{Code demographic variables in responses}
\usage{
code_geo_vars(data, append = TRUE, verbose = TRUE)
}
\arguments{
\item{data}{Input data frame of survey responses.}

\item{append}{If TRUE, append to existing dataframe otherwise return new variables in a fresh dataframe.}

\item{verbose}{Print helpful messages.}
}
\value{
Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{geographic location (currently only in the U.S.)}: \itemize{
 \item \code{zip}: 6-digit ZIP code
 \item \code{urban4}: 4-point urban/rural classification for ZIP code based on the USDA's 2010 RUCA (rural-urban commuting area) coding (Metropolitan, Micropolitan, Small town, Rural)
 \item \code{zip}: 6-digit ZIP code
 \item \code{state_name}: state name
 \item \code{state_abb}: state abbreviation
 \item \code{region}: U.S. Census region
 \item \code{division}: U.S. Census division
}
}
\description{
Given a dataframe of survey responses, look for standard SurveyMonkey demographic questions and code/recode them into new variables that are more banner-, plot-, and analysis-friendly. Use \code{code_std_demo_vars()} to perform all standard demographic recodes, or select the specific function you need. Run \code{std_demo_vars()} to return the full index of SurveyMonkey's standard demographic variables.
}

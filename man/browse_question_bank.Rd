% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/browse_surveys.R
\name{browse_question_bank}
\alias{browse_question_bank}
\title{Browse user's question bank}
\usage{
browse_question_bank(oauth_token = get_token())
}
\arguments{
\item{oauth_token}{Your OAuth 2.0 token. By default, retrieved from \code{get_token()}.}
}
\value{
A dataframe of survey questions available to the user.
}
\description{
Retrieve the survey question bank available to the user.
}
\details{
This function calls the SurveyMonkey API using the current OAuth token and returns
the bank of survey questions available to the user.
}
\references{
SurveyMonkey API V3 at \url{https://developer.surveymonkey.com/api/v3/#api-endpoints-get-survey_templates}.
}

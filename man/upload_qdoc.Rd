% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_survey.R
\name{upload_qdoc}
\alias{upload_qdoc}
\title{Upload processed questionnaire to SurveyMonkey}
\usage{
upload_qdoc(
  qdoc,
  title,
  from_template_id = NULL,
  from_survey_id = NULL,
  from_team_template_id = NULL,
  nickname = NULL,
  language = "en",
  folder_id = NULL,
  theme_id = NULL,
  ...,
  oauth_token = get_token()
)
}
\arguments{
\item{qdoc}{\code{qdoc} object for survey questionnaire (e.g. via \code{read_qdoc}) to be uploaded.}

\item{title}{Name for survey in uploaded account.}

\item{from_template_id}{Survey template to copy from, see \code{browse_templates} (optional).}

\item{from_survey_id}{Survey id to copy from, see \code{browse_surveys} (optional).}

\item{from_team_template_id}{Team survey template to copy from, see \code{browse_templates} (optional).}

\item{nickname}{Survey nickname (optional).}

\item{language}{Survey language, default is 'en'.}

\item{folder_id}{Adds the survey to the folder with that id (optional).}

\item{theme_id}{Creates the survey using the theme provided (optional).}

\item{...}{Any additional parameters for upload POST call (see API reference).}

\item{oauth_token}{Your OAuth 2.0 token. By default, retrieved from \code{get_token()}.}
}
\value{
Upload status and metadata (e.g. assigned survey ID, edit URL) as a dataframe.
}
\description{
Takes a processed \code{qdoc} object and uploads to SurveyMonkey
account associated with token with selected title, location, and format.
}
\examples{
if (FALSE) { ## not run
data(qdocs)

set_token("XXXXXXX")

my_qdoc <- read_qdoc(text = qdoc.simple) 

upload_qdoc(my_qdoc, title = "My Survey")
}
}
\references{
SurveyMonkey \code{POST /survey} API V3 at \url{https://developer.surveymonkey.com/api/v3/#api-endpoints-post-surveys}.
}

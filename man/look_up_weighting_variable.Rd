% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weight_surveys.R
\name{look_up_weighting_variable}
\alias{look_up_weighting_variable}
\title{Look up weighting target variable}
\usage{
look_up_weighting_variable(target.var)
}
\arguments{
\item{target.var}{name of a potential target variable used in a target population profile (see \code{list_targets()}).}
}
\value{
dataframe of question-variable mappings associated with searched target variable.
}
\description{
Find all information (e.g. applicable survey questions, applicable levels) for a potential weighting target variable.
}

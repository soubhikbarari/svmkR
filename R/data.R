#' SVMK Electric Vehicles Survey 2022 Study
#'
#' Simulated data based on a real market research study on Electric Vehicles conducted by the SurveyMonkey Research Insights Team.
#'
#' @format
#' A simulated data frame of partially coded survey responses with 1000 rows and 31 columns:
#' \describe{
#'   \item{gender}{Gender (standard research team format)}
#'   \item{age}{Age (standard research team format)}
#'   \item{education}{Education (standard research team format)}
#'   \item{incomse}{Income (standard research team format)}
#'   \item{vehage}{"How old is your main vehicle?"}
#'   \item{vehcost}{"In total, how much did you pay to buy or lease your main vehicle?"}
#'   \item{purchase}{"How did you purchase or lease your main vehicle?"}
#'   \item{ev_heard_*}{"Which of the following electric vehicles have you heard of?" (select all that apply)}
#'   \item{mech_factors_*}{"Select the top 3 factors that are most important to you when selecting an auto repair provider:" (select all that apply)}
#'   \item{sub_services_*}{"Which of the following services would you be willing to pay a recurring subscription for in a car?"  (select all that apply)}
#'   ...
#' }
"ev22"

#' SVMK Social Media Usage 2023 Study
#'
#' Simulated data based on a real tracker study conducted by the SurveyMonkey Research Insights Team on social media usage and other behaviors/attitudes.
#'
#' @format
#' A simulated data frame of raw survey responses with 4000 rows and 81 columns with the following identifying variables for each respondent (mostly redacted):
#' \describe{
#'   \item{collector_id}{Survey collector ID (redacted)}
#'   \item{collection_mode}{Survey collection mode}
#'   \item{survey_id}{Survey ID (redacted)}
#'   \item{response_id}{Unique respondent ID (redacted)}
#'   \item{response_status}{Response staus}
#'   \item{date_created}{Date response started}
#'   \item{date_modified}{Date response last modified}
#'   \item{first_name}{Respondent's first name (redacted)}
#'   \item{last_name}{Respondent's last name (redacted)}
#'   \item{email_address}{Respondent's email address (redacted)}
#'   \item{ip_address}{Respondent's IP address (redacted)}
#'   ...
#' }
"smda23"
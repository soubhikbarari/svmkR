#' browse_surveys
#'
#' Retrieve a dataframe of the user's surveys.
#'
#' This function calls the SurveyMonkey API using the current OAuth token and returns
#' a dataframe of surveys filtered by the parameters entered.
#'
#' @param per_page Integer number to set the number of surveys to return per page.
#' Maximum value is 1000 surveys per page; try that if your survey is not on the first 100,
#' to reduce API calls.
#' @param page Integer number to select which page of resources to return. By default is 1.
#' @param sort_by String used to sort returned survey list:
#' ‘title’, 'date_modified’, or 'num_responses’. By default, date_modified.
#' @param sort_order String used to set the sort order for returned surveys:
#' 'ASC’ or 'DESC’. By default, DESC.
#' @param start_modified_at Date string used to select surveys last modified after this date.
#' By default is NULL.
#' @param end_modified_at Date string used to select surveys modified before this date.
#' By default is NULL.
#' @param title String used to select survey by survey title.  By default is NULL.
#' @param include Character vector as a comma separated string used to filter survey list:
#' 'response_count’, 'date_created’, 'date_modified’, 'language’,
#' 'question_count’, 'analyze_url’, 'preview’.
#' By default is NULL. Use \code{browse_surveys('everything')} to pull all fields.
#' @param folder_id Specify the id of a folder to only return surveys in it.
#' @param oauth_token Your OAuth 2.0 token.
#' By default, retrieved from \code{get_token()}.
#'
#' @return A dataframe of surveys and associated metadata from desired account.
#'
#' @references SurveyMonkey API V3 at \url{https://developer.surveymonkey.com/api/v3/#surveys}.
#'
#' @importFrom rlang .data
#'
#' @export
browse_surveys <- function(per_page = 100,
                           page = NULL,
                           sort_by = NULL,
                           sort_order = NULL,
                           start_modified_at = NULL,
                           end_modified_at = NULL,
                           title = NULL,
                           include = NULL,
                           folder_id = NULL,
                           oauth_token = get_token()) {

  u <- "https://api.surveymonkey.com/v3/surveys?"
  h <- standard_request_header(oauth_token)

  start_modified_at <- format_date(start_modified_at)
  end_modified_at <- format_date(end_modified_at)

  b <- list(
    page = page,
    per_page = per_page,
    sort_by = sort_by,
    sort_order = sort_order,
    start_modified_at = start_modified_at,
    end_modified_at = end_modified_at,
    title = title,
    include = include,
    folder_id = folder_id
  )
  nulls <- sapply(b, is.null)
  if (all(nulls)) {
    b <- NULL
  } else {
    b <- b[!nulls]
  }

  if (!is.null(b$include)) {
    b$include <- paste(b$include, collapse = ",")

    if (b$include == "everything") {
      b$include <- paste(c(
        "response_count",
        "date_created",
        "date_modified",
        "language",
        "question_count",
        "analyze_url",
        "preview"
      ),
      collapse = ","
      )
    }
  }

  if (!is.null(b)) {
    parsed_content <- sm_get(url = u, query = b, config = h)
    sl <- dplyr::bind_rows(parsed_content$data)
    dplyr::select(
      sl,
      .data$title, .data$id, url = .data$href, .data$nickname,
      tidyselect::everything()
    )
  } else {
    stop("all query inputs are NULL. see ?browse_surveys for input details.")
  }
}

#' browse_templates
#'
#' Retrieve a dataframe of survey templates available to the user.
#'
#' This function calls the SurveyMonkey API using the current OAuth token and returns
#' a dataframe of survey template available to the user.
#'
#' @param oauth_token Your OAuth 2.0 token. By default, retrieved from \code{get_token()}.
#'
#' @return A dataframe of survey templates and associated metadata from desired account.
#'
#' @references SurveyMonkey API V3 at \url{https://developer.surveymonkey.com/api/v3/#api-endpoints-get-survey_templates}.
#'
#' @importFrom rlang .data
#'
#' @export
browse_templates <- function(oauth_token = get_token()) {
  u <- "https://api.surveymonkey.com/v3/survey_templates"
  h <- standard_request_header(oauth_token)
  parsed_content <- sm_get(url = u, query = NULL, config = h)
  sl.user <- dplyr::bind_rows(parsed_content$data)

  return(sl.user)
}

#' browse_team_templates
#'
#' Retrieve a dataframe of survey templates available to the user's team.
#'
#' This function calls the SurveyMonkey API using the current OAuth token and returns
#' a dataframe of survey template available to the user's team.
#'
#' @param oauth_token Your OAuth 2.0 token. By default, retrieved from \code{get_token()}.
#'
#' @return A dataframe of team survey templates and associated metadata from desired account.
#'
#' @references SurveyMonkey API V3 at \url{https://developer.surveymonkey.com/api/v3/#api-endpoints-get-team_survey_templates}.
#'
#' @importFrom rlang .data
#'
#' @export
browse_team_templates <- function(oauth_token = get_token()) {
  u <- "https://api.surveymonkey.com/v3/team_survey_templates"
  h <- standard_request_header(oauth_token)
  parsed_content <- sm_get(url = u, query = NULL, config = h)
  sl.team <- dplyr::bind_rows(parsed_content$data)
  
  return(sl.team)
}


#' browse_question_bank
#'
#' Retrieve the survey question bank available to the user.
#'
#' This function calls the SurveyMonkey API using the current OAuth token and returns
#' the bank of survey questions available to the user.
#'
#' @param oauth_token Your OAuth 2.0 token. By default, retrieved from \code{get_token()}.
#'
#' @return A dataframe of survey questions available to the user.
#'
#' @references SurveyMonkey API V3 at \url{https://developer.surveymonkey.com/api/v3/#api-endpoints-get-survey_templates}.
#'
#' @importFrom rlang .data
#'
#' @export
browse_question_bank <- function(oauth_token = get_token()) {
  u <- "https://api.surveymonkey.com/v3/question_bank/questions"
  h <- standard_request_header(oauth_token)
  parsed_content <- sm_get(url = u, query = NULL, config = h)
  
  sl <- dplyr::bind_rows(lapply(parsed_content$data, function(.) {
    list(question_id=.$question_id, text=.$text)
  }))
  return(sl)
}


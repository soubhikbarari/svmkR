#' Download a survey's responses
#'
#' @param id ID number of survey to be fetched.
#' @param oauth_token Your OAuth 2.0 token.
#' By default, retrieved from \code{get_token()}.
#'
#' @return a survey object, which is a nested list containing info about the survey.
#' @export
#'
#' @examples
#' # not run:
#' # fetch_survey_obj(123456789)
fetch_survey_obj <- function(id,
                             oauth_token = get_token()) {
  if (missing(id)) {
    stop("specify an id")
  }
  
  u <- "https://api.surveymonkey.com/v3/surveys?"
  h <- standard_request_header(oauth_token)
  p <- list("v3", survey = "surveys", id = id, details = "details")
  
  parsed_content <- sm_get(url = u, query = NULL, config = h, path = p)
  
  message("+ Parsing questions 📋")
  families  = list()
  subtypes  = list()
  questions = list()
  answers   = list()
  choices   = list()
  pbar <- utils::txtProgressBar(min=0, max=length(parsed_content))
  p <- 0
  while (p < length(parsed_content$pages)) {
    p <- p + 1
    page <- parsed_content$pages[[p]]
    utils::setTxtProgressBar(pbar, p)
    for (question in page$questions) {
      questions[[question$id]] <- strip_tags(question$headings[[1]]$heading)
      families[[question$id]]  <- question$family
      subtypes[[question$id]]  <- question$subtype
      choices[[question$id]]   <- c()
      
      if ("answers" %in% names(question)) {
        if ("rows" %in% names(question$answers)) {
          for (row in question$answers$rows) {
            answers[[row$id]] <- trimws(row$text)
          }      
        }
        if ("choices" %in% names(question$answers)) {
          for (choice in question$answers$choices) {
            answers[[choice$id]] <- trimws(choice$text)
            choices[[question$id]] <- c(choices[[question$id]], answers[[choice$id]])
          }
        }
        if ("other" %in% names(question$answers)) {
          answers[[question$answers$other$id]] <- trimws(question$answers$other$text)
        }
      }
    }
  }
  parsed_content$families   = families
  parsed_content$subtypes   = subtypes
  parsed_content$questions  = questions
  parsed_content$answers    = answers
  parsed_content$choices    = choices
  
  parsed_content
}

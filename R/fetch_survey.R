#' Download a survey's responses
#'
#' @param id ID number of survey to be fetched.
#' @param oauth_token Your OAuth 2.0 token.
#' By default, retrieved from \code{get_token()}.
#'
#' @return A survey object of type \code{qdoc}, which is a nested list containing info about the survey and underlying questionnaire. Note that you can print this object for an informative summary like you can any other qdoc object. 
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
  
  svy_obj <- sm_get(url = u, query = NULL, config = h, path = p)
  
  message("+ Parsing questions 📋")
  families  = list()
  subtypes  = list()
  questions = list()
  choices   = list()
  answers   = list()
  
  pbar <- utils::txtProgressBar(min=0, max=length(svy_obj))
  p <- 0
  while (p < length(svy_obj$pages)) {
    p <- p + 1
    page <- svy_obj$pages[[p]]
    utils::setTxtProgressBar(pbar, p)
    q <- 0
    while (q < length(page$questions)) {
      q <- q + 1
      question <- page$questions[[q]]
      
      families[[question$id]]  <- question$family
      subtypes[[question$id]]  <- question$subtype
      questions[[question$id]] <- strip_tags(question$headings[[1]]$heading)
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
      class(svy_obj$pages[[p]]$questions[[q]]) <- c(paste0("qdoc.", question$family), "list")
    }
  }
  svy_obj$qdoc       = qdoc
  svy_obj$families   = families
  svy_obj$subtypes   = subtypes
  svy_obj$questions  = questions
  svy_obj$choices    = choices
  svy_obj$answers    = answers
  class(svy_obj) <- c("qdoc", class(svy_obj))
  
  svy_obj
}

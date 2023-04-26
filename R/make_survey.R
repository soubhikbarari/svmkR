#' Process a questionnaire question
#'
#' Takes a question written in proper QDOC format and 
#' generates an object that is ready to be uploaded as a survey.
#' See \code{vignette("qdocs")} for a guide to the QDOC format.
#'
#' @param text text of question written in proper QDOC format.
#' @param quiet.fail if cannot parse question, silently return an empty list (default is FALSE).
#'
#' @return An object for a particular QDOC question family (e.g. \code{qdoc.single_choice}, \code{qdoc.multiple_choice}, \code{qdoc.matrix}).
#'
#' @export
#' 
#' @examples
#' # multiple-choice (single answer)
#' qdoc.question("What is your gender?\n\nMale\nFemale\nOther")
#' qdoc.question("[[Question:MC]]\nWhat is your gender?[[Choices]]\nMale\nFemale\nOther")
#'
#' # multiple-choice (multi-answer)
#' qdoc.question("What is your gender?[[MultiAnswer]]\n\nMale\nFemale\nOther")
#' qdoc.question("[[Question:MC:MultiAnswer]]\nWhat is your gender?\n[[Choices]]\nMale\nFemale\nOther")
#'
#' # matrix
#' qdoc.question("Please rate the following:\n\na\nb\nc\n\n1\n2\n3")
#' qdoc.question("[[Question:Matrix]]\nPlease rate the following:\n[[Rows]]\na\nb\nc\n[[Columns]]\n1\n2\n3")
qdoc.question <- function(text, 
                          quiet.fail = FALSE) {
  text <- trimws(text)
  
  ## replace any text specified by ID
  if (grepl("\\[\\[ID\\:", text, ignore.case=T)) {
    q.ids <- stringr::str_extract_all(text, "(?<=\\[\\[ID\\:)(\\d+)")[[1]]
    token <- get_token()
    if (nchar(token) == 0 | is.null(token)) {
      token <- readline(prompt=sprintf("Question specified with question bank ID (%s), but no OAuth token specified to access bank. Please paste a valid token below:", q.id))
      set_token(token)
    }
    q.bank <- browse_question_bank()
    for (q.id in q.ids) {
      if (!(q.id %in% q.bank$question_id)) {
        warning(sprintf("Question specified with question bank ID (%s), but not found in question bank.", q.id))
      } else {
        q.id.text <- q.bank$text[q.bank$question_id == q.id][1]
        q.id.tag <- paste0("\\[\\[ID\\:",q.id, "\\]\\]")
        text <- stringr::str_replace(text, q.id.tag, q.id.text)
      }
    }
  }
  
  q <- list()
  
  ## go down format hierarchy
  ## (i.e., try matrix first, then try multiple choice, etc.)
  if (is_matrix(text)) {
    
    class(q) <- c("qdoc.matrix", "list")
    q$family <- "matrix"
    q$subtype <- get_subtype(text, q$family)
    q$headings <- list(get_heading(text))
    q$forced_ranking <- FALSE ## curently don't support option to change this (yet)
    q$answers <- list()
    q$answers$rows <- get_rows(text)
    if (q$subtype == "rating") {
      ## currently don't support unequal weights, though 
      ## could by introducing a within-choice tag e.g. "[[Weight:10]]"
      q$answers$choices <- lapply(get_cols(text), function(.) modifyList(., list(weight=1))) 
      # } else if (q$subtype == "menu") {
      # ## currently don't support 'matrix - menu' questions due to nested choices required
      #  q$answers$cols <- get_cols(text)
    } else {
      q$answers$choices <- get_cols(text)
    } 
    #}  else if (is_datetime(text)) {
    # ## currently don't support 'datetime' questions due to nested text format required
    #  q$family <- "datetime"
    #  q$subtype <- get_subtype(text, q$family)
    #  q$headings <- list(get_heading(text))
    #  
  } else if (is_multiple_choice(text)) {
    
    class(q) <- c("qdoc.multiple_choice", "list")
    q$family <- "multiple_choice"
    q$subtype <- get_subtype(text, q$family)
    q$headings <- list(get_heading(text))
    q$answers <- list()
    q$answers$choices <- get_choices(text)
    
  } else if (is_single_choice(text)) {
    
    class(q) <- c("qdoc.single_choice", "list")
    q$family <- "single_choice"
    q$subtype <- get_subtype(text, q$family)
    q$headings <- list(get_heading(text))
    q$answers$choices <- get_choices(text)
    
  } else if (is_open_ended(text)) {
    
    class(q) <- c("qdoc.open_ended", "list")
    q$family <- "open_ended"
    q$subtype <- get_subtype(text, q$family)
    q$headings <- list(get_heading(text))
    
  } else {
    q <- list()
    if (!quiet.fail) {
      stop("couldn't discern question family")
    }
  }
  
  q <- modifyList(q, get_question_params(text))
  
  return(q)
}

#' @rdname qdoc.question
#' @export
as.qdoc.question <- qdoc.question

#' @rdname qdoc.question
#' @export
qdoc.q <- qdoc.question

#' Process a questionnaire
#'
#' Takes a questionnaire written in proper QDOC format and 
#' generates an object that is ready to be uploaded as a survey.
#' See \code{vignette("qdocs")} for a guide to the QDOC format.
#'
#' @param file.path Path of questionnaire written in proper QDOC format.
#' @param text Character string written in in proper QDOC format.
#' @param oauth_token Your OAuth 2.0 token (only needed if questionnaire references question bank). By default, retrieved from get_token().
#' @return Output of class \code{qdoc}.
#'
#' @export
#'
#' @examples
#' data(qdocs)
#'
#' set_token("XXXXXXX")
#' 
#' read_qdoc(text = qdoc.simple) # simple format
#' read_qdoc(text = qdoc.adv)    # advanced format
#' 
read_qdoc <- function(file.path = NULL, 
                      text = NULL, 
                      oauth_token = get_token()) {
  # Process a questionnaire written in the Qualtrics TXT format
  # into the format (qdoc) required for an SVMK API call.
  
  set_token(oauth_token)
  
  qdoc <- list()
  class(qdoc) <- c("qdoc", class(qdoc))
  
  if (is.null(file.path) & is.null(text)) {
    stop("Must specify either `file.path` or `text`.")
  } else if (!is.null(file.path)) {
    qdoc.text <- paste0(readLines(file.path), collapse="\n")
  } else {
    qdoc.text <- text
  }
  attr(qdoc, "text") <- qdoc.text
  
  qualtrics.adv <- grepl("\\[\\[AdvancedFormat\\]\\]", qdoc.text)
  
  # delimit pages or blocks
  delim.tags <- c("Block","block","BLOCK",
                  "Page","page","PAGE",
                  "PageBreak","Pagebreak","pagebreak","PAGEBREAK")
  
  delim <- paste0("[\r\n]+(?=(", paste0(sprintf("\\[\\[%s\\:*[a-zA-Z\\: ]*\\]\\]", delim.tags), collapse="|"), "))")
  
  qdoc.text <- stringr::str_split(qdoc.text, delim)[[1]]
  qdoc.text <- gsub(delim,"",qdoc.text)
  
  # establish first block
  qdoc.text <- trimws(qdoc.text)
  qdoc.blocks <- grepl("^(\\[\\[Question\\:[a-zA-Z\\: ]*\\]\\]|\\d+\\.)", qdoc.text)
  qdoc.text <- qdoc.text[qdoc.blocks]
  attr(qdoc, "text.pages") <- qdoc.text
  
  # delimit questions by enum (e.g. "1.", "2.") or "[[Question:.*]]"
  qdoc.text <- stringr::str_split(qdoc.text, "[\r\n]+(?=(\\[\\[Question\\:[a-zA-Z\\: ]*\\]\\]|\\d+\\.))")
  attr(qdoc, "text.questions") <- qdoc.text
  
  qdoc$pages <- list()
  idx <- 0
  for (b in 1:length(qdoc.text)) {
    qdoc$pages[[b]] <- list(questions = lapply(qdoc.text[[b]], function(.) {
      q <- as.qdoc.question(.)
      q$position <- idx
      idx <<- idx + 1
      return(q)
    }))
  }
  
  return(qdoc)
}

#' Upload processed questionnaire to SurveyMonkey
#'
#' Takes a processed \code{qdoc} object and uploads to SurveyMonkey
#' account associated with token with selected title, location, and format.
#'
#' @param qdoc \code{qdoc} object for survey questionnaire (e.g. via \code{read_qdoc}) to be uploaded.
#' @param title Name for survey in uploaded account.
#' @param from_template_id Survey template to copy from, see \code{browse_templates} (optional).
#' @param from_survey_id Survey id to copy from, see \code{browse_surveys} (optional).
#' @param from_team_template_id Team survey template to copy from, see \code{browse_templates} (optional).
#' @param nickname Survey nickname (optional).
#' @param language Survey language, default is 'en'.
#' @param folder_id Adds the survey to the folder with that id (optional).
#' @param theme_id Creates the survey using the theme provided (optional).
#' @param ... Any additional parameters for upload POST call (see API reference).
#' @param oauth_token Your OAuth 2.0 token. By default, retrieved from \code{get_token()}.
#' @return Upload status and metadata (e.g. assigned survey ID, edit URL) as a dataframe.
#' @references SurveyMonkey \code{POST /survey} API V3 at \url{https://developer.surveymonkey.com/api/v3/#api-endpoints-post-surveys}.
#' @export
upload_qdoc <- function(qdoc,
                        title,
                        from_template_id = NULL,
                        from_survey_id = NULL,
                        from_team_template_id = NULL,
                        nickname = NULL,
                        language = 'en',
                        folder_id = NULL,
                        theme_id = NULL,
                        ...,
                        oauth_token = get_token()) {
  qdoc$title <- title
  qdoc$from_template_id <- from_template_id
  qdoc$from_survey_id <- from_survey_id
  qdoc$from_team_template_id <- from_team_template_id
  qdoc$nickname <- nickname
  qdoc$language <- language  
  qdoc$folder_id <- folder_id  
  qdoc$theme_id <- theme_id
  
  qdoc <- modifyList(qdoc, list(...))
  
  u <- "https://api.surveymonkey.com/v3/surveys"
  b <- jsonlite::toJSON(qdoc, auto_unbox=TRUE)
  h <- standard_request_header(oauth_token)
  
  uploaded_content <- sm_post(url = u, body = b, config = h)
  
  message("OK.")
  
  uploaded_content <- dplyr::bind_rows(unlist(uploaded_content))
  return(uploaded_content)
}
#' Take a survey object and parses it into a tidy data.frame (optimized).
#'
#' @param surv_obj a survey, the result of a call to \code{fetch_survey_obj}.
#' @param oauth_token Your OAuth 2.0 token. By default, retrieved from
#'  \code{get_token()}.
#' @param col_names set column names either to question IDs or names. Options: "id" or "name". Default: "id".
#' @param ... additional arguments to pass on to \code{get_responses}.  See the documentation
#' \code{?get_responses} where these arguments are listed.
#' @return a data.frame (technically a \code{tibble}) with clean responses, one line per respondent.
#' @importFrom rlang .data
#' @export
parse_survey <- function(surv_obj, 
                         oauth_token = get_token(), 
                         col_names = "id",
                         ...
) {
  . <- NULL
  if (surv_obj$response_count == 0) {
    warning("No responses were returned for this survey.  Has anyone responded yet?")
    return(data.frame(survey_id = as.numeric(surv_obj$id)))
  }
  if (!(col_names %in% c("id","name"))) {
    stop("Must pass either 'id' or 'name' to col_names.")
  }

  message("+ Getting responses 🎣")
  
  time_start <- Sys.time()
  respondents <- get_responses(surv_obj$id, oauth_token = oauth_token, ...)
  time_end <- Sys.time()
  time_taken <- time_end - time_start
  print(time_taken)
  
  message("+ Parsing responses ⛏")
  
  labels <- list()
  recordsList <- pbapply::pblapply(1:length(respondents), function(r) {
    response <- respondents[[r]]
    pages <- respondents[[r]]$pages
    record <- list()
    for (p in 1:length(pages)) {
      page <- pages[[p]]
      if (length(page$questions) == 0) next
      for (q in 1:length(page$questions)) {
        question <- page$questions[[q]]
        question_id <- question$id
        family <- surv_obj$families[[question_id]]
        if (family == "matrix") {
          out <- process_matrix(surv_obj, question)
        }
        if (family == "multiple_choice") {
          out <- process_multiple_choice(surv_obj, question)
        }
        if (family == "single_choice") {
          out <- process_single_choice(surv_obj, question)
        }
        if (family == "open_ended") {
          out <- process_open_ended(surv_obj, question)
        }
        if (family == "datetime") {
          out <- process_datetime(surv_obj, question)
        }
        if (col_names == "id") {
          col <- out$id
          labels_new <- as.list(names(out$name))
          names(labels_new) <- names(out$id)
        } else {
          col <- out$name
          labels_new <- as.list(names(out$id))
          names(labels_new) <- names(out$name)
        }
        labels <<- modifyList(labels, labels_new)
        record <- append(record, col)
      }
    }
    record$collector_id <- response$collector_id
    record$collection_mode <- response$collection_mode
    record$response_id <- response$id
    record$response_status <- response$response_status
    record$date_created <- response$date_created
    record$date_modified <- response$date_modified  
    record$first_name <- response$first_name
    record$last_name <- response$last_name
    record$email_address <- response$email_address
    record$ip_address <- response$ip_address

    return(dplyr::bind_cols(record))
  })
  
  message("+ Merging responses 🤝")
  
  records <- dplyr::bind_rows(recordsList)

  for (q in rev(names(surv_obj$questions))) {
    if (col_names == "id") {
      cols <- colnames(records)[grepl(q, colnames(records))]
      if (length(cols) > 0) {
        records <- dplyr::relocate(records, cols)
      }
    } else {
      cols <- names(labels[startsWith(unlist(labels), q)])
      if (length(cols) > 0) {
        records <- dplyr::relocate(records, cols)
      }      
    }
  }
  
  records <- dplyr::relocate(records, c("collector_id",
                                        "collection_mode",
                                        "response_id",
                                        "response_status",
                                        "date_created",
                                        "date_modified",
                                        "first_name",
                                        "last_name",
                                        "email_address",
                                        "ip_address"))
  

  colnames(records) <- gsub("  ", " ", colnames(records))
  names(labels) <- gsub("  "," ", names(labels))
  
  message("+ Levelling columns 🗂")

  for (level.var in names(surv_obj$levels)) {
    if (col_names == "id") {
      col.levels <- surv_obj$levels[[level.var]]
      col.vars <- colnames(records)[grepl(level.var, colnames(records))]
      for (col.var in col.vars) {
        records[[col.var]] <- factor(records[[col.var]], levels = col.levels)
        base::levels(records[[col.var]]) <- col.levels
      }
    } else {
      col.levels <- surv_obj$levels[[level.var]]
      col.vars <- names(labels[startsWith(unlist(labels), level.var)])
      
      for (col.var in col.vars) {
        records[[col.var]] <- factor(records[[col.var]], levels = col.levels)
        base::levels(records[[col.var]]) <- col.levels
      }
    }
  }
  
  message("+ Labelling columns 🗂")

  labels$collector_id <- "Collector ID"
  labels$collection_mode <- "Collector mode"
  labels$response_id <- "Response ID"
  labels$response_status <- "Response status"
  labels$date_created <- "Date response created"
  labels$date_modified <- "Date response modified"
  labels$first_name <- "Respondent's first name"
  labels$last_name <- "Respondent's last name"
  labels$email_address <- "Respondent's email address"
  labels$ip_address <- "Respondent's IP address"
  
  labels <- labels[names(labels) %in% colnames(records)]
  records <- records %>%
    labelled::set_variable_labels(.labels = labels)
  
  if (sample(c(1,2,3,4,5),1)==1) {
    message("\nDONE 🐵\n") 
  } else if (sample(c(1,2,3,4,5),1)==2) {
    message("\nDONE 🙈\n") 
  } else if (sample(c(1,2,3,4,5),1)==3) { 
    message("\nDONE 🙉\n") 
  } else if (sample(c(1,2,3,4,5),1)==4) { 
    message("\nDONE 🙊\n") 
  } else { 
    message("\nDONE 🐒\n")
  }
  
  message(sprintf("%d rows x %d columns", nrow(records), ncol(records)))
  return(records)
}


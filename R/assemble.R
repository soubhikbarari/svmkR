#' Take a survey object and parses it into a tidy data.frame (optimized).
#'
#' @param surv_obj a survey, the result of a call to \code{fetch_survey_obj}.
#' @param oauth_token Your OAuth 2.0 token. By default, retrieved from
#'  \code{get_token()}.
#' @param ... additional arguments to pass on to \code{get_responses}.  See the documentation
#' \code{?get_responses} where these arguments are listed.
#'
#' @param fix_duplicates character if 'error', the default detection of duplicate data will result
#' in an error being raised, otherwise allow the function to return. If 'keep' duplicate results
#' will be retained, if 'drop' duplicates will be removed from the results.
#' @return a data.frame (technically a \code{tibble}) with clean responses, one line per respondent.
#' @importFrom rlang .data
#' @export
parse_survey <- function(surv_obj, 
                         oauth_token = get_token(), 
                         ...,
                         fix_duplicates = c("error", "drop", "keep")
) {
  . <- NULL
  if (surv_obj$response_count == 0) {
    warning("No responses were returned for this survey.  Has anyone responded yet?")
    return(data.frame(survey_id = as.numeric(surv_obj$id)))
  }

  message("+ Getting responses 🎣")
  time_start <- Sys.time()
  respondents <- get_responses(surv_obj$id, oauth_token = oauth_token, ...)
  time_end <- Sys.time()
  time_taken <- time_end - time_start
  print(time_taken)
  
  # Save response status to join later
  message("+ Merging responses 🤝")
  vals <- c("id", "response_status")
  response_status_list <- lapply(respondents, "[", vals)
  status <- do.call(rbind.data.frame, response_status_list)

  message("+ Parsing responses from JSON ⛏")
  responses <- respondents %>%
    parse_respondent_list()

  question_combos <- parse_all_questions(surv_obj)

  # SB: if a choice is deleted or added after collection, some responses
  # may not merge with the present question set; remove these responses
  if (sum(!(responses$choice_id %in% question_combos$choice_id)) > 0) {
    message(sprintf("\n+ Removing %d responses that don't correspond to existing choices in survey 🛑\n",
                    sum(!(responses$choice_id %in% question_combos$choice_id))))
  }
  responses <- responses %>%
    dplyr::filter(.data$choice_id %in% question_combos$choice_id)
  
  # this join order matters
  # - putting q_combos on left yields the right ordering of columns in final result
  # the joining variables vary depending on question types present,
  # so can't hard-code. Thus squash message
  x <- suppressMessages(dplyr::full_join(question_combos, responses))
  
  # # SB: remove any question combos that are never entered
  # x <- subset(x, !is.na(response_id))
 
  # ref: issue #74
  # assertion stops function from returning anything in the case of duplicates
  # to deal with this add parameter fix_duplicate where default behaviour is to error, but
  # can be set to allow the function to continue and return.
  fix_duplicates <- match.arg(fix_duplicates)
  if (fix_duplicates == "error") {
    x <- duplicate_error(x)
  } else if (fix_duplicates == "keep") {
    x <- duplicate_keep(x)
  } else {
    x <- duplicate_drop(x)
  }

  # questions with only simple answer types might not have some referenced columns, #46
  add_if_not_present <- c(choice_id = NA_character_, choice_position = NA_integer_)
  x <- x %>%
    tibble::add_column(!!!add_if_not_present[!names(add_if_not_present) %in% names(.)])

  # 'type' and 'required' are created when question_type == 'demographic'
  # Drop them because it causes issues with duplicated rows per respondent_id
  # Reference Issue #27, Issue #62
  x$type <- NULL
  x$required <- NULL

  # Issue #73 - API added choice_metadata for certain question types.
  # Need to investigate further, but as of 11/2021, the addition is
  #  preventing parse_survey() from working.
  x$choice_metadata <- NULL

  # Issue #57 - Star Ranking
  # - If rating labels are not used, choice_text appears blank.
  # - Need to recode so that choice_text is choice_position
  x <- x %>%
    dplyr::mutate(choice_text = dplyr::case_when(
      .data$choice_text == "" &
        .data$question_type == "matrix" &
        .data$question_subtype == "rating" ~
        as.character(.data$choice_position),
      TRUE ~ .data$choice_text
    ))

  # If question type = Multiple Choice, include choice text + ID in the combined new columns
  message("+ Making unique question IDs 📋")
  pbar <- utils::txtProgressBar(min=0, max=1, style=3)
  utils::setTxtProgressBar(pbar, 0)
  x$q_unique_id <- pbapply::pbapply(
    x %>%
      dplyr::select(.data$question_id, .data$row_id, .data$col_id, .data$other_id),
    1,
    function(x) paste(stats::na.omit(x), collapse = "_")
  )

  utils::setTxtProgressBar(pbar, 0.5)
  x$q_unique_id[
    x$question_type == "multiple_choice" | x$question_subtype == "multi" & is.na(x$other_id)
  ] <- paste(
    x$q_unique_id[
      x$question_type == "multiple_choice" | x$question_subtype == "multi" & is.na(x$other_id)
    ],
    x$choice_id[
      x$question_type == "multiple_choice" | x$question_subtype == "multi" & is.na(x$other_id)
    ],
    sep = "_"
  )
  utils::setTxtProgressBar(pbar, 1)
  close(pbar)
  
  # SB: explicitly check that payload didn't return multiple answers for 
  # questions where respondent can only provide single choice
  x_dup <- x %>% 
    dplyr::filter(.data$question_type == "single_choice" | .data$question_subtype == "single") %>%
    dplyr::group_by(.data$response_id, .data$q_unique_id) %>%
    dplyr::summarise(n_resp = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_resp > 1)
  if (nrow(x_dup) > 0) {
    message(sprintf("+ De-duping %d responses to single-choice questions 🛑",
                    nrow(x_dup)))
    
    x_dup <- x_dup %>% 
      dplyr::left_join(x %>%
                         dplyr::select(.data$response_id, .data$q_unique_id, .data$choice_id), 
                       by = c("response_id", "q_unique_id")) %>%
      dplyr::group_by(.data$response_id, .data$q_unique_id) %>%
      dplyr::filter(dplyr::row_number() == 1)
    
    x <- x %>%
      dplyr::anti_join(x_dup, by = c("response_id", "q_unique_id", "choice_id"))
  }
  
  message("+ Making combined question headings 📋")
  pbar <- utils::txtProgressBar(min=0, max=1, style=3)
  utils::setTxtProgressBar(pbar, 0)  
  # x <- x %>% ###SB: unwrap lapply and do in one go
  #   rowwise() %>% 
  #   mutate(row_text2 = ifelse(row_text == "", NA, row_text)) %>%
  #   mutate(combined_q_heading = paste(na.omit(heading, row_text2, col_text, other_text), collapse = "_")) %>%
  #   select(-row_text2)
  x$combined_q_heading <- pbapply::pbapply(
    x %>%
      dplyr::select(.data$heading, .data$row_text, .data$col_text, .data$other_text) %>%
      dplyr::mutate(row_text = ifelse(.data$row_text == "", NA, .data$row_text)),
    1,
    function(x) paste(stats::na.omit(x), collapse = " - ")
  )
  utils::setTxtProgressBar(pbar, 0.5)
  
  x <- x %>%
    dplyr::mutate(
      combined_q_heading = dplyr::case_when(
        .data$question_type == "multiple_choice" &
          is.na(.data$other_text) ~
          paste(.data$combined_q_heading, .data$choice_text, sep = " - "),
        .data$question_type != "open_ended" &
          .data$question_subtype == "multi" &
          is.na(.data$other_text) ~
          paste(.data$combined_q_heading, .data$choice_text, sep = " - "),
        TRUE ~ .data$combined_q_heading
      )
    )
  utils::setTxtProgressBar(pbar, 1)
  close(pbar)

  # combine open-response text and choice text into a single field to populate the eventual table
  x$answer <- dplyr::coalesce(x$response_text, x$choice_text)

  assertthat::assert_that(
    sum(!is.na(x$answer)) == (sum(!is.na(x$response_text)) + sum(!is.na(x$choice_text))),
    msg = paste0(
      "Uh oh, we failed to account for a combination of open-response text - ",
      file_bug_report_msg()
    )
  )

  static_vars <- setdiff(names(x), c(
    "heading", "question_id", "question_type", "question_subtype",
    "choice_position", "choice_text", "quiz_options", "choice_id",
    "other_id", "other_text", "row_text", "row_id", "description",
    "col_text", "response_text", "col_id", "q_unique_id",
    "combined_q_heading", "answer"
  ))

  message("+ Making wide dataframe ⚡")
  
  final_x <- x %>%
    dplyr::select(
      tidyselect::all_of(static_vars),
      .data$combined_q_heading, .data$answer, .data$q_unique_id
    )


  qid_text_crosswalk <- final_x %>%
    dplyr::distinct(.data$q_unique_id, .data$combined_q_heading) %>%
    dplyr::mutate(unique_text = de_duplicate_names(.data$combined_q_heading))

  # did a full_join above to make sure that all questions [q_unique_ids]
  # are present in result even if no one answered them
  # but that means the spread will fail b/c there's more than one response
  # per q_unique_id for is.na(response_id)
  # Adjust for that to spread, then filter that out after spread
  final_x_real <- final_x %>%
    dplyr::filter(!is.na(.data$response_id))

  final_x_dummy <- final_x %>%
    dplyr::filter(is.na(.data$response_id)) %>%
    dplyr::distinct(.data$q_unique_id)

  final_x <- dplyr::bind_rows(final_x_real, final_x_dummy)

  # spread wide
  # get column order to reset to after spread makes alphabetical
  col_names <- c(
    names(final_x)[!(names(final_x) %in% c("combined_q_heading", "answer", "q_unique_id"))],
    qid_text_crosswalk$unique_text
  )

  out <- final_x %>%
    dplyr::select(-.data$combined_q_heading) %>%
    dplyr::mutate(
      # to spread unrepresented levels
      q_unique_id = factor(.data$q_unique_id, levels = qid_text_crosswalk$q_unique_id)) %>%
    tidyr::pivot_wider(names_from = .data$q_unique_id, values_from = .data$answer) %>%
    dplyr::filter(!is.na(.data$response_id))

  # Takes spread-out results data.frame and turns multiple choice cols into factors.  GH issue #12
  # Doing this within the main function so it can see crosswalk

  master_qs <- x %>%
    dplyr::distinct(
      .data$q_unique_id, .data$choice_id,
      .data$question_id, .data$choice_position, .data$choice_text
    )

  # set a vector as a factor, if it has answer choices associated with its question id
  set_factor_levels <- function(vec, q_id) {

    # fetch possible answer choices given a question's text
    get_factor_levels <- function(q_id) {
      master_qs %>%
        dplyr::filter(.data$q_unique_id == q_id, !is.na(.data$choice_id)) %>%
        # appears to always come from API in order but don't want to assume
        dplyr::arrange(.data$choice_position) %>%
        dplyr::pull(.data$choice_text) %>%
        unique() # in case they loaded the same value twice as answer choices, #48
    }

    name_set <- get_factor_levels(q_id)
    if (length(name_set) == 0) {
      return(vec)
    } else {
      factor(vec, levels = name_set)
    }
  }
  
  message("+ Setting factors 🪜")
  out <- purrr::map2_dfc(out, names(out), set_factor_levels)
  
  if (FALSE) { ###SB: faster parallelization below, but no longer needed 
    nc <- parallel::detectCores()
    cl <- parallel::makeCluster(nc, outfile = "tmp")
    doParallel::registerDoParallel(cl)
    select <- dplyr::select
    parallel::clusterExport(cl = cl, 
                            varlist = c("out", "set_factor_levels", 
                                        "master_qs", "select", "mutate", 
                                        "filter", "arrange", "pull", "%>%"))  
    out2 <- pblapply(cl = cl,
                     X = names(out), 
                     function(q_id) set_factor_levels(out[[q_id]], q_id))
    names(out2) <- names(out)
    out <- bind_cols(out2)
  }
  
  # reset to text names instead of numbers
  # and then re-order to correct columns
  names(out)[(length(static_vars) + 1):length(names(out))] <- qid_text_crosswalk$unique_text[match(
    names(out)[(length(static_vars) + 1):length(names(out))],
    qid_text_crosswalk$q_unique_id
  )]
  out <- out[, col_names]
  out <- out %>%
    dplyr::arrange(dplyr::desc(.data$response_id)) %>%
    dplyr::rename(respondent_id = .data$response_id)

  # Join response status
  out <- out %>%
    dplyr::left_join(., status, by = c("respondent_id" = "id")) %>%
    dplyr::select(
      .data$survey_id, .data$collector_id, .data$respondent_id,
      .data$date_created, .data$date_modified, .data$response_status,
      tidyselect::everything()
    )
  message("\nDONE 🥳\n")
  out
}

# Helper function for de-duplicating identical Q names
# Input: the vector of names
# Adapted from janitor::make_clean_names()
de_duplicate_names <- function(x) {
  dupe_count <- vapply(seq_along(x), function(i) {
    sum(x[i] == x[1:i])
  }, integer(1))
  x[dupe_count > 1] <- paste(
    x[dupe_count > 1], dupe_count[dupe_count > 1],
    sep = "_"
  )
  x
}

# does a data frame contain duplicate rows
# @param x a data.frame
# @return logical, TRUE if there are any duplicates in the data.frame
contains_duplicates <- function(x) {
  sum(find_duplicates(x)) > 0
}

# @param x a data.frame
# @return a logical vector of the duplicated rows
find_duplicates <- function(x) {
  duplicated(dplyr::select_if(x, is.atomic))
}

# @param x a data.frame
duplicate_drop <- function(x) {
  ix_dupes <- find_duplicates(x)
  n_dupes <- sum(ix_dupes)
  if (n_dupes > 0) {
    warning(
      "There are ", n_dupes, " duplicate responses, duplicates are dropped in
      the results. Set fix_duplicates = 'keep' to retain them."
    )
  }
  x[!ix_dupes, ]
}

# @param x a data.frame
duplicate_keep <- function(x) {
  if (contains_duplicates(x)) {
    n_dupes <- sum(find_duplicates(x))
    warning(
      "There are ", n_dupes, " duplicate responses, duplicates are retained in
      the results. Set fix_duplicates = 'drop' to remove them."
    )
  }
  x
}

# Deal with duplicate data by throwing an error
duplicate_error <- function(x) {
  # There should not be duplicate rows here, but putting this here in case of oddities like #27
  assertthat::assert_that(!contains_duplicates(x),
    msg = paste0(
      "There are duplicated rows in the responses.\n",
      "To proceed and retain duplicates, re-run this function with fix_duplicates = 'keep'\n",
      "To proceed with dropped duplicates, re-run with fix_duplicates = 'drop'\n",
      "If this is unexpected - ",
      file_bug_report_msg()
    )
  )
  x
}

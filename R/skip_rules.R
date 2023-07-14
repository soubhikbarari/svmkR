#' Find and code skip rules from survey responses
#' 
#' Given a dataframe of survey responses, reverse engineer skip rules based on completion patterns (and optionally user input) in order to distinguish/re-code "Not applicable" \code{NA} values from "No answer" \code{NA} values.  
#' 
#' Note: if \code{skip_rule_val} or \code{no_answer_val} are set to character strings, the corresponding factor variables (in the relevant question columns) will be expanded via the \code{forcats} library to include those values as levels.
#'
#' @param data input survey data frame of responses (note: function only guaranteed to work if this is the output from \code{parse_survey}).
#' @param interactive if set to \code{TRUE}, detect possible skip rules and ask before re-coding responses; else automatically detect rules and re-code responses.
#' @param skip_rule_val what to code question responses where the respondent never saw the question due to a skip rule (default is \code{NA} which tends to play nicely with banner and cross-tab creation).
#' @param no_answer_val what to code question responses where the respondent saw the question and did not answer (default is "No answer").
#' By default, retrieved from \code{get_token()}.
#'
#' @return A survey response dataframe with NAs distinguished between "Not applicable" responses from "No answer" responses. 
#' @export
find_skip_rules <- function(data,
                            interactive = TRUE,
                            skip_rule_val = NA,
                            no_answer_val = "No answer") {
  # group questions together
  cols <- colnames(data)
  cols <- cols[!(cols %in% c("collector_id","collection_mode",
                             "survey_id","response_id",
                             "response_status","date_created",
                             "date_modified","first_name",
                             "last_name","email_address","ip_address"))]
  if (all(grepl("[a-zA-Z]", cols)==F)) {
    # columns are IDs
    q_stems <- gsub("\\_.*","", cols)
  } else{
    # columns are questions
    q_stems <- gsub(" - .*","", cols)
  }
  q_ids <- cols
  names(q_ids) <- q_stems
  
  # find possible branch paths in survey
  no_answer_val = "No answer"
  skip_rule_val = NA
  
  for (r in 1:(length(q_ids)-1)) {
    cat("\014")
    ## possible root question in skip logic
    root_qid <- q_ids[r]
    if (!is.factor(data[[root_qid]])) {
      next
    }
    if (interactive) {
      prompt <- paste0("\nIs the following question used in a skip rule in your survey?\n`",root_qid,"`\n\n* Enter [y (Yes) | n (No) | q (Quit)]\n")
      ans <- invisible(readline(prompt))
      while (!tolower(ans) %in% c("y","n","q")) {
        ans <- invisible(readline("\n* Please enter [y (Yes) | n (No) | q (Quit)]\n")) 
      }
      if (tolower(ans) == "n") {
        next
      } else if (tolower(ans) == "q") {
        cat("\nQuitting\n"); Sys.sleep(0.1); cat("\014");
        return(data)        
      } else {
        cat("\nFinding possible skip rules...\n"); Sys.sleep(0.1);
      }
      cat("\014")
    }
    for (b in 1:(length(unique(names(q_ids))))) {
      cat("\014")
      ## possible branch question in skip logic
      branch_qstem <- unique(names(q_ids))[b]
      branch_qstem_ids <- q_ids[names(q_ids) == branch_qstem]
      if (min(which(names(q_ids) == branch_qstem)) <= r) {
        ## only those that come after root question
        next
      }
      ## check if any root question perfectly separates NAs and non-NAs
      is_na <- apply(is.na(data[c(branch_qstem_ids)]), 1, all)
      is_na_tbl <- table(data[[root_qid]], is_na)
      is_na_tbl <- as.data.frame.matrix(prop.table(is_na_tbl, 1))
      colnames(is_na_tbl) <- c("% not NA", "% NA")
      is_na_idx <- is_na_tbl[,2]==1
      if ((any(is_na_idx))) {
        prompt.1 <- paste0("================================\nIF `", root_qid, "` IN")
        message(prompt.1)
        prompt.2 <- rownames(is_na_tbl[is_na_idx,])
        print(prompt.2)
        prompt.3 <- paste0("THEN SKIP `", branch_qstem, "`\n================================")
        message(prompt.3)
        ans <- NA
        if (interactive) {
          prompt.4 <- "Is this a skip rule used in your survey?\n\n* Enter [y (Yes) | n (No) | q (Quit)]\n"
          ans <- invisible(readline(prompt.4))
          
          while (!tolower(ans) %in% c("y","n","q")) {
            ans <- invisible(readline("\n* Please enter [y (Yes) | n (No) | q (Quit)]\n")) 
          }
        }
        if (tolower(ans) %in% "q") {
          cat("\nQuitting\n"); Sys.sleep(0.1); cat("\014");          
          return(data)
        }
        if (!interactive | (tolower(ans) %in% "y")) {
          cat("\nCoding N/A...\n");
          skipped <- data[[root_qid]] %in% prompt.2
          for (branch_qstem_id in branch_qstem_ids) {
            if (!is.na(skip_rule_val)) {
              if (is.factor(data[[branch_qstem_id]])) {
                data[[branch_qstem_id]] <- 
                  forcats::fct_expand(data[[branch_qstem_id]], skip_rule_val) 
              }
            }
            if (!is.na(no_answer_val)) {
              if (is.factor(data[[branch_qstem_id]])) {
                data[[branch_qstem_id]] <- 
                  forcats::fct_expand(data[[branch_qstem_id]], no_answer_val)
              }
            }
            if (is.factor(data[[branch_qstem_id]])) {
              data[skipped, branch_qstem_id] <- skip_rule_val
              data[!skipped & is.na(data[[branch_qstem_id]]), branch_qstem_id] <- no_answer_val
            } else {
              data[[branch_qstem_id]] <- as.character(data[[branch_qstem_id]])
              data[skipped, branch_qstem_id] <- skip_rule_val
              data[!skipped & is.na(data[[branch_qstem_id]]), branch_qstem_id] <- no_answer_val              
            }
            message(sprintf("New `%s`:", branch_qstem_id))
            print(table(data[[branch_qstem_id]], useNA = "always"))
          }
          if (interactive) {
            readline("\n\n* Press any key to continue or `q` to quit\n")
          }
        }
      }
    }
  } 
}
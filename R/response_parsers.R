is_numeric <- function(.) {
  suppressWarnings(!is.na(as.numeric(.)))
}

make_output_named <- function(question_text, answer_text) {
  out <- list(answer_text)
  names(out) <- question_text
  out
}

make_output_id <- function(col_id, answer_text) {
  out <- list(answer_text)
  names(out) <- col_id
  out
}

process_matrix <- function(surv_obj, question) {
  question_id <- question$id
  family <- surv_obj$families[[question_id]]
  subtype <- surv_obj$subtypes[[question_id]]
  out_named <- list()
  out_id <- list()
  for (answer in question$answers) {
    if (subtype == "multi") {
      question_text = paste0(surv_obj$questions[[question_id]], " - ", surv_obj$answers[[answer$row_id]], " - ", surv_obj$answers[[answer$choice_id]])
      col_id = paste0(question_id, "_", answer$row_id, "_", answer$choice_id)
      if ((surv_obj$col_fill %in% c("name")) | ("other_id" %in% names(answer)))
        answer_text = surv_obj$answers[[answer$choice_id]]
      else
        answer_text = TRUE
      out_named[[question_text]] <- answer_text
      out_id[[col_id]] <- answer_text
    } else {
      question_text = paste0(surv_obj$questions[[question_id]], " - ", surv_obj$answers[[answer$row_id]])
      col_id = paste0(question_id, "_", answer$row_id)
      answer_text = surv_obj$answers[[answer$choice_id]]
      out_named[[question_text]] <- answer_text
      out_id[[col_id]] <- answer_text
    }
  }
  return(list(name=out_named, id=out_id))
}

process_multiple_choice <- function(surv_obj, question) {
  question_id <- question$id
  family <- surv_obj$families[[question_id]]
  out_named <- list()
  out_id <- list()  
  for (answer in question$answers) {
    if ("other_id" %in% names(answer)) {
      question_text = paste0(surv_obj$questions[[question_id]], " - ", surv_obj$answers[[answer$other_id]])
      answer_text = answer$text
      col_id = paste0(question_id, "_", answer$other_id)
    } else {
      question_text = paste0(surv_obj$questions[[question_id]], " - ", surv_obj$answers[[answer$choice_id]])
      if ((surv_obj$col_fill %in% c("name")))
        answer_text = surv_obj$answers[[answer$choice_id]]
      else
        answer_text = TRUE
      col_id = paste0(question_id, "_", answer$choice_id)
    }
    out_named[[question_text]] <- answer_text
    out_id[[col_id]] <- answer_text
  }
  return(list(name=out_named, id=out_id))
}

process_single_choice <- function(surv_obj, question) {
  question_id <- question$id
  family <- surv_obj$families[[question_id]]  
  answer <- question$answers[[1]]
  if ("other_id" %in% names(answer)) {
    question_text = paste0(surv_obj$questions[[question_id]], " - ", surv_obj$answers[[answer$other_id]])
    col_id = paste0(question_id, "_", answer$other_id)
    answer_text = answer$text
  } else if ((length(answer[[1]]) == 9 & is_numeric(answer[[1]])) | (answer[[1]] %in% names(surv_obj$answers))) {
    question_text = surv_obj$questions[[question_id]]
    col_id = question_id
    answer_text = surv_obj$answers[[answer[[1]]]]
  } else {
    question_text = surv_obj$questions[[question_id]]
    col_id = question_id    
    answer_text = answer[[1]]
  }
  return(list(name = make_output_named(question_text=question_text, answer_text=answer_text),
              id = make_output_id(col_id=col_id, answer_text=answer_text)))
}

process_open_ended <- function(surv_obj, question) {
  question_id = question$id
  col_id = question_id
  family = surv_obj$families[[question_id]]  
  answer_text = question$answers[[1]]$text
  question_text = surv_obj$questions[[question_id]]
  return(list(name = make_output_named(question_text=question_text, answer_text=answer_text),
              id = make_output_id(col_id=col_id, answer_text=answer_text)))
}

process_datetime <- function(surv_obj, question) {
  question_id = question$id
  col_id = question_id  
  family = surv_obj$families[[question_id]]  
  question_text = paste0(surv_obj$questions[[question_id]], " - ", surv_obj$answers[[question$answers[[1]]$row_id]])
  answer_text = question$answers[[1]]$text  
  return(make_output_named(question_text=question_text, answer_text=answer_text))
}
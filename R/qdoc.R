strip_qdoc_tags <- function(x) {
  x <- gsub("\\[\\[[a-zA-Z0-9 \\:]*\\]\\]", "", x)
  x <- gsub("  "," ", x)
  return(x)
}

sanitize <- function(x) {
  x <- gsub("<br>","\n", x)
  return(x)
}

#' @method print qdoc
#' @export
print.qdoc <- function(x) {
  if ("page_count" %in% names(x)) {
    P <- x$page_count
  } else {
    P <- length(x$pages)  
  }
  if ("question_count" %in% names(x)) {
    Q <- x$question_count
  } else {
    Q <- sum(sapply(x$pages, function(y) length(y$questions)))
  }
  if ("title" %in% names(x)) {
    message(x$title)
  }
  message(sprintf("- %d questions\n- %d pages", Q, P))
  if ("response_count" %in% names(x)) {
    message(sprintf("- %d responses", x$response_count))
  }
  if ("edit_url" %in% names(x)) {
    message(sprintf("- edit URL: %s", x$edit_url))
  }
  if ("preview_url" %in% names(x)) {
    message(sprintf("- preview URL: %s", x$preview_url))
  }
  q <- 1
  for (p in 1:P) {
    page <- x$pages[[p]]
    message("============================")
    message(sprintf("Page %d", p))
    message("============================")
    for (qq in 1:length(page$questions)) {
      question <- page$questions[[qq]]
      message("                                  ")
      if (!is.null(question$required)) {
        if (!all(question$required %in% FALSE))
          message(sprintf("        Question %d. (required)        ", q))
        else
          message(sprintf("        Question %d.        ", q))
      } else{
        message(sprintf("        Question %d.        ", q))
      }
      print(question)
      prompt = "* Press [enter] to continue"
      invisible(readline(prompt=prompt))
      cat(paste0(rep("\b", nchar(prompt)+1), collapse=""))
      q <- q + 1
    }
  }
  cat(paste0(rep(" ", nchar(prompt)+1), collapse=""))
}

#' @method print qdoc.multiple_choice
#' @export
print.qdoc.multiple_choice <- function(x) {
  cat(sanitize(x[["headings"]][[1]][[1]]))
  for (choice in x$answers$choices) {
    cat(paste("\n■",choice$text))
  }
}

#' @method print qdoc.single_choice
#' @export
print.qdoc.single_choice <- function(x) {
  cat(sanitize(x[["headings"]][[1]][[1]]))
  for (choice in x$answers$choices) {
    cat(paste("\n○",choice$text))
  }
}

#' @method print qdoc.matrix
#' @export
print.qdoc.matrix <- function(x) {
  if (x$subtype == "single") {
    dot <- "○"  
  } else if (x$subtype == "multi") {
    dot <- "■"
  } else if (x$subtype == "rating") {
    dot <- "🏆"
  }
  
  rows <- sapply(x$answers$rows, function(y) y$text)
  row.tab.size <- max(nchar(rows))
  row.tab <- paste0(rep(" ", row.tab.size), collapse="")
  cols <- sapply(x$answers$choices, function(y) y$text)
  col.tab.size <- max(nchar(cols))
  col.tab <- paste0(rep(" ", col.tab.size), collapse="")
  
  cat(sanitize(x[["headings"]][[1]][[1]]))
  cat("\n\n")  
  cat(row.tab)
  cat(" | ")
  for (c in 1:length(cols)) {
    col <- cols[c]
    cat(col); cat(" | ")
  }
  under.line <- paste0("\n",paste0(rep("-", row.tab.size+sum(nchar(cols))+(length(cols)*3)), collapse=""),"---")
  cat(under.line)
  for (r in 1:length(rows)) {
    row <- rows[r]
    cat(paste0("\n",row))
    row.indent <- paste0(paste0(rep(" ", row.tab.size-nchar(row)), collapse=""), " | ")
    cat(row.indent)
    for (c in 1:length(cols)) {
      tab.size.before <- floor(nchar(cols[c])/2)
      tab.size.after <- nchar(cols[c]) - tab.size.before
      tab.before <- paste0(rep(" ", tab.size.before), collapse="")
      tab.after <- paste0(rep(" ", tab.size.after), collapse="")
      row.dot <- paste0(tab.before, dot, tab.after, collapse="")
      cat(row.dot)
      cat("  ")
    }
    #row.dots <- rep(paste0(dot, col.tab), length(cols))
    #cat(row.dots)
  }
}

# print.qdoc.datetime <- function(.) {
#   cat(.[["headings"]][[1]][[1]])
# }

#' @method print qdoc.open_ended
#' @export
print.qdoc.open_ended <- function(x) {
  cat(sanitize(x[["headings"]][[1]][[1]]))
  cat("\n______________________")
}


is_advanced_format <- function(q.text) {
  # Note: must specify "[[Question.*]]" in Qualtrics, but "[[Q.*]]" suffices in SVMK
  grepl("\\[\\[Q", q.text, ignore.case=T)
}

has_choices <- function(q.text) {
  ## blank line between question text and choices
  grepl("\n\n", q.text, ignore.case=T) |
    grepl("\\[\\[Choices\\]\\](\n)*.*[^ ]", q.text, ignore.case=T) |
    grepl("\\[\\[AdvancedChoices\\]\\](\n)*.*[^ ]\\[\\[Choice(\\:|\\d)*\\]\\]", q.text, ignore.case=T)
}

has_rows_and_cols <- function(q.text) {
  (grepl("\\[\\[(Choices|Rows|AdvancedChoices|AdvancedRows)", q.text, ignore.case = T) & 
     grepl("\\[\\[(Answers|Cols|Columns|AdvancedAnswers|AdvancedCols)", q.text, ignore.case = T)) |
    stringr::str_count(q.text, "\n\n") == 2
}

get_choices <- function(q.text, quiet.fail = FALSE) {
  # Extract choices for multiple choice questions
  if (grepl("\\[\\[Choices\\]\\]", q.text, ignore.case = T)) {
    choices_line <- stringr::str_extract_all(q.text, "(?<=\\[\\[Choices\\]\\])((.|\n)*)")[[1]]
    choices <- stringr::str_split(choices_line, "\n")[[1]]
  } else if (grepl("\\[\\[AdvancedChoices\\]\\]", q.text, ignore.case = T)) {
    choices_line <- stringr::str_extract_all(q.text, "(?<=\\[\\[AdvancedChoices\\]\\])((.|\n)*)")[[1]]
    choices <- stringr::str_split(choices_line, "\\[\\[Choice(\\:|\\d)*\\]\\]")[[1]]
  } else if (grepl("\n\n", q.text, ignore.case = T)){
    choices_line <- stringr::str_extract_all(q.text, "(?<=\n\n)((.|\n)*)")[[1]]
    choices <- stringr::str_split(choices_line, "\n")[[1]]
  } else {
    choices <- c()
    if (!quiet.fail) {
      stop(sprintf("no choices found in question text"))
    }
  }
  choices <- trimws(gsub("^(\t)*(\\-|\\d\\.|\\d\\))","", choices))
  choices <- choices[choices != ""]
  if (length(choices) == 0) {
    if (!quiet.fail) {
      stop(sprintf("no choices found in question text"))
    }
  }
  choices <- lapply(choices, function(.) list(text=.))
  return(choices)
}

get_rows <- function(q.text, quiet.fail = FALSE) {
  # Extract rows for matrix questions
  q.text <- stringr::str_remove(q.text, "\\[\\[Advanced(Answers|Col|Cols|Columns)\\]\\](.|\n)*")  
  q.text <- stringr::str_remove(q.text, "\\[\\[(Answers|Col|Cols|Columns)\\]\\](.|\n)*")
  if (grepl("\\[\\[(Choices|Rows)\\]\\]", q.text, ignore.case = T)) {
    rows_line <- stringr::str_extract_all(q.text, "(?<=\\[\\[(Choices|Rows)\\]\\])((.|\n)*)")[[1]]
    rows <- stringr::str_split(rows_line, "\n")[[1]]
  } else if (grepl("\\[\\[Advanced(Choices|Rows)\\]\\]", q.text, ignore.case = T)) {
    rows_line <- stringr::str_extract_all(q.text, "(?<=\\[\\[Advanced(Choices|Rows)\\]\\])((.|\n)*)")[[1]]
    rows <- stringr::str_split(rows_line, "\\[\\[(Choice|Row)(\\:|\\d)*\\]\\]")[[1]]
  } else if (grepl("\n\n", q.text, ignore.case = T)){
    rows_line <- stringr::str_extract_all(q.text, "(?<=\n\n)((.|\n)*)(?=\n\n)")[[1]]
    rows <- stringr::str_split(rows_line, "\n")[[1]]
  } else {
    rows <- c()
    if (!quiet.fail) {
      stop(sprintf("no rows found in question text"))
    }
  }
  rows <- trimws(gsub("^(\\-|\\d\\.|\\d\\))","", rows))
  rows <- rows[rows != ""]
  if (length(rows) == 0) {
    if (!quiet.fail) {
      stop(sprintf("no rows found in question text"))
    }
  }
  rows <- lapply(rows, function(.) list(text=.))
  return(rows)
}

get_cols <- function(q.text, quiet.fail = FALSE) {
  # Extract columns for matrix questions
  if (grepl("\\[\\[(Answers|Cols|Columns)\\]\\]", q.text, ignore.case = T)) {
    cols_line <- stringr::str_extract_all(q.text, "(?<=\\[\\[(Answers|Cols|Columns)\\]\\])((.|\n)*)")[[1]]
    cols <- stringr::str_split(cols_line, "\n")[[1]]
  } else if (grepl("\\[\\[Advanced(Answers|Cols|Columns)\\]\\]", q.text, ignore.case = T)) {
    cols_line <- stringr::str_extract_all(q.text, "(?<=\\[\\[Advanced(Answers|Cols|Columns)\\]\\])((.|\n)*)")[[1]]
    cols <- stringr::str_split(cols_line, "\\[\\[(Answer|Col|Column)(\\:|\\d)*\\]\\]")[[1]]
  } else if (grepl("\n\n", q.text, ignore.case = T)){
    if (stringr::str_count(q.text, "\n\n") == 2) {
      cols_line <- stringr::str_remove(q.text, "(.|\n)+\n\n(.|\n)+\n\n")
      cols <- stringr::str_split(cols_line, "\n")[[1]]
    } else {
      cols <- c()
      if (!quiet.fail) {
        stop(sprintf("question text not formatted properly"))
      }
    }
  } else {
    cols <- c()
    if (!quiet.fail) {
      stop(sprintf("no cols found in question text"))
    }
  }
  cols <- trimws(gsub("^(\\-|\\d\\.|\\d\\))","", cols))
  cols <- cols[cols != ""]
  if (length(cols) == 0) {
    if (!quiet.fail) {
      stop(sprintf("no cols found in question text"))
    }
  }
  cols <- lapply(cols, function(.) list(text=.))
  return(cols)
}

get_heading <- function(q.text) {
  if (is_advanced_format(q.text)) {
    heading <- stringr::str_remove(q.text, "\\[\\[(Choices|AdvancedChoices|Rows|AdvancedRows)\\]\\](.|\n)*")
  } else {
    heading <- stringr::str_remove(q.text, "\n\n(.|\n)*")
    heading <- trimws(heading)
    heading <- stringr::str_remove(heading, "^\\d+(\\.|\\))")
  }
  heading <- strip_qdoc_tags(heading)
  return(list(heading=trimws(heading)))
}

is_multiple_choice <- function(q.text) {
  # Note: "multiple answer" in Qualtrics translates to "multiple choice" in SVMK,
  #        must specify "multiple answer" for a MC in SVMK, otherwise default is "single answer".
  if (is_advanced_format(q.text)) {
    grepl("\\[\\[Question\\:(MC|Multi|MultipleChoice)", q.text, ignore.case=T) &
      grepl("(\\[\\[|\\:)(MultipleAnswer|MultiAnswer|MultiSelect)", q.text, ignore.case=T)
  } else {
    has_choices(q.text) &
      (grepl("\\[\\[(MultipleAnswer|MultiAnswer|MultiSelect)", q.text, ignore.case=T) |
         grepl("Select all that apply", q.text, ignore.case=T))
  }
}

is_single_choice <- function(q.text) {
  # Note: "single answer" in Qualtrics translates to "single choice" in SVMK
  if (is_advanced_format(q.text)) {
    (grepl("\\[\\[Question\\:(MC|Multi|MultipleChoice)", q.text, ignore.case=T) &
       !grepl("(\\[\\[|\\:)(MultipleAnswer|MultiAnswer|MultiSelect)", q.text, ignore.case=T))
  } else {
    has_choices(q.text)
  }
}

is_matrix <- function(q.text) {
  if (is_advanced_format(q.text)) {
    grepl("\\[\\[(Question\\:Matrix|Matrix)", q.text, ignore.case=T)
  } else {
    has_rows_and_cols(q.text)
  }
}

is_datetime <- function(q.text) {
  grepl("\\[\\[(Question\\:(DateTime|DT)|(DateTime|DT))", q.text, ignore.case=T)
}

is_open_ended <- function(q.text) {
  grepl("\\[\\[(Question\\:(TextEntry|TE|OpenEnd|Open)|(TextEntry|TE|OpenEnded|Open))", q.text, ignore.case=T)
}

get_question_params <- function(q.text) {
  params <- list()
  if (grepl("(\\[\\[|\\:)Required", q.text, ignore.case=T)) {
    params$required <- list(
      text = "This question requires an answer.",  ## default
      type = "all", ## default
      amount = "1"
    )
  } 
  return(params)
}

get_subtype <- function(q.text, fam = NULL) {
  ## assume first one is default
  subtypes <- list(single_choice   = c("vertical", "horiz", "menu"),
                   matrix          = c("single", "rating", "ranking", "multi"), #"menu"
                   open_ended      = c("single", "multi", "numerical", "essay"),
                   datetime        = c("both", "date_only", "time_only"),
                   multiple_choice = c("vertical"))
  if (grepl("(\\[\\[|\\:)Horiz", q.text, ignore.case=T)) {
    subtype <- "horiz"
  } else if (grepl("(\\[\\[|\\:)Vertical", q.text, ignore.case=T)) {
    subtype <- "vertical"
  } else if (grepl("(\\[\\[|\\:)(Dropdown|Menu)", q.text, ignore.case=T)) {
    subtype <- "menu"
  } else if (grepl("(\\[\\[|\\:)Single", q.text, ignore.case=T)) {
    subtype <- "single"
  } else if (grepl("(\\[\\[|\\:)Multi", q.text, ignore.case=T) |
             grepl("Select all that apply", q.text, ignore.case=T)) {
    subtype <- "multi"
  } else if (grepl("(\\[\\[|\\:)Essay", q.text, ignore.case=T)) {
    subtype <- "essay"
  } else if (grepl("(\\[\\[|\\:)(Numerical|Form)", q.text, ignore.case=T)) {
    subtype <- "numerical"
  } else {
    subtype <- NA
  }
  if (!is.null(fam)) {
    if (!(fam %in% names(subtypes))) {
      subtype <- NA
    } else if (!(subtype %in% subtypes[[fam]])) {
      subtype <- subtypes[[fam]][1]
    }
  }
  return(subtype)
}
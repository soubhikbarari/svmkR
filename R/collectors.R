get_collectors <- function(survey_id,
                           page = 1,
                           per_page = NULL,
                           sort_by = NULL,
                           sort_order = NULL,
                           name = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           include = NULL,
                           all_pages = TRUE,
                           oauth_token = get_token()) {
  if (!is.null(include)){
    include <- paste(include, collapse = ",")
  }

  if (!is.null(include) & "everything" %in% tolower(include)) {
    include <- "type,status,response_count,date_created,date_modified,url"
  }

  u <- paste("https://api.surveymonkey.net/v3/surveys/", survey_id, "/collectors/", sep = "")
  h <- standard_request_header(oauth_token)
  b <- list(page = page,
            per_page = per_page,
            sort_by = sort_by,
            sort_order = sort_order,
            name = name,
            start_date = start_date,
            end_date = end_date,
            include = include)
  nulls <- sapply(b, is.null)
  if (all(nulls)) {
    b <- NULL
  } else {
    b <- b[!nulls]
  }

  if(!is.null(b)){
  parsed_content <- sm_get(url = u, query = b, config = h)

  collectors <- parsed_content$data

  collectors %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id = as.numeric(.data$id)) %>%
    return()
  } else {
    stop("all query inputs are NULL. see ?get_collectors for input details.")
  }
}

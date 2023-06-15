get_recipients <- function(collector_id,
                           page = 1,
                           per_page = 50,
                           all_pages = TRUE,
                           oauth_token = get_token()) {

  u <- paste("https://api.surveymonkey.net/v3/collectors/", collector_id, "/recipients/", sep = "")
  h <- standard_request_header(oauth_token)

  b <- list(
    page = page,
    include = c("survey_link")
  )
  nulls <- sapply(b, is.null)
  if (all(nulls)) {
    b <- NULL
  } else {
    b <- b[!nulls]
  }

  parsed_content <- sm_get(url = u, query = b, config = h)

  recipients <- parsed_content$data

  # recursively get all recipients if all_pages = TRUE
  if (all_pages == TRUE & (!is.null(parsed_content$links[["next"]]))) {
    rnext <- get_recipients(collector_id,
      page = page + 1,
      per_page = per_page,
      all_pages = all_pages
    )
    recipients <- c(recipients, rnext)
  }

  recipients %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id = as.numeric(.data$id)) %>%
    return()
}

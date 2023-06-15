get_responses <- function(id,
                          page = 1,
                          all_pages = TRUE,
                          per_page = 100,
                          start_created_at = NULL,
                          end_created_at = NULL,
                          start_modified_at = NULL,
                          end_modified_at = NULL,
                          sort_order = "DESC",
                          sort_by = "date_modified",
                          oauth_token = get_token()) {

  u <- paste("https://api.surveymonkey.net/v3/surveys/", id, "/responses/bulk?", sep = "")
  h <- standard_request_header(oauth_token)

  start_created_at <- format_date(start_created_at)
  end_created_at <- format_date(end_created_at)
  start_modified_at <- format_date(start_modified_at)
  end_modified_at <- format_date(end_modified_at)

  b <- list(
    page = page,
    per_page = per_page,
    start_created_at = start_created_at,
    end_created_at = end_created_at,
    start_modified_at = start_modified_at,
    end_modified_at = end_modified_at,
    sort_order = sort_order,
    sort_by = sort_by
  )
  nulls <- sapply(b, is.null)
  if (all(nulls)) {
    b <- NULL
  } else {
    b <- b[!nulls]
  }

  parsed_content <- sm_get(url = u, query = b, config = h)

  responses <- parsed_content$data

  # recursively get all responses if all_pages = TRUE
  if (all_pages == TRUE & (!is.null(parsed_content$links[["next"]]))) {
    rnext <- get_responses(id,
      page = page + 1,
      all_pages,
      per_page,
      start_created_at,
      end_created_at,
      start_modified_at,
      end_modified_at,
      sort_order,
      sort_by,
      oauth_token = oauth_token
    )
    responses <- c(responses, rnext)
  }
  responses
}

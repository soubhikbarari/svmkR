test_that("there is an error message when no token exists", {
  expect_error(browse_surveys(include = 'everything', oauth_token = "foo") %>%
                 suppressWarnings())
})

test_that("browse surveys works as intended", {
  httptest::with_mock_api({
    surveys <- browse_surveys(oauth_token = "temp") %>% 
      suppressWarnings()
  })
  expect_equal(names(surveys), c("title", "id", "url", "nickname"))
  expect_equal(surveys$title, c("my survey", "dev"))
  expect_true("data.frame" %in% class(surveys))
})

test_that("include = everything returns all fields", {
  httptest::with_mock_api({
    surveys <- browse_surveys(include = "everything", oauth_token = "temp") %>% 
      suppressWarnings()
  })
  
  expect_equal(names(surveys), c("title", "id", "url", "nickname", "response_count", "date_created",
                                 "date_modified", "language", "question_count", "analyze_url",
                                 "preview"))
})

test_that("all NULL values are stopped", {
  httptest::with_mock_api({
    expect_error(browse_surveys(oauth_token = "temp",
                                page = NULL,
                                per_page = NULL,
                                sort_by = NULL,
                                sort_order = NULL,
                                start_modified_at = NULL,
                                end_modified_at = NULL,
                                title = NULL,
                                include = NULL,
                                folder_id = NULL))
  })
})




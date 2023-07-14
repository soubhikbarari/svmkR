test_that("responses are fetched and parsed as expected", {
  httptest::with_mock_api({
    survey.0 <- fetch_survey_obj(318754279, oauth_token = "temp") %>% 
      suppressWarnings()
  })
  httptest::with_mock_api({
    survey <- parse_survey(survey.0, 
                           oauth_token = "temp",
                           col_names = "id") %>% 
      suppressWarnings()
  })
  expected_cols <- c("collector_id", "collection_mode", "survey_id", 
                     "response_id", "response_status", "date_created", 
                     "date_modified", "first_name", "last_name", 
                     "email_address", "ip_address", "769893650", "769893903")
  
  expect_true("data.frame" %in% class(survey))
  expect_true(all(expected_cols %in% names(survey)))
  expect_true(all(names(survey) %in% expected_cols))
  
  expect_type(survey$survey_id, "double")
  expect_type(survey$collector_id, "double")
  expect_type(survey$response_id, "double")
  expect_type(survey$response_status, "character")
  expect_type(survey$ip_address, "character")
  
  httptest::with_mock_api({
    survey.0 <- fetch_survey_obj(318754279, oauth_token = "temp") %>% 
      suppressWarnings()
  })
  httptest::with_mock_api({
    survey <- parse_survey(survey.0, 
                           oauth_token = "temp",
                           col_names = "name") %>% 
      suppressWarnings()
  })
  
  expect_true(is.factor(survey$`How many pets do you have?`))
  expect_true(is.character(survey$`What are the names of your pets?`))
  expect_true(!all(is.na(survey)))
})

with_mock_api({
  test_that("response count == 0 shows a warning", {
    oauth <- "temp"
    survey <- fetch_survey_obj(318754279, oauth_token = oauth) %>% 
      suppressWarnings()
    survey$response_count <- 0
    expect_warning( parse_survey(survey, oauth_token = "temp"))
  })
})



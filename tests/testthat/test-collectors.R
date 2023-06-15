

test_that("get_collectors works as intended", {
  httptest::with_mock_api({
    collector <- get_collectors(survey_id = 318754279, oauth_token = "temp") %>% 
      suppressWarnings()
    
    expect_equal(names(collector), c("name", "id", "href", "type"))
    expect_equal(collector$name, c("Web Link 2", "Web Link 1"))
    expect_equal(collector$id, c(415838011, 415837773))
    expect_equal(collector$href[1], "https://api.surveymonkey.net/v3/collectors/415838011")
    expect_type(collector$name, "character")
    expect_type(collector$id, "double")
    expect_type(collector$href, "character")
    expect_type(collector$type, "character")
    expect_true("data.frame" %in% class(collector))
  })
})

test_that("all NULL values are stopped", {
  httptest::with_mock_api({
    expect_error(get_collectors(survey_id = 318754279,
                                oauth_token = "temp",
                                page = NULL,
                                all_pages = NULL,
                                verbose = FALSE))
  })
})

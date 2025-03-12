#' Mock API Response Examples for svmkR
#' 
#' This script provides examples of creating and using mock API responses
#' for testing without making actual API calls.

source("tests/interactive/debug_helpers.R")
library(httr)
library(jsonlite)

# ---------------------------------------------------------------------
# Creating Mock Fixtures
# ---------------------------------------------------------------------

#' Create a basic survey list mock response
#' 
#' This shows the structure of a typical SurveyMonkey API response
create_survey_list_mock <- function() {
  # Simplified structure based on actual SurveyMonkey API response
  content <- list(
    data = list(
      list(
        id = "123456789",
        title = "Test Survey 1",
        nickname = "Test1",
        href = "https://api.surveymonkey.com/v3/surveys/123456789"
      ),
      list(
        id = "987654321",
        title = "Test Survey 2",
        nickname = "Test2",
        href = "https://api.surveymonkey.com/v3/surveys/987654321"
      )
    ),
    per_page = 2,
    page = 1,
    total = 2,
    links = list(
      self = "https://api.surveymonkey.com/v3/surveys?page=1&per_page=2"
    )
  )
  
  # Create a mock response object
  response <- create_mock_response(content)
  
  # Save to fixtures
  save_response(response, "mock_survey_list")
  
  return(response)
}

#' Create a mock survey details response
#' 
#' @param survey_id ID to use for the mock survey
create_survey_details_mock <- function(survey_id = "123456789") {
  # Simplified survey details response
  content <- list(
    id = survey_id,
    title = "Test Survey Details",
    nickname = "TestDetails",
    language = "en",
    question_count = 2,
    page_count = 1,
    response_count = 10,
    date_created = "2023-01-01T00:00:00",
    date_modified = "2023-01-02T00:00:00",
    pages = list(
      list(
        id = "page1",
        title = "Page 1",
        description = "First page of questions",
        questions = list(
          list(
            id = "question1",
            heading = "How would you rate our service?",
            position = 1,
            type = "matrix",
            family = "matrix",
            subtype = "rating",
            answers = list(
              rows = list(
                list(id = "row1", text = "Customer Service"),
                list(id = "row2", text = "Product Quality")
              ),
              choices = list(
                list(id = "choice1", text = "1"),
                list(id = "choice2", text = "2"),
                list(id = "choice3", text = "3"),
                list(id = "choice4", text = "4"),
                list(id = "choice5", text = "5")
              )
            )
          ),
          list(
            id = "question2",
            heading = "Any other comments?",
            position = 2,
            type = "open_ended",
            family = "open_ended",
            subtype = "essay"
          )
        )
      )
    )
  )
  
  # Create a mock response object
  response <- create_mock_response(content)
  
  # Save to fixtures
  save_response(response, paste0("mock_survey_details_", survey_id))
  
  return(response)
}

#' Create a mock survey responses list
#' 
#' @param survey_id ID to use for the mock survey
create_survey_responses_mock <- function(survey_id = "123456789") {
  # Simplified response list
  content <- list(
    data = list(
      list(
        id = "response1",
        recipient_id = "recipient1",
        collection_mode = "default",
        survey_id = survey_id,
        custom_variables = list(),
        edit_url = "https://www.surveymonkey.com/r/?sm=editlink1",
        analyze_url = "https://www.surveymonkey.com/analyze/browse/",
        total_time = 300,
        date_modified = "2023-01-15T10:00:00",
        date_created = "2023-01-15T09:55:00",
        ip_address = "192.0.2.1",
        pages = list()
      ),
      list(
        id = "response2",
        recipient_id = "recipient2",
        collection_mode = "default",
        survey_id = survey_id,
        custom_variables = list(),
        edit_url = "https://www.surveymonkey.com/r/?sm=editlink2",
        analyze_url = "https://www.surveymonkey.com/analyze/browse/",
        total_time = 240,
        date_modified = "2023-01-16T14:00:00",
        date_created = "2023-01-16T13:56:00",
        ip_address = "192.0.2.2",
        pages = list()
      )
    ),
    per_page = 2,
    page = 1,
    total = 2,
    links = list(
      self = paste0("https://api.surveymonkey.com/v3/surveys/", survey_id, 
                    "/responses?page=1&per_page=2")
    )
  )
  
  # Create a mock response object
  response <- create_mock_response(content)
  
  # Save to fixtures
  save_response(response, paste0("mock_survey_responses_", survey_id))
  
  return(response)
}

#' Create a detailed mock response with actual answer data
#' 
#' @param survey_id ID to use for the mock survey
#' @param response_id ID to use for the mock response
create_detailed_response_mock <- function(survey_id = "123456789", response_id = "response1") {
  # Detailed response with answers
  content <- list(
    id = response_id,
    recipient_id = "recipient1",
    collection_mode = "default",
    survey_id = survey_id,
    custom_variables = list(),
    edit_url = "https://www.surveymonkey.com/r/?sm=editlink1",
    analyze_url = "https://www.surveymonkey.com/analyze/browse/",
    total_time = 300,
    date_modified = "2023-01-15T10:00:00",
    date_created = "2023-01-15T09:55:00",
    ip_address = "192.0.2.1",
    pages = list(
      list(
        id = "page1",
        questions = list(
          list(
            id = "question1",
            answers = list(
              list(
                row_id = "row1",
                choice_id = "choice4",
                text = "4"
              ),
              list(
                row_id = "row2",
                choice_id = "choice5",
                text = "5"
              )
            )
          ),
          list(
            id = "question2",
            answers = list(
              list(
                text = "Very satisfied with the service. Will recommend to others."
              )
            )
          )
        )
      )
    )
  )
  
  # Create a mock response object
  response <- create_mock_response(content)
  
  # Save to fixtures
  save_response(response, paste0("mock_detailed_response_", survey_id, "_", response_id))
  
  return(response)
}

# ---------------------------------------------------------------------
# Using Mock Responses for Testing
# ---------------------------------------------------------------------

#' Demonstrate how to use mock responses to test package functions
#' 
#' This shows how to patch API calls to use mock responses instead
demo_using_mocks <- function() {
  # First, create some mock fixtures if they don't exist
  if (!file.exists("tests/fixtures/mock_survey_list.rds")) {
    create_survey_list_mock()
    create_survey_details_mock("123456789")
    create_survey_responses_mock("123456789")
    create_detailed_response_mock("123456789", "response1")
  }
  
  # Load the svmkR package
  load_package()
  
  # Example of patching the httr GET function to return mock responses
  # This is a simplified example - in real testing you'd use more sophisticated
  # mocking tools like testthat::with_mock or mockery
  
  # Original GET function
  original_GET <- httr::GET
  
  # Mock GET function that returns fixtures for specific URLs
  mock_GET <- function(url, ...) {
    if (grepl("surveys\\?", url)) {
      message("Using mock survey list response")
      return(load_fixture("mock_survey_list"))
    } else if (grepl("surveys/[0-9]+$", url)) {
      survey_id <- sub(".*/surveys/([0-9]+)$", "\\1", url)
      message("Using mock survey details response for survey ", survey_id)
      return(load_fixture(paste0("mock_survey_details_", survey_id)))
    } else if (grepl("surveys/[0-9]+/responses\\?", url)) {
      survey_id <- sub(".*/surveys/([0-9]+)/responses.*", "\\1", url)
      message("Using mock survey responses list for survey ", survey_id)
      return(load_fixture(paste0("mock_survey_responses_", survey_id)))
    } else if (grepl("surveys/[0-9]+/responses/[a-zA-Z0-9]+", url)) {
      survey_id <- sub(".*/surveys/([0-9]+)/responses/([a-zA-Z0-9]+).*", "\\1", url)
      response_id <- sub(".*/surveys/([0-9]+)/responses/([a-zA-Z0-9]+).*", "\\2", url)
      message("Using mock detailed response for survey ", survey_id, ", response ", response_id)
      return(load_fixture(paste0("mock_detailed_response_", survey_id, "_", response_id)))
    } else {
      # For URLs we don't have mocks for, use the real function
      return(original_GET(url, ...))
    }
  }
  
  # Example: Use with_mock to temporarily replace the GET function
  # Note: This is a simplified example, in real tests you'd use testthat::with_mock
  with_mock_example <- function(expr) {
    # Save the original function
    old_GET <- httr::GET
    
    # Replace with our mock
    unlockBinding("GET", as.environment("package:httr"))
    assign("GET", mock_GET, as.environment("package:httr"))
    
    # Evaluate the expression with the mock in place
    result <- tryCatch(
      expr,
      finally = {
        # Always restore the original function
        assign("GET", old_GET, as.environment("package:httr"))
      }
    )
    
    return(result)
  }
  
  # Example usage
  test_with_mocks <- function() {
    with_mock_example({
      # This will use our mock responses instead of making real API calls
      surveys <- svmkR::browse_surveys(verb = FALSE)
      print(surveys)
      
      details <- svmkR::fetch_survey_details("123456789")
      print(details)
    })
  }
  
  test_with_mocks()
}

# Create a set of initial mock fixtures
initialize_mock_fixtures <- function() {
  create_survey_list_mock()
  create_survey_details_mock("123456789")
  create_survey_responses_mock("123456789")
  create_detailed_response_mock("123456789", "response1")
  
  message("Basic mock fixtures have been created in tests/fixtures/")
}

# Example of an error response
create_error_response_mock <- function(status_code = 401, message = "Invalid API key") {
  content <- list(
    error = list(
      id = "error_id",
      name = "Authentication Error",
      docs = "https://developer.surveymonkey.com/api/v3/#error-codes",
      message = message,
      http_status_code = status_code
    )
  )
  
  response <- create_mock_response(content, status_code = status_code)
  save_response(response, paste0("mock_error_", status_code))
  
  return(response)
}

message("Mock examples loaded. Run initialize_mock_fixtures() to create basic fixtures.")

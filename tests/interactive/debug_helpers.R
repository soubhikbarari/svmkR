#' Interactive Debugging Helpers for svmkR
#' 
#' This script provides helper functions for debugging svmkR package.
#' Load it using source() or use load_all() to load the package code first.

library(devtools)
library(httr)
library(jsonlite)
library(testthat)
library(magrittr)
library(dplyr)
library(purrr)


# ---------------------------------------------------------------------
#### Setup and Configuration ####
# ---------------------------------------------------------------------

#' Load the package for testing
#'
#' Loads the svmkR package from the current directory for interactive testing
load_package <- function() {
  devtools::load_all()
  message("svmkR package loaded from current directory")
}

#' Setup API credentials
#'
#' @param api_key Your SurveyMonkey API key
#' @param oauth_token Your SurveyMonkey OAuth token
#' @param use_env If TRUE, will attempt to use environment variables SURVEYMONKEY_API_KEY and SURVEYMONKEY_OAUTH_TOKEN
setup_credentials <- function(api_key = NULL, oauth_token = NULL, use_env = TRUE) {
  if (use_env) {
    api_key <- Sys.getenv("SURVEYMONKEY_API_KEY", api_key)
    oauth_token <- Sys.getenv("SURVEYMONKEY_OAUTH_TOKEN", oauth_token)
  }
  
  if (is.null(api_key) || api_key == "" || is.null(oauth_token) || oauth_token == "") {
    stop("API key and OAuth token must be provided or set as environment variables")
  }
  
  options(sm_api_key = api_key)
  options(sm_oauth_token = oauth_token)
  
  message("API credentials configured")
}

# ---------------------------------------------------------------------
# Logging and Debugging
# ---------------------------------------------------------------------

#' Enable verbose HTTP logging
#'
#' Enables detailed logging of HTTP requests and responses
enable_http_logging <- function() {
  httr::set_config(httr::verbose())
  message("HTTP logging enabled")
}

#' Disable verbose HTTP logging
disable_http_logging <- function() {
  httr::reset_config()
  message("HTTP logging disabled")
}

#' Create a function-level tracer
#'
#' @param function_name Name of function to trace
#' @param package_name Name of package containing function
trace_function <- function(function_name, package_name = "svmkR") {
  trace(function_name, exit = quote({
    cat("EXIT ", match.call()[[1]], " with result: \n")
    if (!is.null(returnValue())) str(returnValue())
  }), print = FALSE)
  
  message(paste0("Tracing enabled for ", function_name))
}

#' Write API response to file
#'
#' @param response httr response object
#' @param filename Name to save the response as
save_response <- function(response, filename) {
  if (!dir.exists("tests/fixtures")) {
    dir.create("tests/fixtures", recursive = TRUE)
  }
  
  filepath <- file.path("tests/fixtures", paste0(filename, ".rds"))
  saveRDS(response, filepath)
  message(paste0("Response saved to ", filepath))
}

# ---------------------------------------------------------------------
# Testing Utilities
# ---------------------------------------------------------------------

#' Test API connection
#'
#' Tests if the SurveyMonkey API is accessible with current credentials
test_api_connection <- function() {
  tryCatch({
    result <- svmkR::browse_surveys(page_size = 1, verb = FALSE)
    message("API connection successful!")
    invisible(TRUE)
  }, 
  error = function(e) {
    message("API connection failed: ", e$message)
    invisible(FALSE)
  })
}

#' Get a sample survey for testing
#'
#' @param index Which survey to return (by position)
#' @return Survey ID for testing
get_test_survey_id <- function(index = 1) {
  surveys <- svmkR::browse_surveys(verb = FALSE)
  if (length(surveys$data) < index) {
    stop("Not enough surveys available. Requested index: ", index, 
         ", available surveys: ", length(surveys$data))
  }
  
  survey_id <- surveys$data[[index]]$id
  message("Selected survey ID: ", survey_id)
  return(survey_id)
}

#' Run a function with detailed error capture
#'
#' @param expr Expression to evaluate
#' @return Result of expression or detailed error information
safe_run <- function(expr) {
  result <- tryCatch({
    list(
      status = "success",
      value = eval(expr),
      error = NULL
    )
  }, 
  error = function(e) {
    list(
      status = "error",
      value = NULL,
      error = list(
        message = e$message,
        call = e$call,
        trace = sys.calls()
      )
    )
  })
  
  if (result$status == "error") {
    message("Error occurred: ", result$error$message)
  }
  
  return(result)
}

# ---------------------------------------------------------------------
# Mock Response Utilities
# ---------------------------------------------------------------------

#' Create a mock HTTP response
#'
#' @param content The content to include in the response
#' @param status_code HTTP status code
#' @param headers HTTP headers
create_mock_response <- function(content, status_code = 200, 
                                headers = list(`Content-Type` = "application/json")) {
  response <- structure(
    list(
      url = "https://api.surveymonkey.com/v3/mock",
      status_code = status_code,
      headers = headers,
      content = charToRaw(jsonlite::toJSON(content, auto_unbox = TRUE))
    ),
    class = "response"
  )
  
  return(response)
}

#' Load a saved response fixture
#'
#' @param filename Name of the fixture file without extension
load_fixture <- function(filename) {
  filepath <- file.path("tests/fixtures", paste0(filename, ".rds"))
  
  if (!file.exists(filepath)) {
    stop("Fixture file does not exist: ", filepath)
  }
  
  readRDS(filepath)
}

# ---------------------------------------------------------------------
# Example Usage
# ---------------------------------------------------------------------

# Example workflow:
# 
# # 1. Load package and set credentials
# load_package()
# setup_credentials(use_env = TRUE)
# 
# # 2. Test connection
# test_api_connection()
# 
# # 3. Enable logging for debugging
# enable_http_logging()
# 
# # 4. Get a survey to work with
# survey_id <- get_test_survey_id()
# 
# # 5. Try fetching survey details and save response for fixture
# result <- safe_run(svmkR::fetch_survey_details(survey_id))
# if (result$status == "success") {
#   save_response(result$value, "survey_details_example")
# }
# 
# # 6. Disable logging when done
# disable_http_logging()

message("Debug helpers loaded. Use load_package() to start debugging.")


# --------------------------------------------------------
##### Questions #######
# --------------------------------------------------------
#' Analyze survey families and their associated question IDs
#'
#' This function extracts all question families from a survey object and maps
#' them to their associated question IDs. It also provides question text for
#' easier identification.
#'
#' @param surv_obj A survey object from fetch_survey_obj
#' @return A data frame containing columns for family, subtype, question_id, and question_text
#' @export
analyze_survey_families <- function(surv_obj) {
  # Initialize an empty list to store the results
  results <- list()
  
  # Iterate through all pages in the survey
  for (page_idx in seq_along(surv_obj$pages)) {
    page <- surv_obj$pages[[page_idx]]
    
    # Skip if no questions on this page
    if (length(page$questions) == 0) {
      next
    }
    
    # Iterate through all questions on the page
    for (q_idx in seq_along(page$questions)) {
      question <- page$questions[[q_idx]]
      question_id <- question$id
      
      # Get the family and subtype from the question
      family <- question$family
      subtype <- question$subtype
      
      # Get the question text (heading)
      question_text <- NA_character_
      if (!is.null(question$headings) && length(question$headings) > 0) {
        if (!is.null(question$headings[[1]]$heading)) {
          question_text <- question$headings[[1]]$heading
        }
      }
      
      # Add to results
      results[[length(results) + 1]] <- list(
        family = family,
        subtype = subtype,
        question_id = question_id,
        question_text = question_text
      )
    }
  }
  
  # Convert results to a data frame
  result_df <- dplyr::bind_rows(results)
  
  # Return a sorted data frame
  return(dplyr::arrange(result_df, family, subtype))
}

#' Summarize the families and subtypes present in a survey
#'
#' This function provides a summary count of all question families and subtypes
#' in a survey object.
#'
#' @param surv_obj A survey object from fetch_survey_obj
#' @return A data frame summarizing the counts of each family/subtype combination
#' @export
summarize_survey_families <- function(surv_obj) {
  # Get the detailed family analysis
  family_data <- analyze_survey_families(surv_obj)
  
  # Summarize by family and subtype
  summary <- family_data %>%
    dplyr::group_by(family, subtype) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(family, subtype)
  
  return(summary)
}


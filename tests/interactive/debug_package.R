#' Interactive Debugging Session for svmkR
#' 
#' This is the main entry point for debugging the svmkR package.
#' It provides a structured workflow for identifying and fixing bugs.

# Load our helper functions
source("tests/interactive/debug_helpers.R")

# ---------------------------------------------------------------------
# Main debugging workflow
# ---------------------------------------------------------------------

#' Initialize debugging environment
#' 
#' Sets up everything needed for a debugging session
initialize_debug_session <- function() {
  # Load the package
  load_package()
  
  # Check for credentials
  api_key <- Sys.getenv("SURVEYMONKEY_API_KEY", "")
  oauth_token <- Sys.getenv("SURVEYMONKEY_OAUTH_TOKEN", "")
  
  if (api_key == "" || oauth_token == "") {
    message("API credentials not found in environment variables.")
    message("Please run setup_credentials() with your API key and OAuth token before proceeding.")
  } else {
    setup_credentials(use_env = TRUE)
    message("API credentials loaded from environment variables.")
  }
  
  # Create log directory if it doesn't exist
  if (!dir.exists("tests/logs")) {
    dir.create("tests/logs", recursive = TRUE)
    message("Created logs directory at tests/logs/")
  }
  
  message("Debugging session initialized")
}

#' Run a systematic check of key package functionality
#'
#' This function tests all major components of the package
#' and reports any issues it finds
systematic_check <- function() {
  results <- list()
  
  # Function to run a check and record result
  run_check <- function(name, expr) {
    message(paste0("Testing: ", name, "..."))
    result <- safe_run(expr)
    results[[name]] <<- result
    
    # Log the result
    log_file <- file.path("tests/logs", paste0(name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
    saveRDS(result, log_file)
    
    # Print result status
    if (result$status == "success") {
      message(paste0("  ✓ ", name, " - Success"))
    } else {
      message(paste0("  ✗ ", name, " - Error: ", result$error$message))
    }
    
    return(result$status == "success")
  }
  
  # 1. Test API connection
  api_ok <- run_check("api_connection", {
    test_api_connection()
  })
  
  if (!api_ok) {
    message("API connection failed. Cannot proceed with further checks.")
    return(results)
  }
  
  # 2. Test browsing surveys
  run_check("browse_surveys", {
    surveys <- svmkR::browse_surveys(verb = FALSE)
    if (length(surveys$data) == 0) {
      stop("No surveys found in account")
    }
    surveys
  })
  
  # 3. Get a survey ID for further testing
  survey_id <- NULL
  run_check("get_survey_id", {
    survey_id <<- get_test_survey_id()
    survey_id
  })
  
  if (is.null(survey_id)) {
    message("Could not get a survey ID. Cannot proceed with survey-specific checks.")
    return(results)
  }
  
  # 4. Test fetching survey details
  run_check("fetch_survey_details", {
    svmkR::fetch_survey_details(survey_id)
  })
  
  # 5. Test fetching survey responses
  run_check("fetch_survey_responses", {
    svmkR::fetch_survey_responses(survey_id)
  })
  
  # 6. Test parsing a survey
  run_check("parse_survey", {
    svmkR::parse_survey(survey_id)
  })
  
  # Print summary
  message("\nTest Summary:")
  success_count <- sum(sapply(results, function(r) r$status == "success"))
  message(paste0(success_count, "/", length(results), " tests passed"))
  
  if (success_count < length(results)) {
    message("\nFailed tests:")
    failed_tests <- names(results)[sapply(results, function(r) r$status != "success")]
    for (test in failed_tests) {
      message(paste0("- ", test, ": ", results[[test]]$error$message))
    }
  }
  
  return(results)
}

#' Debug a specific function with tracing
#'
#' @param func_name Name of the function to debug
#' @param ... Arguments to pass to the function
debug_function <- function(func_name, ...) {
  # Enable HTTP logging to see API interactions
  enable_http_logging()
  
  # Set up tracing for the function and its key dependencies
  trace_function(func_name)
  
  # For certain functions, also trace their common dependencies
  if (func_name %in% c("fetch_survey", "parse_survey", "fetch_survey_responses")) {
    trace_function("standard_request_header")
    trace_function("process_request")
  }
  
  # Get the function object
  func <- get(func_name, envir = asNamespace("svmkR"))
  
  # Run the function with provided arguments
  message(paste0("Running ", func_name, " with tracing..."))
  result <- safe_run(do.call(func, list(...)))
  
  # Untrace functions
  untrace(func_name)
  if (func_name %in% c("fetch_survey", "parse_survey", "fetch_survey_responses")) {
    untrace("standard_request_header")
    untrace("process_request")
  }
  
  # Disable HTTP logging
  disable_http_logging()
  
  return(result)
}

#' Create a comparison report between expected and actual results
#'
#' @param expected The expected result
#' @param actual The actual result
#' @param save_to_file If TRUE, saves the report to a file
compare_results <- function(expected, actual, save_to_file = TRUE) {
  # Convert both to lists for easier comparison
  if (inherits(expected, "response")) {
    expected <- content(expected, "parsed")
  }
  if (inherits(actual, "response")) {
    actual <- content(actual, "parsed")
  }
  
  # Function to find differences recursively
  find_differences <- function(exp, act, path = "") {
    differences <- list()
    
    # Check if both are lists or vectors
    if (is.list(exp) && is.list(act)) {
      # Get all keys
      all_keys <- unique(c(names(exp), names(act)))
      
      # Check each key
      for (key in all_keys) {
        current_path <- if(path == "") key else paste0(path, "$", key)
        
        # Missing in actual
        if (!(key %in% names(act))) {
          differences[[length(differences) + 1]] <- list(
            path = current_path,
            issue = "missing_in_actual",
            expected = exp[[key]]
          )
        }
        # Missing in expected
        else if (!(key %in% names(exp))) {
          differences[[length(differences) + 1]] <- list(
            path = current_path,
            issue = "missing_in_expected",
            actual = act[[key]]
          )
        }
        # Both have the key, recurse if both are lists
        else if (is.list(exp[[key]]) && is.list(act[[key]])) {
          sub_diffs <- find_differences(exp[[key]], act[[key]], current_path)
          differences <- c(differences, sub_diffs)
        }
        # Both have the key, compare values
        else if (!identical(exp[[key]], act[[key]])) {
          differences[[length(differences) + 1]] <- list(
            path = current_path,
            issue = "value_mismatch",
            expected = exp[[key]],
            actual = act[[key]]
          )
        }
      }
    }
    # Check atomic vectors
    else if (is.atomic(exp) && is.atomic(act)) {
      if (!identical(exp, act)) {
        differences[[length(differences) + 1]] <- list(
          path = path,
          issue = "value_mismatch",
          expected = exp,
          actual = act
        )
      }
    }
    # Different types
    else {
      differences[[length(differences) + 1]] <- list(
        path = path,
        issue = "type_mismatch",
        expected_type = class(exp),
        actual_type = class(act)
      )
    }
    
    return(differences)
  }
  
  # Find all differences
  differences <- find_differences(expected, actual)
  
  # Create report
  report <- list(
    timestamp = Sys.time(),
    differences_count = length(differences),
    differences = differences
  )
  
  # Print summary
  message(paste0("Comparison found ", length(differences), " differences"))
  
  # Save report if requested
  if (save_to_file) {
    filename <- paste0("comparison_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    filepath <- file.path("tests/logs", filename)
    saveRDS(report, filepath)
    message(paste0("Report saved to ", filepath))
  }
  
  return(report)
}

#' Test a specific bug fix
#'
#' @param bug_name Name to identify this bug
#' @param test_func Function that reproduces the bug
#' @param fix_func Function implementing the fix
#' @param verification_func Function to verify the fix works
test_bug_fix <- function(bug_name, test_func, fix_func = NULL, verification_func = NULL) {
  # Create a log entry
  log_entry <- list(
    bug_name = bug_name,
    timestamp = Sys.time(),
    initial_test = NULL,
    fix_applied = FALSE,
    verification = NULL
  )
  
  message(paste0("Testing bug: ", bug_name))
  
  # Run the test function to reproduce the bug
  message("Attempting to reproduce the bug...")
  log_entry$initial_test <- safe_run(test_func())
  
  if (log_entry$initial_test$status == "success") {
    message("Note: The test function did not produce an error. This may indicate the bug is already fixed.")
  } else {
    message(paste0("Bug reproduced: ", log_entry$initial_test$error$message))
  }
  
  # Apply the fix if provided
  if (!is.null(fix_func)) {
    message("Applying fix...")
    fix_result <- safe_run(fix_func())
    log_entry$fix_applied <- fix_result$status == "success"
    
    if (log_entry$fix_applied) {
      message("Fix applied successfully")
    } else {
      message(paste0("Failed to apply fix: ", fix_result$error$message))
    }
  }
  
  # Verify the fix if provided
  if (!is.null(verification_func) && log_entry$fix_applied) {
    message("Verifying fix...")
    log_entry$verification <- safe_run(verification_func())
    
    if (log_entry$verification$status == "success") {
      message("Verification successful: Bug appears to be fixed!")
    } else {
      message(paste0("Verification failed: ", log_entry$verification$error$message))
    }
  }
  
  # Save the log entry
  log_file <- file.path("tests/logs", paste0("bugfix_", gsub(" ", "_", bug_name), "_", 
                                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
  saveRDS(log_entry, log_file)
  message(paste0("Bug test log saved to ", log_file))
  
  return(log_entry)
}

# ---------------------------------------------------------------------
# Example bug fixes
# ---------------------------------------------------------------------

# Example: Fix for a hypothetical issue where parse_survey fails on certain question types
fix_parse_survey_bug <- function() {
  # This is just a template - you would implement the actual fix here
  # Example implementation would be:
  # 1. Identify the problematic function in the package
  # 2. Create a fixed version of the function
  # 3. Replace the original function with the fixed version temporarily for testing
  
  message("This is a placeholder for an actual bug fix implementation")
  
  # Example of how to monkey-patch a function for testing:
  # old_parse_matrix <- svmkR:::parse_matrix_question
  # 
  # patched_parse_matrix <- function(...) {
  #   # Fixed implementation
  #   result <- tryCatch({
  #     old_parse_matrix(...)
  #   }, error = function(e) {
  #     # Handle specific error case that was causing problems
  #     # Return a valid result
  #   })
  #   return(result)
  # }
  # 
  # # Replace the function
  # unlockBinding("parse_matrix_question", as.environment("package:svmkR"))
  # assign("parse_matrix_question", patched_parse_matrix, asNamespace("svmkR"))
  # lockBinding("parse_matrix_question", as.environment("package:svmkR"))
  
  return(TRUE)  # Return success if the fix was applied
}

# ---------------------------------------------------------------------
# Start the debugging session
# ---------------------------------------------------------------------

message("Interactive debugging script loaded.")
message("Run initialize_debug_session() to begin debugging.")
message("Then run systematic_check() to test key package functionality.")
message("Use debug_function(\"function_name\", ...args) to debug specific functions.")

standard_request_header = function(token) {
  token = get_bearer_token(token)
  httr::add_headers(
    Authorization = token,
    "Accept" = "application/json",
    "Content-Type" = "application/json"
  )
}

sm_post = function(url, body, config, ...) {
  out = httr::POST(
    url,
    config = config,
    body = body,
    httr::user_agent("http://github.com/soubhikbarari/svmkR"),
    ...
  )

  remaining_request_message(out)
  reset_time_message(out)
  
  out.parsed = httr::content(out, as = "parsed")
  if ("error" %in% names(out.parsed)) {
    stop(jsonlite::toJSON(out.parsed, pretty=TRUE))
  } else {
    out.parsed
  }
}

sm_get = function(url, query, config, ...) {
  out = httr::GET(
    url,
    config = config,
    query = query,
    httr::user_agent("http://github.com/soubhikbarari/svmkR"),
    ...
  )

  remaining_request_message(out)
  reset_time_message(out)
  
  out.parsed = httr::content(out, as = "parsed")
  if ("error" %in% names(out.parsed)) {
    stop(jsonlite::toJSON(out.parsed, pretty=TRUE))
  } else {
    out.parsed
  }
}

remaining_request_message = function(response) {
  if(length(response$headers$`x-ratelimit-app-global-day-remaining`>0)) {
  message(paste0(
    "You have ",
    response$headers$`x-ratelimit-app-global-day-remaining`,
    " requests left today before you hit the limit"
  ))
  } else {
    warning("Could not determine API request limit")
  }
}

reset_time_message = function(response, frequency = 20) {
  if(length(response$headers$`x-ratelimit-app-global-day-remaining`>0)) {
  if (as.numeric(response$headers$`x-ratelimit-app-global-day-remaining`) %% frequency == 0) {
    message(paste0(
      "Your daily request limit will reset in ",
      response$headers$`X-Ratelimit-App-Global-Day-Reset`,
      " seconds"
    ))
  }
  } else {
    warning("Could not determine API request limit")
  }
}

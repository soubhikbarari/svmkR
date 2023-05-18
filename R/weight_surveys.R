#' Look up SurveyMonkey target populations
#' 
#' Look up all of the target population profiles (i.e. specific variables and their marginal and joint distributions) available to weight your survey to.
#'
#' @export
list_targets <- function() {
  targets.dirpath <- system.file("extdata", "targets", package = "svmkR")
  targets <- dir(targets.dirpath)
  return(targets)
}

#' Get a SurveyMonkey target population
#' 
#' Read in a default target population  (i.e. specific variables and their marginal and joint distributions) available through this package for you to weight your survey to.
#'
#' @export
get_target <- function(name) {
  target.dirpath <- system.file("extdata", "targets", name, package = "svmkR")
  if (target.dirpath == "") stop(sprintf("Could not find a target distribution named `%s`", name))
  target.files <- dir(target.dirpath)
  target.filepaths <- file.path(target.dirpath, target.files)  
  target <- lapply(target.filepaths, function(.) suppressMessages(readr::read_csv(.)))
  names(target) <- gsub("\\.csv", "", target.files)  
  class(target) <- c("target", class(target))
  return(target)
}

#' @method print target
#' @export
print.target <- function(target) {
  cat(sprintf("%d marginal distributions:\n\n", length(target)))
  for (t in 1:length(target)) {
    if (any(!is.null(names(target)))) {
      name <- names(target)[t]  
      message(sprintf("\033[0;32m%s\033[0m:", name))
    }
    print(target[[t]])
    prompt = "* Press [enter] to continue"
    invisible(readline(prompt=prompt))
    cat(paste0(rep("\b", nchar(prompt)+1), collapse=""))      
  }
}

check_target <- function(target) {
  for (t in 1:length(target)) {
    if (!("data.frame" %in% class(target[[t]]))) 
      stop(sprintf("Entry %d in target is not a valid dataframe", t))
    if (!("target" %in% colnames(target[[t]])))
      stop(sprintf("Entry %d in target does not have a `target` column", t))
  }
}

find_stems <- function(q) {
  # for multi-select questions
  return(unique(gsub(" - .*", "", q)))
}

#' Weight your survey
#'
#' Generate statistical weights for your survey responses to better represent some population of interest.
#' 
#' The default SurveyMonkey to weight surveys is via raking. For an overview of raking, see \href{https://www.abtassociates.com/raking-survey-data-aka-sample-balancing}{this guide}.
#'
#' @param data input survey data frame.
#' @param target either name of target population (run \code{list_targets()} for options) or a list of dataframes corresponding to different population distributions to weight survey to.
#' @param auto.remove remove any weighting variables that can't be found in survey.
#' @param initial.weights initial set of weights as starting point for raking algorithm.
#' @param trim.weights percentiles to trim your weights (default are 0.01 and .99, or 1\% and 99\%); can specify either an upper percentile or both a lower and upper percentile.
#' @param verbose output helpful progress and warning messages (recommended).
#' @export
weight_to <- function(data,
                      target = "us_genpop_acs18",
                      auto.remove = TRUE,
                      initial.weights = NULL,
                      trim.weights = c(0.01, .99),
                      verbose = TRUE) {
  data.cols <- colnames(data)
  data.cols <- data.cols[!(data.cols %in% c("collector_id","collection_mode","survey_id","response_id","respondent_id","custom_value","response_status","date_created","date_modified","first_name","last_name","email_address","ip_address"))]
  
  # read in target population margins
  if (all(typeof(target) == "character")) {
    target <- get_target(target)
  } else if (!("target" %in% class(d) | "list" %in% class(d))) {
    stop("Doesn't look like you passed in a valid target. Must be named target distribution or a list of dataframes.")
  }
  check_target(target)
  
  # map to SVMK question bank
  qmap <- suppressMessages(readr::read_csv(system.file("extdata", "qmap.txt", package = "svmkR")))
  
  data$i <- 1:nrow(data)
  data.mapped <- data.frame(i = 1:nrow(data))
  target.codebook <- data.frame()
  target.vars.all <- c()
  target.vars.failed <- c()
  margin.formulas <- list()
  pop.margins <- list()
  for (t in 1:length(target)) {
    target.t <- target[[t]]
    target.vars <- colnames(target.t)[grepl("[^(target)]", colnames(target.t))]
    
    for (v in 1:length(target.vars)) {
      target.var <- target.vars[v]
      if (target.var %in% colnames(data.mapped))
        next
      if (target.var %in% target.vars.failed)
        next
      
      # find candidate 
      q.candidates <- qmap[tolower(qmap$variable_name) == tolower(target.var),]
      q.candidates.text <- unique(q.candidates$question_text)
      
      for (cand in q.candidates.text) {
        cand.idx <- grepl(cand, data.cols, ignore.case = TRUE)
        if (any(cand.idx) == TRUE) {
          q.col.name <- data.cols[cand.idx]
          if (length(q.col.name) > 1) {
            # specific variables (e.g. race, party ID) may need multiple questions to code;
            # here we collapse those multiple questions into a single column
            race.rgx <- "(What is your race|Which racial group do you most identify with|What race or ethnicity best describes you|Which race or ethnicity best describes you)"
            if (all(grepl(race.rgx, q.col.name))) {
              q.col.name.stem <- find_stems(q.col.name)[1]
              
              data[q.col.name.stem] <- data[q.col.name] %>%
                purrr::map2_df(q.col.name, ~replace(.x, .x==T, .y)) %>% 
                apply(1, function(x) {
                  # collapse any multi-racial respondents into "Other"
                  ifelse(sum(!is.na(x)) == 1, x[!is.na(x)], 
                         ifelse(sum(!is.na(x)) == 0, NA, "Other"))
                })
              q.col.name <- q.col.name.stem
            } else if (FALSE) {
              # TODO: add any others here (e.g. party ID) as needed, following the above format for coding race
            } else {
              # otherwise just pick the first matching question
              q.col.name <- q.col.name[1]
            }
          }
          
          q.col.map <- q.candidates[q.candidates$question_text == cand,]
          message(sprintf("﹂\033[0;32m`%s`\033[0m → \033[0;36m`%s`\033[0m", q.col.name, target.var))
          
          if (grepl("ZIP code", q.col.name)) {
            data[[q.col.name]] <- ifelse(is.na(data[[q.col.name]]), NA, sprintf("%05d", as.numeric(data[[q.col.name]])))
          }
          
          # map question levels to target levels
          data.mapped[[target.var]] <- data[[q.col.name]] %>%
            forcats::fct_relabel(function(x) {
              for (c in 1:nrow(q.col.map)) {
                x[grepl(q.col.map$question_choice[c], x, ignore.case=T)] <- q.col.map$variable_level[c]
              }
              return(x)
            }) %>%
            forcats::fct_other(keep = unique(q.col.map$variable_level))
          if ("Other" %in% data.mapped[[target.var]] & !("Other" %in% unique(q.col.map$variable_level))) {
            data.mapped[[target.var]] <- suppressWarnings(forcats::fct_recode(data.mapped[[target.var]], `NULL`="Other"))
          }
          break
        }
      }
      if (is.null(data.mapped[[target.var]]) & !auto.remove) {
        stop(sprintf("\tWarning: could not find a question to map to `%s`", target.var))
      } else if (is.null(data.mapped[[target.var]]) & auto.remove) {
         message(sprintf("\tWarning: could not find a question to map to `%s` ... removing", target.var))
        target.vars.failed <- c(target.vars.failed, target.var)
      }
    }
    
    if (all(target.vars %in% colnames(data.mapped))) {
      collapse_vars <- function(x) ifelse(any(is.na(x)), NA, gsub("(\\:| )", "", paste(x, collapse="_")))
      
      # combine target variables in pop margins
      target.vars.cmbd <- collapse_vars(target.vars)
      target.t[target.vars.cmbd] <- apply(target.t, 1, function(.) collapse_vars(.[target.vars]))
      target.t <- target.t[c(target.vars.cmbd, "target")]
      
      # combine target variables in sample
      data.mapped[target.vars.cmbd] <- apply(data.mapped, 1, function(.) collapse_vars(.[target.vars]))
      
      # integerize both
      target.codebook <- rbind(target.codebook, 
                               data.frame(variable=target.vars.cmbd, value=unique(target.t[[target.vars.cmbd]]), level=1:length(unique(target.t[[target.vars.cmbd]]))))
      data.mapped[paste0(target.vars.cmbd,"_")] <- as.numeric(factor(data.mapped[[target.vars.cmbd]], levels = unique(target.t[[target.vars.cmbd]])))
      target.t[paste0(target.vars.cmbd,"_")] <- as.numeric(factor(target.t[[target.vars.cmbd]], levels = unique(target.t[[target.vars.cmbd]])))
      target.t[target.vars.cmbd] <- NULL
      
      # add pop margin
      pop.margins[[length(pop.margins)+1]] <- target.t
      
      # add margin formula
      margin.formulas[[length(margin.formulas)+1]] <- formula(paste0("~`", target.vars.cmbd,"_`"))
      
      # collect vars
      target.vars.all <- c(target.vars.all, target.vars.cmbd)
      
    }
  }
  
  if (all(!is.null(initial.weights))) {
    data.mapped$initwt <- initial.weights
  } else {
    data.mapped$initwt <- 1
  }
  data.mapped <- data.mapped[complete.cases(data.mapped),]
  message(sprintf("\nDropped %d (%0.0f%%) incomplete observations", 
                  nrow(data)-nrow(data.mapped),
                  100-(100*nrow(data.mapped)/nrow(data))))
  
  data.wtd <- rake(design = svydesign(ids = ~1, data = data.mapped, weights = data.mapped$initwt),
                   sample.margins = margin.formulas,
                   population.margins = pop.margins,
                   control = list(maxit = 200, epsilon = 1, verbose = FALSE))
  
  # trim weights
  data.mapped <- data.mapped %>%
    mutate(wt = weights(data.wtd)) %>%
    mutate(wt = wt * (n() / sum(wt)))
  
  message("\nRaw weight percentiles:")
  print(quantile(data.mapped$wt, probs=c(0.01,0.25,0.50,0.75,0.99)))
  
  if (all(!is.null(trim.weights)) | !(any(trim.weights == FALSE))) {
    
    trim.weights[trim.weights > 1] <- trim.weights[trim.weights > 1]/100
    trim.weights.pctl <- quantile(data.mapped$wt, probs = trim.weights)
    
    if (length(trim.weights.pctl) == 1) {
      message(sprintf("\nTrimming weights >= %0.3f", trim.weights.pctl))
      data.mapped <- data.mapped %>%
        mutate(wt = case_when(wt >= trim.weights.pctl ~ trim.weights.pctl,
                              TRUE ~ wt)) 
    } else {
      message(sprintf("\nTrimming weights <= %0.3f and >= %0.3f", trim.weights.pctl[1], trim.weights.pctl[2]))    
      data.mapped <- data.mapped %>%
        mutate(wt = case_when(wt <= trim.weights.pctl[1] ~ trim.weights.pctl[2],
                              wt >= trim.weights.pctl[2] ~ trim.weights.pctl[2],
                              TRUE ~ wt))     
    }
    data.mapped <- data.mapped %>%
      mutate(wt = wt * (n() / sum(wt)))
  }
  
  # compare distributions
  weight.summary <- data.mapped %>%
    dplyr::select_at(c(target.vars.all, paste0(target.vars.all,"_"), "wt")) %>%
    tidyr::gather(key="variable", value="value", c(target.vars.all)) %>%
    dplyr::group_by(variable, value) %>%
    dplyr::summarise(unweighted = n(), weighted = sum(wt), .groups = "drop") %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(unweighted = unweighted/sum(unweighted), weighted = weighted/sum(weighted)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(target.codebook, by = c("variable", "value")) %>%
    dplyr::left_join(rbindlist(lapply(pop.margins, function(x) { 
      x %>% 
        tidyr::gather(key="variable", value="level", -target) %>%
        dplyr::mutate(variable = trimws(variable, whitespace="\\_"))
    })), by = c("variable", "level")) %>%
    dplyr::select(variable, value, unweighted, weighted, target)
  
  avg.infl <- round(with(weight.summary, mean(weighted/unweighted)-1)*100, 2)
  avg.infl <- ifelse(avg.infl > 0, paste0("+",avg.infl,"%"), paste0(avg.infl,"%"))
  message(paste0("\nAverage strata inflation after weighting: ", avg.infl))
  
  avg.diff <- round(with(weight.summary, mean(weighted-target))*100, 2)
  avg.diff <- ifelse(avg.diff > 0, paste0("+",avg.diff,"%"), paste0(avg.diff,"%"))
  message(paste0("\nAverage strata weight - target weight: ", avg.diff))
  cat("\n")
  
  weights <- data["i"] %>%
    dplyr::left_join(data.mapped %>%
                       select(i, weight=wt),
                     by = "i") %>%
    dplyr::pull(weight)
  
  return(list(weights = weights,
              weight.summary = weight.summary))
}


rake <- function(design, 
                 sample.margins, 
                 population.margins, 
                 control = list(maxit = 200, epsilon = 1, verbose = FALSE), 
                 compress = NULL) 
{
  if (!missing(control)) {
    control.defaults <- formals(rake)$control
    for (n in names(control.defaults)) if (!(n %in% names(control))) 
      control[[n]] <- control.defaults[[n]]
  }
  is.rep <- inherits(design, "svyrep.design")
  if (is.rep && is.null(compress)) 
    compress <- inherits(design$repweights, "repweights_compressed")
  if (is.rep) 
    design$degf <- NULL
  if (length(sample.margins) != length(population.margins)) 
    stop("sample.margins and population.margins do not match.")
  nmar <- length(sample.margins)
  if (control$epsilon < 1) 
    epsilon <- control$epsilon * sum(weights(design, "sampling"))
  else epsilon <- control$epsilon
  strata <- lapply(sample.margins, function(margin) if (inherits(margin, 
                                                                 "formula")) {
    mf <- model.frame(margin, data = design$variables, na.action = na.fail)
  })
  allterms <- unique(unlist(lapply(sample.margins, all.vars)))
  # oldtable <- data.table::data.table(design$variables)[,sum(initwt),by=allterms]
  oldtable <- design$variables %>% 
    dplyr::group_by_at(allterms) %>% 
    dplyr::summarise(V1=sum(initwt), .groups = "drop")
  if (control$verbose) 
    print(oldtable)
  oldpoststrata <- design$postStrata
  iter <- 0
  converged <- FALSE
  while (iter < control$maxit) {
    design$postStrata <- NULL
    for (i in 1:nmar) {
      design <- survey::postStratify(design, strata[[i]], population.margins[[i]], 
                                     compress = FALSE, partial=TRUE)
    }
    design$variables$initwt <- weights(design)
    # newtable <- data.table::data.table(design$variables)[,sum(initwt),by=allterms]
    newtable <- design$variables %>% 
      dplyr::group_by_at(allterms) %>% 
      dplyr::summarise(V1=sum(initwt), .groups = "drop")    
    if (control$verbose) 
      print(newtable)
    delta <- max(abs(oldtable - newtable))
    if (delta < epsilon) {
      converged <- TRUE
      break
    }
    oldtable <- newtable
    iter <- iter + 1
  }
  rakestrata <- design$postStrata
  if (!is.null(rakestrata)) {
    class(rakestrata) <- "raking"
    design$postStrata <- c(oldpoststrata, list(rakestrata))
  }
  design$call <- sys.call()
  if (is.rep && compress)
    design$repweights <- survey::compressWeights(design$repweights)
  if (is.rep) 
    design$degf <- degf(design)
  if (!converged) 
    warning("Raking did not converge after ", iter, " iterations.\n")
  return(design)
}
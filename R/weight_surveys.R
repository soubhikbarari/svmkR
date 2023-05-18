#' Weight your survey
#'
#' Generate statistical weights for your survey responses to better represent some population of interest.
#' 
#' The default SurveyMonkey to weight surveys is via raking. For an overview of raking, see \href{https://www.abtassociates.com/raking-survey-data-aka-sample-balancing}{this guide}.
#'
#' @param data input survey data frame.
#' @param target name of target population, see \code{us_genpop_targets()} for options.
#' @param auto.remove remove any weighting variables that can't be found in survey.
#' @param initial.weights initial set of weights as starting point for raking algorithm.
#' @param trim.weights percentiles to trim your weights (default are 0.01 and .99, or 1\% and 99\%); can specify either an upper percentile or both a lower and upper percentile.
#' @export
weight_to_US_genpop <- function(data,
                                target = "acs18",
                                auto.remove = TRUE,
                                initial.weights = NULL,
                                trim.weights = c(0.01, .99)) {
  # read in target population margins
  targets.dirpath <- system.file("extdata", "targets/us_genpop", target, package = "svmkR")
  targets.files <- dir(targets.dirpath)
  targets.filepaths <- file.path(targets.dirpath, targets.files)
  targets <- lapply(targets.filepaths, function(.) suppressMessages(readr::read_csv(.)))
  names(targets) <- gsub("\\.csv", "", targets.files)
  
  # map to SVMK question bank
  qmap <- suppressMessages(readr::read_csv(system.file("extdata", "qmap.txt", package = "svmkR")))
  
  data$i <- 1:nrow(data)
  data.targets <- data.frame(i = 1:nrow(data))
  target.codebook <- data.frame()
  target.vars.all <- c()
  margin.formulas <- list()
  pop.margins <- list()
  for (t in 1:length(targets)) {
    target.t <- targets[[t]]
    target.vars <- colnames(target.t)[grepl("[^(target)]", colnames(target.t))]
    
    for (v in 1:length(target.vars)) {
      target.var <- target.vars[v]
      # find candidate 
      q.candidates <- qmap[tolower(qmap$variable_name) == tolower(target.var),]
      q.candidates.text <- unique(q.candidates$question_text)
      
      for (cand in q.candidates.text) {
        cand.idx <- grepl(cand, data.cols, ignore.case = TRUE)
        if (any(cand.idx) == TRUE) {
          q.col.name <- data.cols[cand.idx]
          q.col.map <- q.candidates[q.candidates$question_text == cand,]
          message(sprintf("﹂\033[0;32m`%s`\033[0m → \033[0;36m`%s`\033[0m", q.col.name, target.var))
          
          if (grepl("ZIP code", q.col.name)) {
            data[[q.col.name]] <- ifelse(is.na(data[[q.col.name]]), NA, sprintf("%05d", as.numeric(data[[q.col.name]])))
          }
          
          # map question levels to target levels
          data.targets[[target.var]] <- data[[q.col.name]] %>%
            forcats::fct_relabel(function(x) {
              for (c in 1:nrow(q.col.map)) {
                x[grepl(q.col.map$question_choice[c], x, ignore.case=T)] <- q.col.map$variable_level[c]
              }
              return(x)
            }) %>%
            suppressWarnings(forcats::fct_other(keep = q.col.map$variable_level))
          if ("Other" %in% data.targets[[target.var]]) {
            data.targets[[target.var]] <- suppressWarnings(forcats::fct_recode(data.targets[[target.var]], `NULL`="Other"))
          }
          break
        }
      }
      if (is.null(data.targets[[target.var]]) & !auto.remove) {
        stop(sprintf("Could not find a question to map to `%s`", target.var))
      } else if (is.null(data.targets[[target.var]]) & auto.remove) {
        message(sprintf("Could not find a question to map to `%s` ... removing", target.var))
      }
    }
    
    if (all(target.vars %in% colnames(data.targets))) {
      collapse_vars <- function(x) ifelse(any(is.na(x)), NA, gsub("(\\:| )", "", paste(x, collapse="_")))
      
      # combine target variables in pop margins
      target.vars.cmbd <- collapse_vars(target.vars)
      target.t[target.vars.cmbd] <- apply(target.t, 1, function(.) collapse_vars(.[target.vars]))
      target.t <- target.t[c(target.vars.cmbd, "target")]
      
      # combine target variables in sample
      data.targets[target.vars.cmbd] <- apply(data.targets, 1, function(.) collapse_vars(.[target.vars]))
      
      # integerize both
      target.codebook <- rbind(target.codebook, 
                               data.frame(variable=target.vars.cmbd, value=unique(target.t[[target.vars.cmbd]]), level=1:length(unique(target.t[[target.vars.cmbd]]))))
      data.targets[paste0(target.vars.cmbd,"_")] <- as.numeric(factor(data.targets[[target.vars.cmbd]], levels = unique(target.t[[target.vars.cmbd]])))
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
    data.targets$initwt <- initial.weights
  } else {
    data.targets$initwt <- 1
  }
  data.targets <- data.targets[complete.cases(data.targets),]
  message(sprintf("Dropping %d (%0.0f%%) incomplete observations", 
                  nrow(data)-nrow(data.targets),
                  100-(100*nrow(data.targets)/nrow(data))))
  
  data.wtd <- rake(design = svydesign(ids = ~1, data = data.targets, weights = data.targets$initwt),
                   sample.margins = margin.formulas,
                   population.margins = pop.margins,
                   control = list(maxit = 200, epsilon = 1, verbose = FALSE))
  
  # trim weights
  data.targets <- data.targets %>%
    mutate(wt = weights(data.wtd)) %>%
    mutate(wt = wt * (n() / sum(wt)))
  
  message("\nRaw weight percentiles:")
  print(quantile(data.targets$wt, probs=c(0.01,0.25,0.50,0.75,0.99)))
  
  if (all(!is.null(trim.weights)) | !(any(trim.weights == FALSE))) {
    
    trim.weights[trim.weights > 1] <- trim.weights[trim.weights > 1]/100
    trim.weights.pctl <- quantile(data.targets$wt, probs = trim.weights)
    
    if (length(trim.weights.pctl) == 1) {
      message(sprintf("\nTrimming weights >= %0.3f", trim.weights.pctl))
      data.targets <- data.targets %>%
        mutate(wt = case_when(wt >= trim.weights.pctl ~ trim.weights.pctl,
                              TRUE ~ wt)) 
    } else {
      message(sprintf("\nTrimming weights <= %0.3f and >= %0.3f", trim.weights.pctl[1], trim.weights.pctl[2]))    
      data.targets <- data.targets %>%
        mutate(wt = case_when(wt <= trim.weights.pctl[1] ~ trim.weights.pctl[2],
                              wt >= trim.weights.pctl[2] ~ trim.weights.pctl[2],
                              TRUE ~ wt))     
    }
    data.targets <- data.targets %>%
      mutate(wt = wt * (n() / sum(wt)))
  }
  
  # compare distributions
  weight.summary <- data.targets %>%
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
  cat(paste0("\nAverage strata inflation after weighting: ", avg.infl))
  
  avg.diff <- round(with(weight.summary, mean(weighted-target))*100, 2)
  avg.diff <- ifelse(avg.diff > 0, paste0("+",avg.diff,"%"), paste0(avg.diff,"%"))
  cat(paste0("\nAverage strata weight - target weight: ", avg.diff))
  cat("\n")
  
  weights <- data["i"] %>%
    dplyr::left_join(data.targets %>%
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
#' Estimate the Kish design effect of using a set of weights.
#' 
#' The Kish design effect is how much the variance of a sample estimator 
#' is expected to increase as a result of post-hoc sample weights.
#'
#' @param weights vector of sample weights used to adjust the sample.
#' @param na.rm trim entries that are NA.
#' @return the ratio of the actual sample size to the effective sample size with weights.
#' @export
design_eff <- function(weights, na.rm=F) {
  if (na.rm) weights <- weights[!is.na(weights)]
  n <- length(weights)
  deff <- (n*sum(weights^2))/(sum(weights)^2)
  return(deff)
}

#' Estimate the Kish effective sample size of using a set of weights.
#' 
#' The Kish effective sample size is the reduced size of our sample given the increase in variance.
#'
#' @param weights vector of sample weights used to adjust the sample.
#' @param na.rm trim entries that are NA.
#' @return the sample size that produces equivalent variance when introducing the sample weights.
#' @export
n_eff <- function(weights, na.rm=F) {
  if (na.rm) weights <- weights[!is.na(weights)]
  deff <- (sum(weights)^2)/(sum(weights^2))
  return(deff)
}

#' Simulate the Margin of Error for an overall survey or a single question.
#' 
#' For the overall survey: given weights and/or a sample size, the 
#' Maximum Margin of Error gives us the upper bound on the margin of error for all y/n questions across 
#' the entire survey -- by estimating it for a hypothetical y/n question with 50/50% split in the population 
#' (the inherently most uncertain population estimand).
#' 
#' For a single question: given a vector of sample responses and/or weights, the Margin of Error
#' estimates the margin of error by simulating the sampling distribution.
#'
#' Estimates via simulation tend to be more conservative (larger) than 
#' using the formulaic margin of error (\code{est_modeled_error}) as it incorporates 
#' more variance from the weights. 
#' 
#' This method is recommended for non-probability surveys with unknown sampling mechanisms (e.g. river samples).
#'
#' @param .data data frame to optionally pipe in \code{dplyr}-style (see example).
#' @param weights vector of sample weights used to adjust the sample (optional if \code{n} or \code{x} is specified).
#' @param n the sample size of the suvrey (not needed if \code{weights} or \code{x} is specified).
#' @param x the particular question to estimate margin of error for (if not specified, estimate the survey's MOE instead).
#' @param conf the level of statistical confidence to estimate the error.
#' @param sims number of bootstrap re-samples to perform in order to simulate the sampling distribution.
#' @param random whether or not to return different, random estimates every time.
#' 
#' @return The survey margin of error (MOE) or "modeled error" for the overall survey or question of interest.
#' @export
#' @examples
#' 
#' data(auto.evs)
#' if (FALSE) { ## not run
#'
#' # Unweighted maximum margin of error
#' simu_moe(n = nrow(auto.evs))
#' simu_moe(weights = rep(1, nrow(auto.evs)))
#' 
#' # Weighted maximum margin of error
#' simu_moe(weights = auto.evs$weight_genpop)
#' 
#' # Unweighted margin of error for specific question
#' simu_moe(x = auto.evs$ev_heard_1)
#' 
#' # Weighted margin of error for specific question
#' auto.evs %>%
#'   filter(!is.na(weight_genpop)) %>%
#'   simu_moe(x = ev_heard_1, weights = weight_genpop)
#' }
simu_moe <- function(.data = NULL, weights = NULL, n = NULL, x = NULL, conf = 0.95, sims = 5000, random = FALSE) {
  if (!is.null(.data)) {
    weights <- enquo(weights)
    x <- enquo(x)
    if (!is.null(weights)) weights <- dplyr::pull(.data, !!weights)
    if (!is.null(x)) x <- dplyr::pull(.data, !!x)
  }
  
  if (!random) set.seed(100)
  
  if (is.null(x) & (!is.null(n) | !is.null(weights))) {
    message("Estimating Maximum Margin of Error")
    # We are asking: what would the largest margin of error be for 
    # an estimated proportion -- i.e. the CI around a y/n question 
    # with an even split in the population? (this is the highest possible 
    # variance Bernoulli population distribution)    
  } else if (!is.null(x)) {
    message("Estimating Margin of Error given `x`")
  } else {
    stop("Must specify either `x` or `n`/`weights`")
  }
  
  n <- c(n, length(x), length(weights))[1]
  wt <- if (is.null(weights) | length(weights)==0) rep(1, n) else weights
  wt <- wt[!is.na(wt)]
  
  if (is.null(x)) {
    # Create pseudo-population success trials
    pop.yes <- as.numeric(cumsum(wt) > (sum(wt)/2)) ## elegant random assignment
    # Assume true success rate (given weights)
    pop.prob <- sum(pop.yes * wt)/sum(wt)
  } else {
    x <- as.numeric(as.numeric(x) %in% c(1))
    x <- x[!is.na(x)]
    if (length(x) != length(wt)) stop("Length of `x` doesn't match up with `weights`")
    if (length(x)==0 | length(wt)==0) stop("No non-null values in `x` or `weights`")
    pop.yes <- x
    pop.prob <- sum(x * wt)/sum(wt)
  }
  
  # Estimate the success rate over repeated bootstraps
  pop.prob.ests <- pbapply::pbsapply(1:sims, function(b){
    idx.b <- sample(1:length(wt), replace = TRUE)
    samp.yes.b <- pop.yes[idx.b]
    samp.wt.b <- wt[idx.b]
    pop.prob.est.b <- sum(samp.wt.b * samp.yes.b)/sum(samp.wt.b)
    pop.prob.est.b
  })
  # Estimate the error of the resulting CI
  pop.prob.est.ci <- quantile(pop.prob.ests, p = c(0.5-(conf/2), 0.5+(conf/2)))
  err <- mean(abs(pop.prob - pop.prob.est.ci[1]))
  
  return(err)
}

#' Estimate the Margin of Error (or "modelled error") for an overall survey or a single question.
#' 
#' For the overall survey: given weights and/or a sample size, the 
#' Maximum Margin of Error gives us the upper bound on the margin of error for all y/n questions across 
#' the entire survey -- by estimating it for a hypothetical y/n question with 50/50% split in the population 
#' (the inherently most uncertain population estimand).
#' 
#' For a single question: given a vector of sample responses and/or weights, the Margin of Error
#' estimates the margin of error by plugging into known quantiles of the asymptotic sampling distribution.
#'
#' Estimates from this asymptotic formula tend to be smaller than
#' using the simulated margin of error (\code{sim_modeled_error}) as it assumes 
#' all our survey's questions follow an asymptotic normal distribution 
#' (i.e. a large sample of independent observations). 
#' 
#' For non-probability surveys without design weights, it is recommended to use `sim_modeled_error`.
#'
#' @param .data data frame to optionally pipe in \code{dplyr}-style (see example).
#' @param weights vector of sample weights used to adjust the sample (optional if \code{n} or \code{x} is specified).
#' @param n the sample size of the suvrey (not needed if \code{weights} or \code{x} is specified).
#' @param x the particular question to estimate margin of error for (if not specified, estimate the survey's MOE instead).
#' @param conf the level of statistical confidence to estimate the error.
#' 
#' @return The survey margin of error (MOE) or "modeled error" for the overall survey or question of interest.
#' @export
#' @examples
#' 
#' data(auto.evs)
#' if (FALSE) { ## not run
#'
#' # Unweighted maximum margin of error
#' esti_moe(n = nrow(auto.evs))
#' esti_moe(weights = rep(1, nrow(auto.evs)))
#' 
#' # Weighted maximum margin of error
#' esti_moe(weights = auto.evs$weight_genpop)
#' 
#' # Unweighted margin of error for specific question
#' esti_moe(x = auto.evs$ev_heard_1)
#' 
#' # Weighted margin of error for specific question
#' auto.evs %>%
#'   filter(!is.na(weight_genpop)) %>%
#'   esti_moe(x = ev_heard_1, weights = weight_genpop)
#' }
esti_moe <- function(.data = NULL, weights = NULL, n = NULL, x = NULL, conf = 0.95) {
  if (!is.null(.data)) {
    weights <- enquo(weights)
    x <- enquo(x)
    if (!is.null(weights)) weights <- dplyr::pull(.data, !!weights)
    if (!is.null(x)) x <- dplyr::pull(.data, !!x)
  }
  
  if (is.null(x) & (!is.null(n) | !is.null(weights))) {
    message("estimating Maximum Margin of Error")
    # We are asking: what would the largest margin of error be for 
    # an estimated proportion -- i.e. the CI around a y/n question 
    # with an even split in the population? (this is the highest possible 
    # variance Bernoulli population distribution)
  
    # For reference: 
    # https://www.pewresearch.org/internet/2010/04/27/methodology-85/    
  } else if (!is.null(x)) {
    message("estimating Margin of Error given `x`")
  } else {
    stop("must specify either `x` or `n`/`weights`")
  }
  
  n <- c(n, length(x), length(weights))[1]
  wt <- if (is.null(weights) | length(weights)==0) rep(1, n) else weights
  wt <- wt[!is.na(wt)]

  
  n <- length(wt)
  deff <- (n*sum(wt^2))/(sum(wt)^2)
  q.conf <- qnorm(0.5+(conf/2))
  if (is.null(x)) {
    est.var <- 0.5*0.5
  } else {
    x <- as.numeric(as.numeric(x) %in% c(1))
    x <- x[!is.na(x)]
    if (length(x) != length(wt)) stop("length of `x` doesn't match up with `weights`")
    if (length(x)==0 | length(wt)==0) stop("no non-null values in `x` or `weights`")    
    est.var <- mean(x)*(1-mean(x))
  }
  
  err <- sqrt(deff)*sqrt(est.var/n)
  
  return(err)
}
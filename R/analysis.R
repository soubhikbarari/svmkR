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

#' Simulate the Margin of Error (or "modelled error") for a survey.
#' 
#' Given weights and a sample size, the Modeled Error gives us the upper bound on the margin of error
#' for all y/n questions across the entire survey -- by estimating it
#' for a hypothetical y/n question with 50/50% split in the population (the inherently
#' most uncertain population estimand).
#'
#' Estimates via simulation tend to be more conservative (larger) than 
#' using the formulaic margin of error (\code{est_modeled_error}) as it incorporates 
#' more variance from the weights. 
#' 
#' This method is recommended for non-probability surveys with unknown sampling mechanisms (e.g. river samples).
#'
#' @param weights vector of sample weights used to adjust the sample.
#' @param conf the level of statistical confidence to estimate the error.
#' @param sims number of bootstrap re-samples to perform in order to simulate the sampling distribution.
#' @param random whether or not to return different, random estimates every time.
#' 
#' @return the survey margin of error (SMOE) or "modeled error" for the overall survey.
#' @export
simu_moe <- function(weights, conf = 0.95, sims = 5000, random = FALSE) {
  if (!random) set.seed(100)
  
  wt <- weights
  wt <- wt[!is.na(wt)]
  
  # We are asking: what would the largest margin of error be for 
  # an estimated proportion -- i.e. the CI around a y/n question 
  # with an even split in the population? (this is the highest possible 
  # variance Bernoulli population distribution)
  
  # Create pseudo-population success trials
  pop.yes <- as.numeric(cumsum(wt) > (sum(wt)/2)) ## elegant random assignment
  
  # Assume true success rate (given weights)
  pop.prob <- sum(pop.yes * wt)/sum(wt)
  
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

#' Estimate the Margin of Error (or "modelled error") for a survey.
#' 
#' Given weights and a sample size, the Modeled Error gives us the upper bound on the margin of error
#' for all y/n questions across the entire survey -- by estimating it
#' for a hypothetical y/n question with 50/50% split in the population (the inherently
#' most uncertain population estimand).
#'
#' Estimates from this asymptotic formula tend to be smaller than
#' using the simulated margin of error (\code{sim_modeled_error}) as it assumes 
#' all our survey's questions follow an asymptotic normal distribution 
#' (i.e. a large sample of independent observations). 
#' 
#' For non-probability surveys without design weights, it is recommended to use `sim_modeled_error`.
#'
#' @param weights vector of sample weights used to adjust the sample.
#' @param conf the level of statistical confidence to estimate the error.
#' 
#' @return the survey margin of error (SMOE) or "modeled error" for the overall survey.
#' @export
esti_moe <- function(weights = NULL, conf = 0.95) {
  
  wt <- weights
  wt <- wt[!is.na(wt)]
  
  # We are asking: what would the largest margin of error be for 
  # an estimated proportion -- i.e. the CI around a y/n question 
  # with an even split in the population? (this is the highest possible 
  # variance Bernoulli population distribution)
  
  # For reference: 
  # https://www.pewresearch.org/internet/2010/04/27/methodology-85/
  
  n <- length(wt)
  deff <- (n*sum(wt^2))/(sum(wt)^2)
  q.conf <- qnorm(0.5+(conf/2))
  var.hard <- 0.5*0.5
  
  err <- sqrt(deff)*sqrt(var.hard/n)
  
  return(err)
}
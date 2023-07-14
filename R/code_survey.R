find_qvar_col_idx <- function(data, rgx) {
  q_1 <- colnames(data)
  q_2 <- lapply(colnames(data), function(.) attr(data[[.]], "label"))
  q_1_idx <- grepl(rgx, q_1)
  q_2_idx <- grepl(rgx, q_2)
  if (length(q_1_idx) > 0) {
    return(q_1_idx)
  } else if (length(q_2_idx) > 0) {
    return(q_2_idx)
  } else {
    return(numeric(0))
  }
}

code_success_msg <- function(var_name) {
  message(sprintf("\033[0;32mcoded\033[0m \033[0;36m`%s`\033[0m", var_name))
}

code_fail_msg <- function(var_name) {
  message(sprintf("could not code `%s`", var_name))
}

is_valid <- function(var_name) {
  length(var_name)==1 & !is.na(var_name)
}

#' Code demographic variables in responses
#' 
#' Given a dataframe of survey responses, look for standard SurveyMonkey demographic questions and code/recode them into new variables that are more banner-, plot-, and analysis-friendly. Use \code{code_std_demo_vars()} to perform all standard demographic recodes, or select the specific function you need. Run \code{std_demo_vars()} to return the full index of SurveyMonkey's standard demographic variables. 
#'
#' @param data Input data frame of survey responses.
#' @param append If TRUE, append to existing dataframe otherwise return new variables in a fresh dataframe.
#' @param verbose Print helpful messages.
#' @return Survey data frame with new (or updated) columns corresponding to all discoverable demographic variables for respondents.
#' @export 
code_std_demo_vars <- function(data, append = TRUE, verbose = TRUE) {
  data <- data %>%
    code_party_vars(., append = TRUE, verbose = verbose) %>%
    code_ideo_vars(., append = TRUE, verbose = verbose) %>%
    code_age_vars(., append = TRUE, verbose = verbose) %>%
    code_race_vars(., append = TRUE, verbose = verbose) %>%
    code_sex_vars(., append = TRUE, verbose = verbose) %>%
    code_educ_vars(., append = TRUE, verbose = verbose) %>%
    code_income_vars(., append = TRUE, verbose = verbose) %>%
    code_geo_vars(., append = TRUE, verbose = verbose)
  
  if (append)
    return(data)
  else
    return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
}

#' @inherit code_std_demo_vars
#' @export
std_demo_vars <- function(data) {
  return(c(
    "party5", "party3",
    "ideo5", "ideo3",
    "age", "age3", "age6", "age_gen",
    "race5", "race4", "nonwhite",
    "lgbtq", "lgbtq2", "transgender", "gender_binary", "gender",
    "educ", "educ2", "educ4",
    "income", "income3",
    "zip", "urban4", "state_abb", "state_name", "region", "division"
  ))
}

#' @inherit code_std_demo_vars
#' @export
#' @return Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{U.S. political party affiliation}: \itemize{
#'  \item \code{party5}: 5-point U.S. political party leaning (i.e. Dem, Lean Dem, Ind, Lean Rep, Rep)
#'  \item \code{party3}: 3-point U.S. political party affiliation (i.e. "forced" to Dem, Ind, Rep)
#' }
code_party_vars <- function(data, append = TRUE, verbose = TRUE) {
  party_id_rgx <- "In politics today, do you consider yourself a Republican, Democrat, or Independent?"
  party_id_idx <- find_qvar_col_idx(data, party_id_rgx)
  party_id <- colnames(data)[party_id_idx][1]
  
  party_lean_rgx <- "As of today, do you lean more to the Republican Party or more to the Democratic Party?"
  party_lean_idx <- find_qvar_col_idx(data, party_lean_rgx)
  party_lean <- colnames(data)[party_lean_idx][1]
  
  if (is_valid(party_id)) {
    if (is_valid(party_lean)) {
      data <- data %>% 
        dplyr::mutate(party5 = dplyr::case_when(
          .[[party_id]] == "Republican" ~ "Republican",
          .[[party_id]] == "Democrat" ~ "Democrat",
          .[[party_id]] == "Independent" & .[[party_lean]] == "Republican" ~ "Lean Republican",
          .[[party_id]] == "Independent" & .[[party_lean]] == "Democrat" ~ "Lean Democrat",
          .[[party_lean]] == "Neither" ~ "True Independent",
          TRUE ~ NA
        )) %>% 
        mutate(party5 = factor(
          party5, 
          levels = c("Democrat","Lean Democrat","True Independent","Lean Republican","Republican"))
        ) %>% 
        mutate(party5 = magrittr::set_attr(party5, "label", "Party ID (5-point lean)"))
      if (verbose) code_success_msg("party5")
      
      data <- data %>%
        dplyr::mutate(party3 = dplyr::case_when(
          .[[party_id]] == "Republican" ~ "Republican",
          .[[party_id]] == "Democrat" ~ "Democrat",
          .[[party_id]] == "Independent" & .[[party_lean]] == "Republican" ~ "Republican",
          .[[party_id]] == "Independent" & .[[party_lean]] == "Democrat" ~ "Democrat",
          .[[party_lean]] == "Neither" ~ "True Independent",
          TRUE ~ NA
        )) %>% 
        mutate(party3 = factor(
          party3, 
          levels = c("Democrat","True Independent","Republican"))
        ) %>% 
        mutate(party3 = magrittr::set_attr(party3, "label", "Party ID (3-point forced)"))
      if (verbose) code_success_msg("party3")      
      
    } else {
      data <- data %>%
        dplyr::mutate(party3 = dplyr::case_when(
          .[[party_id]] == "Republican" ~ "Republican",
          .[[party_id]] == "Democrat" ~ "Democrat",
          .[[party_id]] == "Independent" ~ "Independent",
          TRUE ~ NA
        )) %>% 
        mutate(party3 = factor(
          party3, 
          levels = c("Democrat","Independent","Republican"))
        ) %>% 
        mutate(party3 = magrittr::set_attr(party3, "label", "Party ID (3-point)"))
      if (verbose) code_success_msg("party3 (no lean)")
      if (verbose) code_fail_msg("party5")
    }
  } else {
    if (verbose) code_fail_msg("party3")
    if (verbose) code_fail_msg("party5")
  }
  
  if (append)
    return(data)
  else
    return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
}

#' @inherit code_std_demo_vars
#' @export
#' @return Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{political ideology}: \itemize{
#'  \item \code{ideo5}: 5-point political ideology (i.e. Very Cons, Cons, Mod, Lib, Very Lib)
#'  \item \code{ideo3}: 3-point political ideology (i.e. Cons, Mod, Lib)
#' }
code_ideo_vars <- function(data, append = TRUE, verbose = TRUE) {
  ideo_rgx <- "In general, how would you describe your views on most political issues?"
  ideo_idx <- find_qvar_col_idx(data, ideo_rgx)
  ideo <- colnames(data)[ideo_idx][1]
  
  if (is_valid(ideo)) {
    data <- data %>% 
      dplyr::mutate(ideo5 = .[[ideo]],
                    ideo3 = forcats::fct_collapse(ideo5,
                                                  "Conservative" = c("Very conservative", "Conservative"),
                                                  "Moderate" = "Moderate",
                                                  "Liberal" = c("Liberal", "Very liberal"))) %>%
      dplyr::mutate(ideo3 = factor(ideo3, 
                                   levels = c("Liberal","Moderate","Conservative"))) %>% 
      dplyr::mutate(ideo5 = magrittr::set_attr(ideo5, "label", "Political ideology (5-point)"),
                    ideo3 = magrittr::set_attr(ideo3, "label", "Political ideology (3-point)"))
    if (verbose) code_success_msg("ideo3")
    if (verbose) code_success_msg("ideo5")
  } else {
    if (verbose) code_fail_msg("ideo3")
    if (verbose) code_fail_msg("ideo5")
  }
  
  if (append)
    return(data)
  else
    return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
}

#' @inherit code_std_demo_vars
#' @export
#' @return Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{age category and generation}: \itemize{
#'  \item \code{age}: integer age in years 
#'  \item \code{age3}: 3-point age category
#'  \item \code{age6}: 6-point age category
#'  \item \code{age_gen}: 4-point age category corresponding to generation (according to Pew Research Center's now-retired classification)
#' }
code_age_vars <- function(data, append = TRUE, verbose = TRUE) {
  data$age <- NULL
  age_rgx <- "What is your( current)? age"
  age_idx <- find_qvar_col_idx(data, age_rgx)
  age <- colnames(data)[age_idx][1]
  if (is_valid(age)) {
    data <- data %>%
      dplyr::mutate(age = as.numeric(.[[age]])) %>%
      dplyr::mutate(age6 = factor(dplyr::case_when(
        age %in% 18:24 ~ "18 - 24",
        age %in% 25:34 ~ "25 - 34",
        age %in% 35:44 ~ "35 - 44",
        age %in% 45:54 ~ "45 - 54",
        age %in% 55:64 ~ "55 - 64",
        age %in% 65:100 ~ "65 and up"
      ), levels = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 64", "65 and up"))) %>%
      dplyr::mutate(age3 = factor(dplyr::case_when(
        age %in% 18:34 ~ "18 - 34",
        age %in% 35:64 ~ "35 - 64",
        age %in% 65:100 ~ "65 and up"
      ), levels = c("18 - 34", "35 - 64", "65 and up"))) %>%
      dplyr::mutate(age_gen = factor(dplyr::case_when(
        as.numeric(format(Sys.Date(), "%Y")) - age >= 1997 ~ "Gen Z (Born 1997+)",
        as.numeric(format(Sys.Date(), "%Y")) - age >= 1981 ~ "Millennial (Born 1981-96)",
        as.numeric(format(Sys.Date(), "%Y")) - age >= 1965 ~ "Gen X (Born 1965-80)",
        as.numeric(format(Sys.Date(), "%Y")) - age >= 1946 ~ "Boomer (Born 1946-64)",
        as.numeric(format(Sys.Date(), "%Y")) - age >= 1928 ~ "Silent (Born 1928-45)",
      ), levels = c("Gen Z (Born 1997+)",
                    "Millennial (Born 1981-96)",
                    "Gen X (Born 1965-80)",
                    "Boomer (Born 1946-64)",
                    "Silent (Born 1928-45)"))) %>%
      dplyr::mutate(age = magrittr::set_attr(age, "label", "Age (years)"),
                    age3 = magrittr::set_attr(age3, "label", "Age (3-point)"),
                    age6 = magrittr::set_attr(age6, "label", "Age (6-point)"),
                    age_gen = magrittr::set_attr(age_gen, "label", "Generational cohort"))
    if (verbose) code_success_msg("age")
    if (verbose) code_success_msg("age3")
    if (verbose) code_success_msg("age6")
    if (verbose) code_success_msg("age_gen")    
  } else {
    if (verbose) code_fail_msg("age")
    if (verbose) code_fail_msg("age3")
    if (verbose) code_fail_msg("age6")
    if (verbose) code_fail_msg("age_gen")      
  }
  
  if (append)
    return(data)
  else
    return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
}

#' @inherit code_std_demo_vars
#' @export
#' @return Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{race and/or ethnicity} \itemize{
#'  \item \code{race5}: 5-point standard Census race category (i.e. Non-Hispanic White, Non-Hispanic Black, Hispanic, Asian, Other)
#'  \item \code{race4}: 4-point categorization collapsing "Asian" into "Other" (needed for some weighting schemes)
#'  \item \code{nonwhite}: 2-point race category for white/non-white
#' }
code_race_vars <- function(data, append = TRUE, verbose = TRUE) {
  race_rgx <- "Which race or ethnicity best describes you\\? \\(Select all that apply\\)"
  race_idx <- find_qvar_col_idx(data, race_rgx)
  race <- colnames(data)[race_idx][1]
  hispanic_rgx <- "Are you of Hispanic or Latino\\/a origin"
  hispanic_idx <- find_qvar_col_idx(data, hispanic_rgx)
  hispanic <- colnames(data)[hispanic_idx][1]
  if (is_valid(race) & is_valid(hispanic)) {
    data <- data %>%
      dplyr::mutate(race_1 = as.integer(`Which race or ethnicity best describes you? (Select all that apply) - White`),
                    race_2 = as.integer(`Which race or ethnicity best describes you? (Select all that apply) - Hispanic or Latino/a`),
                    race_3 = as.integer(`Which race or ethnicity best describes you? (Select all that apply) - Black or African-American`),
                    race_4 = as.integer(`Which race or ethnicity best describes you? (Select all that apply) - Asian or Asian-American`),
                    race_5 = as.integer(`Which race or ethnicity best describes you? (Select all that apply) - Middle Eastern or North African`),
                    race_6 = as.integer(`Which race or ethnicity best describes you? (Select all that apply) - Native Hawaiian or Pacific Islander`),
                    race_7 = as.integer(`Which race or ethnicity best describes you? (Select all that apply) - Native American or American Indian`),
                    race_8 = dplyr::case_when(is.na(`Which race or ethnicity best describes you? (Select all that apply) - Other (please specify)`) == F ~ as.integer(1)),
                    hispanic = as.integer(`Are you of Hispanic or Latino/a origin?`)) %>%
      dplyr::mutate(sum_race = dplyr::select(., race_1:race_8) %>% 
                      rowSums(na.rm = TRUE)) %>%
      dplyr::mutate(race5 = dplyr::case_when(
        # Non-Hispanic white
        race_1 == 1 & sum_race == 1 & hispanic !=1 ~ "White",
        # Non-Hispanic black
        race_3 == 1 & sum_race == 1 & hispanic !=1 ~ "Black",
        # Hispanic
        race_2 == 1 | hispanic == 1 ~ "Hispanic",
        # Asian
        race_4 == 1 & sum_race == 1 & hispanic !=1 ~ "Asian",
        sum_race > 0 ~ "Other"
      )) %>%
      dplyr::mutate(race5 = forcats::fct_relevel(race5, "White", "Black", "Hispanic", "Asian", "Other")) %>%
      dplyr::mutate(race4 = forcats::fct_collapse(race5,
                                                  "White" = "White",
                                                  "Black" = "Black",
                                                  "Hispanic" = "Hispanic",
                                                  "Other" = c("Asian", "Other"))) %>%
      dplyr::mutate(race4 = forcats::fct_relevel(race4, "White", "Black", "Hispanic",  "Other")) %>%
      dplyr::mutate(nonwhite = forcats::fct_collapse(race5,
                                                     "White" = "White",
                                                     "Non-White" = c("Black", "Hispanic", "Asian", "Other"))) %>%
      dplyr::mutate(nonwhite = forcats::fct_relevel(nonwhite, "White", "Non-White")) %>%
      dplyr::select(-(race_1:race_8), -sum_race) %>%
      dplyr::mutate(race5 = magrittr::set_attr(race5, "label", "Race (5-point)"),
                    race4 = magrittr::set_attr(race4, "label", "Race (4-point)"),
                    nonwhite = magrittr::set_attr(nonwhite, "label", "Race (White/Non-white)"))
    if (verbose) code_success_msg("race5")
    if (verbose) code_success_msg("race4")
    if (verbose) code_success_msg("nonwhite")
  } else {
    if (verbose) code_fail_msg("race5")
    if (verbose) code_fail_msg("race4")
    if (verbose) code_fail_msg("nonwhite")
  }
  
  if (append)
    return(data)
  else
    return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
}

#' @inherit code_std_demo_vars
#' @export
#' @return Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{gender identity and sexual orientation}: \itemize{
#'  \item \code{lgbtq}: 5-point sexual orientation scale (Lesbian, Gay, Bisexual, Heterosexual, Other)
#'  \item \code{lgbtq2}: 2-point sexual orientation scale (Heterosexual, Non-heterosexual)
#'  \item \code{transgender}: binary indicator for transgender identity or not
#'  \item \code{gender_binary}: binary indicator for gender
#'  \item \code{gender}: 3-point scale for gender allowing for non-binary gender identity
#' }
code_sex_vars <- function(data, append = TRUE, verbose = TRUE) {
  data$lgbtq <- NULL
  lgbtq_rgx <- "What is your sexual orientation"
  lgbtq_idx <- find_qvar_col_idx(data, lgbtq_rgx)
  lgbtq <- colnames(data)[lgbtq_idx][1]  
  if (is_valid(lgbtq)) {
    data <- data %>% 
      dplyr::mutate(lgbtq = factor(dplyr::case_when(
        grepl("Heterosexual", `What is your sexual orientation?`) ~ "Heterosexual",
        grepl("Gay", `What is your sexual orientation?`) ~ "Gay",
        grepl("Lesbian", `What is your sexual orientation?`) ~ "Lesbian",
        grepl("Bisexual", `What is your sexual orientation?`) ~ "Bisexual",
        !is.na(`What is your sexual orientation?`) ~ "Other"
      ), levels = c("Lesbian", "Gay", "Bisexual", "Heterosexual", "Other"))) %>%
      dplyr::mutate(lgbtq2 = factor(dplyr::case_when(
        grepl("Heterosexual", `What is your sexual orientation?`) ~ "Heterosexual",
        !is.na(`What is your sexual orientation?`) ~ "Non-heterosexual"
      ), levels = c("Heterosexual","Non-heterosexual"))) %>%
      dplyr::mutate(lgbtq = magrittr::set_attr(lgbtq, "label", "Sexual orientation (5-point)"),
                    lgbtq2 = magrittr::set_attr(lgbtq2, "label", "Sexual orientation (2-point)"))
    
    if (verbose) code_success_msg("lgbtq")
    if (verbose) code_success_msg("lgbtq2")
  } else {
    if (verbose) code_fail_msg("lgbtq")
    if (verbose) code_fail_msg("lgbtq2")
  }
  
  data$transgender <- NULL
  trans_rgx <- "Are you transgender"
  trans_idx <- find_qvar_col_idx(data, trans_rgx)
  trans <- colnames(data)[trans_idx][1]  
  if (is_valid(trans)) {
    data <- data %>% 
      dplyr::mutate(transgender = factor(`Are you transgender?`, 
                                         levels = c("Yes", "No", "Prefer not to say"))) %>%
      dplyr::mutate(transgender = magrittr::set_attr(transgender, "label", "Transgender?"))
    if (verbose) code_success_msg("transgender")
  } else {
    if (verbose) code_fail_msg("transgender")
  }
  
  data$gender <- NULL
  data$gender_binary <- NULL
  gender_rgx <- "Are you transgender"
  gender_idx <- find_qvar_col_idx(data, gender_rgx)
  gender <- colnames(data)[gender_idx][1]  
  if (is_valid(gender)) {
    data <- data %>% 
      dplyr::mutate(gender = `Which gender best describes you?`) %>%
      dplyr::mutate(gender_binary = dplyr::case_when(
        `Which gender best describes you?` == "Male" ~ "Male",
        `Which gender best describes you?` == "Female" ~ "Female",
        TRUE ~ NA
      )) %>%
      dplyr::mutate(gender = magrittr::set_attr(gender, "label", "Gender (3-point)"),
                    gender_binary = magrittr::set_attr(gender_binary, "label", "Gender (binary)"))
    if (verbose) code_success_msg("gender")
    if (verbose) code_success_msg("gender_binary")    
  } else {
    if (verbose) code_fail_msg("gender")
    if (verbose) code_fail_msg("gender_binary")    
  }
  
  if (append)
    return(data)
  else
    return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
}

#' @inherit code_std_demo_vars
#' @export
#' @return Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{educational attainment}: \itemize{
#'  \item \code{educ2}: binary indicator for college vs. not
#'  \item \code{educ4}: 4-point educational attainment scale
#'  \item \code{educ}: 6-point educational attainment scale
#' }
code_educ_vars <- function(data, append = TRUE, verbose = TRUE) {
  data$educ2 <- NULL
  data$educ4 <- NULL
  data$educ <- NULL
  edu_rgx <- "What is the.*(highest level|last grade).*(school|education).*you.*(completed|attained|finished)"
  edu_idx <- find_qvar_col_idx(data, edu_rgx)
  edu <- colnames(data)[edu_idx][1]
  if (is_valid(edu)) {  
    data <- data %>%
      dplyr::mutate(educ = .[[edu]]) %>%
      dplyr::mutate(educ4 = forcats::fct_relevel(forcats::fct_collapse(
        educ,
        "High school or less"  = c("Did not complete high school", "High school or G.E.D."),
        "Some college"         = c("Associate’s degree", "Some college"),
        "College graduate"     = "College graduate",
        "Post graduate degree" = "Post graduate degree")
      ), "High school or less", "Some college", "College graduate", "Post graduate degree") %>%
      mutate(educ2 = forcats::fct_relevel(forcats::fct_collapse(
        educ,
        "HS or less/some college" = c("Did not complete high school", "High school or G.E.D.",
                                      "Associate’s degree", "Some college"),
        "College/grad degree"     = c("College graduate", "Post graduate degree"))
      ), "HS or less/some college", "College/grad degree") %>%
      dplyr::mutate(educ = magrittr::set_attr(educ, "label", "Education"),
                    educ2 = magrittr::set_attr(educ2, "label", "Education (college/non-college)"),
                    educ4 = magrittr::set_attr(educ4, "label", "Education (4-point)"))
    if (verbose) code_success_msg("educ")
    if (verbose) code_success_msg("educ2")
    if (verbose) code_success_msg("educ4")
  } else {
    if (verbose) code_fail_msg("educ")
    if (verbose) code_fail_msg("educ2")
    if (verbose) code_fail_msg("educ4")
  }
  
  if (append)
    return(data)
  else
    return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
}

#' @inherit code_std_demo_vars
#' @export
#' @return Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{income levels}: \itemize{
#'  \item \code{income}: original income scale 
#'  \item \code{income3}: 3-point income scale (i.e. <$50k, $50-100k, >$100k)
#' }
code_income_vars <- function(data, append = TRUE, verbose = TRUE) {
  data$income <- NULL
  data$income3 <- NULL
  inc_rgx <- "(My total family income last year was|total household income)"
  inc_idx <- find_qvar_col_idx(data, inc_rgx)
  inc <- colnames(data)[inc_idx][1]  
  if (is_valid(inc)) {  
    data <- data %>%
      dplyr::mutate(income = .[[inc]]) %>%
      dplyr::mutate(income3 = forcats::fct_collapse(income,
                                                    "Below $50,000" = c("Under $15,000", "Between $15,000 and $29,999", "Between $30,000 and $49,999"),
                                                    "$50,000 - $99,999" = c("Between $50,000 and $74,999", "Between $75,000 and $99,999"),
                                                    "$100,000 and above" = c("Between $100,000 and $150,000", "Over $150,000"))) %>%
      dplyr::mutate(income = magrittr::set_attr(income, "label", "Household income"),
                    income3 = magrittr::set_attr(income3, "label", "Household income (3-point)"))
    if (verbose) code_success_msg("income")
    if (verbose) code_success_msg("income3")
  } else {
    if (verbose) code_fail_msg("income")    
    if (verbose) code_fail_msg("income3")
  }
  if (append)
    return(data)
  else
    return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
}

#' @inherit code_std_demo_vars
#' @export
#' @return Survey data frame with new (or updated) columns corresponding to (if discoverable) respondents' \bold{geographic location (currently only in the U.S.)}: \itemize{
#'  \item \code{zip}: 6-digit ZIP code
#'  \item \code{urban4}: 4-point urban/rural classification for ZIP code based on the USDA's 2010 RUCA (rural-urban commuting area) coding (Metropolitan, Micropolitan, Small town, Rural)
#'  \item \code{zip}: 6-digit ZIP code
#'  \item \code{state_name}: state name
#'  \item \code{state_abb}: state abbreviation
#'  \item \code{region}: U.S. Census region
#'  \item \code{division}: U.S. Census division
#' }
code_geo_vars <- function(data, append = TRUE, verbose = TRUE) {
  data$state_name <- NULL
  data$state_abb <- NULL
  
  zi_rgx <- "In what ZIP code is your home located"
  zi_idx <- find_qvar_col_idx(data, zi_rgx)
  zi <- colnames(data)[zi_idx][1]
  
  st_rgx <- "What state( or U.S. territory)? do you (reside|live)"
  st_idx <- find_qvar_col_idx(data, st_rgx)
  st <- colnames(data)[st_idx][1]
  
  if (is_valid(zi)) {
    zips_fpath <- system.file("extdata", "zips_2020.csv", package = "svmkR")
    zips_xw <- suppressMessages(readr::read_csv(zips_fpath)) %>%
      dplyr::select(zip=ZIP, state_fips=STATE, state_abb=ST, urban4 = RUCA1) %>%
      dplyr::filter(as.integer(state_fips) %in% 1:56) %>%
      dplyr::mutate(urban4 = dplyr::case_when(
        as.numeric(urban4) %in% 1:3 ~ "Urban (metropolitan)",
        as.numeric(urban4) %in% 4:6 ~ "Urban (micropolitan)",
        as.numeric(urban4) %in% 7:9 ~ "Small town",
        as.numeric(urban4) %in% 10 ~ "Rural",
        TRUE ~ NA_character_
      )) %>%      
      dplyr::mutate(state_name = state.name[match(state_abb, state.abb)])
    
    data <- data %>%
      dplyr::mutate(zip = as.character(dplyr::case_when(
        !is.na(.[[zi]]) ~ sprintf("%05d", as.numeric(.[[zi]])),
        TRUE ~ NA
      ))) %>%
      dplyr::left_join(zips_xw %>% 
                         dplyr::select(-state_fips), by = "zip") %>%
      dplyr::mutate(state_abb = factor(state_abb, levels = state.abb),
                    state_name = factor(state_name, levels = state.name),
                    urban4 = factor(urban4, levels = c("Urban (metropolitan)", "Urban (micropolitan)", "Small town", "Rural"))) %>%
      dplyr::mutate(zip = magrittr::set_attr(zip, "label", "ZIP code (5-digit)"),
                    state_abb = magrittr::set_attr(state_abb, "label", "State (abbrv.)"),
                    state_name = magrittr::set_attr(state_name, "label", "State"),
                    urban4 = magrittr::set_attr(urban4, "label", "Urban/rural commuting zone (4-point)"))
    if (verbose) code_success_msg("zip")
    if (verbose) code_success_msg("urban4")
    if (verbose) code_success_msg("state_abb")
    if (verbose) code_success_msg("state_name")
  } else {
    if (verbose) code_fail_msg("zip")
    if (verbose) code_fail_msg("urban4")    
  }
  if (is_valid(st)) {
    data <- data %>%
      dplyr::mutate(state_reported = .[[st]])
    if ("state_name" %in% colnames(data)) {
      data <- data %>% 
        dplyr::mutate(state_name = dplyr::coalesce(state_name, state_reported)) %>%
        dplyr::mutate(state_abb = state.abb[match(state_name, state.name)]) %>%
        dplyr::mutate(state_abb = factor(state_abb, levels = state.abb),
                      state_name = factor(state_name, levels = state.name))
    } else {
      data <- data %>%
        dplyr::rename(state_name = state_reported) %>%
        dplyr::mutate(state_abb = state.abb[match(state_name, state.name)]) %>%
        dplyr::mutate(state_abb = factor(state_abb, levels = state.abb),
                      state_name = factor(state_name, levels = state.name))
    }
    data <- data %>%
      dplyr::mutate(state_abb = magrittr::set_attr(state_abb, "label", "State (abbrv.)"),
                    state_name = magrittr::set_attr(state_name, "label", "State"))
    if (verbose) code_success_msg("state_abb")
    if (verbose) code_success_msg("state_name")    
  } 
  if (!is_valid(zi) & !is_valid(st)) {
    if (verbose) code_fail_msg("zip")
    if (verbose) code_fail_msg("urban4")
    if (verbose) code_fail_msg("state_abb")
    if (verbose) code_fail_msg("state_name")
    if (verbose) code_fail_msg("region")
    if (verbose) code_fail_msg("division")    
    if (append)
      return(data)
    else
      return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
  } else {
    data <- data %>%
      dplyr::mutate(region = dplyr::case_when(
        state_abb %in% c("NH","VT","ME","MA","RI","CT","NY","NJ","PA") ~ "Northeast",
        state_abb %in% c("WI","IL","MI","IN","OH","ND","SD","NE","KS","MN","IA","MO") ~ "Midwest",
        state_abb %in% c("DE","MD","DC","WV","VA","NC","SC","GA","FL","KY","TN","MS","AL","OK","TX","AR","LA") ~ "South",
        state_abb %in% c("MT","ID","WY","NV","UT","CO","AZ","NM","WA","OR","CA","AK","HI") ~ "West"
      )) %>%
      mutate(division = dplyr::case_when(
        state_abb %in% c("NH","VT","ME","MA","RI","CT") ~ "New England",
        state_abb %in% c("NY","NJ","PA") ~ "Middle Atlantic",
        state_abb %in% c("WI","IL","MI","IN","OH") ~ "East North Central",
        state_abb %in% c("ND","SD","NE","KS","MN","IA","MO") ~ "West North Central",
        state_abb %in% c("DE","MD","DC","WV","VA","NC","SC","GA","FL") ~ "South Atlantic",
        state_abb %in% c("KY","TN","MS","AL") ~ "East South Central",
        state_abb %in% c("OK","TX","AR","LA") ~ "West South Central",
        state_abb %in% c("MT","ID","WY","NV","UT","CO","AZ","NM") ~ "Mountain",
        state_abb %in% c("WA","OR","CA","AK","HI") ~ "Pacific"
      )) %>%
      dplyr::mutate(region = magrittr::set_attr(region, "label", "Census Region"),
                    division = magrittr::set_attr(division, "label", "Census Division"))
    
    if (verbose) code_success_msg("region")
    if (verbose) code_success_msg("division")      
  }

  if (append)
    return(data)
  else
    return(data[std_demo_vars()[std_demo_vars() %in% colnames(data)]])
}








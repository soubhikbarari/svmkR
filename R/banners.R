#' Create banners from survey data.
#' 
#' Create an \code{expss} table object and a cleaned data frame from an input data frame.
#' 
#' Before running, must make sure that the input \code{data} is properly labelled/coded according to \code{expss} conventions (see example below). When doing so, make sure that values across multi-response variables (i.e. 'select all that apply' type questions) are given distinct levels, that is converted from binary to categorical (see example).
#'
#' @param data input survey data frame.
#' @param row.vars list of column names in \code{data} to put in the banner rows; any multi-response variables can be specified either as a regex string (e.g. \code{"var_*"}) or as a nested vector of all of the option columns (e.g. \code{c("var_1", "var_2",...)}).
#' @param col.vars list of column names in \code{data} to put in the banner columns; any multi-response variables can be specified either as a regex string (e.g. \code{"var_*"}) or as a nested vector of all of the option columns (e.g. \code{c("var_1", "var_2",...)}).
#' @param weight.var optional column name in \code{data} to use for weighting.
#' @param date.var optional column name in \code{data} to specify for weighting.
#' @param preview if true, print the \code{expss} banner table object in a pretty format.
#' @param total.row.position where to place the total or "Unweighted N"s (either "above" or "below", default is "above").
#' @param verbose output helpful debugging messages.
#' @return a list of banner outputs including the \code{expss} object and a cleaned data frame.
#' @importFrom expss recode
#' @importFrom maditr let
#' @importFrom expss apply_labels
#' @importFrom expss val_lab
#' @importFrom expss var_lab
#' @export
#' @examples
#' data(auto.evs)
#' if (FALSE) { ## not run
#' # Prepare data for banners
#' auto.evs.coded = auto.evs %>%
#'   let(
#'     ## code/label categorical variables
#'     age3 = recode(age,
#'                   "18-34" = 18 %thru% 34 ~ 1, 
#'                   "35-64" = 35 %thru% 64 ~ 2, 
#'                   "65+"   = 65 %thru% hi ~ 3),
#'     gender  = recode(gender,
#'                      "Male"   = 1 ~ 1,
#'                      "Female" = 2 ~ 2),
#'     income3 = recode(as.numeric(income),
#'                      "<$50k"     = 1 %thru% 3 ~ 1,
#'                      "$50k-100k" = 4 %thru% 5 ~ 2,
#'                      ">$100k"    = 6 %thru% 7 ~ 3,
#'                      "No answer" = 8 ~ 4),
#'     educ2 = recode(as.numeric(education),
#'                    "<HS-College" = 1 %thru% 4 ~ 1,
#'                    ">College"    = 5 %thru% 6 ~ 2),
#'     purchase = recode(purchase,
#'                       "Purchased new"                            = 1 ~ 1,
#'                       "Purchased used (non-certified pre-owned)" = 2 ~ 2,
#'                       "Purchased used (certified pre-owned)"     = 3 ~ 3,
#'                       "Leased new"                               = 4 ~ 4,
#'                       "Leased used"                              = 5 ~ 5,
#'                       "Other"                                    = 6 ~ 6),
#'     vehage = recode(vehage,
#'                     "Less than 1 year old"   = 1 ~ 1,
#'                     "1 - 2 years old"        = 2 ~ 2,
#'                     "3 - 5 years old"        = 3 ~ 3,
#'                     "6 - 10 years old"       = 4 ~ 4,
#'                     "More than 10 years old" = 5 ~ 5),
#'     vehcost = recode(vehcost,
#'                      "Under $5,000"      = 1 ~ 1,
#'                      "$5,000 - $9,999"   = 2 ~ 2,
#'                      "$10,000 - $19,999" = 3 ~ 3,
#'                      "$20,000 - $29,999" = 4 ~ 4,
#'                      "$30,000 - $39,999" = 5 ~ 5,
#'                      "$40,000 - $49,999" = 6 ~ 6,
#'                      "$50,000 - $59,999" = 7 ~ 7,
#'                      "$60,000 or more"   = 8 ~ 8),
#'     ## label multi-response variables
#'     ## note: make sure to re-code dichotomous "one-hot encoded" columns into distinct level
#'     ev_heard_1 = recode(ev_heard_1, "Audi e-tron" = 1 ~ 1),
#'     ev_heard_2 = recode(ev_heard_2, "BMW i3" = 1 ~ 2),
#'     ev_heard_3 = recode(ev_heard_3, "Cadillac LYRIQ" = 1 ~ 3),
#'     ev_heard_4 = recode(ev_heard_4, "Chevrolet Bolt" = 1 ~ 4),
#'     ev_heard_5 = recode(ev_heard_5, "Faraday Future" = 1 ~ 5),
#'     ev_heard_6 = recode(ev_heard_6, "Fisker" = 1 ~ 6),
#'     ev_heard_7 = recode(ev_heard_7, "Hyundai Electric" = 1 ~ 7),
#'     ev_heard_8 = recode(ev_heard_8, "Jaguar I-Pace" = 1 ~ 8),
#'     ev_heard_9 = recode(ev_heard_9, "Kia Niro EV" = 1 ~ 9),
#'     ev_heard_10 = recode(ev_heard_10, "Lucid Motors" = 1 ~ 10),
#'     
#'     mech_factors_1 = recode(mech_factors_1, "Price / affordability" = 1 ~ 1),
#'     mech_factors_2 = recode(mech_factors_2, "Convenience" = 1 ~ 2),
#'     mech_factors_3 = recode(mech_factors_3, "Trust / relationship" = 1 ~ 3),
#'     mech_factors_4 = recode(mech_factors_4, "Availability" = 1 ~ 4),
#'     
#'     sub_services_1 = recode(sub_services_1, "In-car internet" = 1 ~ 1),
#'     sub_services_2 = recode(sub_services_1, "Remote car start capability" = 1 ~ 2),
#'     sub_services_3 = recode(sub_services_1, "Live traffic / navigation" = 1 ~ 3),
#'     sub_services_4 = recode(sub_services_1, "In-vehicle safety and security" = 1 ~ 4),
#'     sub_services_5 = recode(sub_services_1, "Multiple driver profiles" = 1 ~ 5),
#'     sub_services_6 = recode(sub_services_1, "Cloud dash cam monitoring" = 1 ~ 6),
#'     sub_services_7 = recode(sub_services_1, "Car software updates" = 1 ~ 7),
#'     sub_services_8 = recode(sub_services_1, "AI- and self-driving capabilities" = 1 ~ 8),
#'     sub_services_9 = recode(sub_services_1, "Car-only media and streaming services" = 1 ~ 9),
#'     sub_services_10 = recode(sub_services_1, "Heated seats" = 1 ~ 10)
#'   ) %>%
#'   apply_labels(gender         = "Gender",
#'                age3           = "Age: 3 categories",
#'                educ2          = "Education",
#'                income3        = "Income",
#'                vehage         = "How old is your main vehicle?",
#'                vehcost        = "In total, how much did you pay to buy or lease your main vehicle?",
#'                purchase       = "How did you purchase or lease your main vehicle?",
#'                ## note: for multiresponse variables, only need to name one column
#'                ev_heard_1     = "Which of the following electric vehicles have you heard of?",
#'                mech_factors_1 = "Select the top 3 factors that are most important to you when selecting an auto repair provider:",
#'                sub_services_1 = "Which of the following services would you be willing to pay a recurring subscription for in a car?") 
#' # Make and save banners  
#' auto.evs.banners <- auto.evs.coded %>%
#'   make_banners(row.vars           = list("purchase","vehage","ev_heard*",c("mech_factors_1","mech_factors_2","mech_factors_3","mech_factors_4")), 
#'                col.vars           = list("age3","educ2",c("sub_services_1","sub_services_2","sub_services_3","sub_services_4")), 
#'                weight.var         = "weight_genpop", 
#'                date.var           = "response_date", 
#'                total.row.position = "below", 
#'                preview            = TRUE) %>%
#'   write_banners(file.path         = "auto_evs.xlsx",
#'                 file.overwrite    = TRUE,
#'                 title             = "Automative/Electric Vehicles Study",
#'                 logo              = "mntv",
#'                 drive.overwrite   = TRUE,
#'                 drive.folder.path = "https://drive.google.com/drive/u/1/folders/0B-OW6-tDrcdMTWw1MFFhdVNQLTg")
#' }
make_banners <- function(data, 
                         row.vars, 
                         col.vars, 
                         weight.var = NULL, 
                         date.var = NULL, 
                         preview = FALSE,
                         total.row.position = "above", 
                         verbose = FALSE) {
  data <- as.data.frame(data)
  banner.data.frame <- suppressWarnings(dplyr::bind_rows(pbapply::pblapply(1:length(row.vars), function(r){
    rr <- row.vars[[r]]
    if (length(rr) == 1 & any(grepl("\\*", rr))) {
      rr <- colnames(data)[grepl(rr, colnames(data))]
    }
    r.uniq <- unique(expss::val_lab(data[,rr]))
    r.type <- NULL
    if (length(rr) > 1) {
      if (all(r.uniq %in% c(0,1))) {
        r.type <- "mdset"
        lazy_tab_cells <- function(.) expss::tab_cells(., mdset_p(paste0("(",paste0(rr,collapse="|"),")")))
      } else {
        r.type <- "mrset"
        lazy_tab_cells <- function(.) expss::tab_cells(., mrset_p(paste0("(",paste0(rr,collapse="|"),")")))
      }
    } else {
      r.type <- "vars_list"
      lazy_tab_cells <- function(.) expss::tab_cells(., vars_list(rr))
    }
    banner.table.r <- NULL
    for (c in 1:length(col.vars)) {
      cc <- col.vars[[c]]
      if (length(cc) == 1 & any(grepl("\\*", cc))) {
        cc <- colnames(data)[grepl(cc, colnames(data))]
      }
      c.uniq <- unique(expss::val_lab(data[,cc]))
      c.type <- NULL
      if (length(cc) > 1) {
        if (all(c.uniq %in% c(0,1))) {
          c.type <- "mdset"
          lazy_tab_cols <- function(.) expss::tab_cols(., total(), mdset_p(paste0("(",paste0(cc,collapse="|"),")")))
        } else {
          c.type <- "mrset"
          lazy_tab_cols <- function(.) expss::tab_cols(., total(), mrset_p(paste0("(",paste0(cc,collapse="|"),")")))
        }
      } else {
        c.type <- "vars_list"
        lazy_tab_cols <- function(.) expss::tab_cols(., total(), vars_list(cc))
      }
      
      if (is.null(weight.var)) {
        weight.var <- "weight"
        data$weight <- 1
      }
      
      banner.table.r.c <- data %>%
        dplyr::rename_at(weight.var, ~"weight") %>%
        expss::tab_weight(weight = weight) %>% 
        expss::tab_total_row_position(total.row.position) %>%
        # Row variables go below
        lazy_tab_cells(.) %>%
        lazy_tab_cols(.) %>%
        expss::tab_stat_cpct() %>%
        expss::tab_pivot() %>%
        dplyr::as_tibble(.name_repair = c("minimal"))
      
      if (!any(grepl("\\|", banner.table.r.c$row_labels))) {
        banner.table.r.c$row_labels <- paste0(rr[1], "|", banner.table.r.c$row_labels)
      }
      if (!any(grepl("\\|", colnames(banner.table.r.c)))) {
        colnames(banner.table.r.c)[3:ncol(banner.table.r.c)] <- paste0(cc[1], "|", colnames(banner.table.r.c)[3:ncol(banner.table.r.c)])
      }
      
      if (is.null(banner.table.r)) {
        banner.table.r <- banner.table.r.c
      } else {
        banner.table.r <- banner.table.r %>%
          dplyr::left_join(banner.table.r.c, by = c("#Total", "row_labels"))
      }
    }
    banner.table.r
  })))
  banner.table <- expss::as.etable(banner.data.frame)
  if (preview) {
    print(banner.table)
  }
  
  message("+ Making banner data frame")
  banner.data.frame <- banner.data.frame %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(question = (stringr::str_split_fixed(row_labels, "\\|", n = 2))[,1]) %>%
    dplyr::mutate(response = (stringr::str_split_fixed(row_labels, "\\|", n = 2))[,2]) %>%
    dplyr::select(-row_labels) %>%
    dplyr::mutate(response = stringr::str_replace_all(response, "#Total cases", "Unweighted N")) %>%
    dplyr::rename(Total = `#Total`) %>%
    # mutate_if(is.numeric, round) %>%
    dplyr::mutate_if(!(names(.) %in% c('question', 'response')),
                     ~dplyr::case_when(
                       (stringr::str_detect(response, "Unweighted N")) == FALSE ~ . / 100,
                       TRUE ~ . )) %>%
    dplyr::select(question, response, tidyselect::everything()) %>%
    # filter(is.na(Total) == F) %>%
    dplyr::mutate_if(!(names(.) %in% c('question', 'response')),
                     ~dplyr::case_when(
                       is.na(.) == TRUE ~ 0,
                       TRUE ~ . )) %>%
    dplyr::select_if(~ !is.numeric(.) || sum(.) != 0)
  
  message("+ Cleaning up")
  row.questions <- banner.data.frame %>%
    dplyr::mutate(new_q = dplyr::case_when(
      dplyr::row_number() == 1 ~ FALSE,
      dplyr::row_number() >= 1 & question == dplyr::lag(question) ~ FALSE,
      dplyr::row_number() >= 1 & question != dplyr::lag(question) ~ TRUE
    )) %>%
    dplyr::mutate(q_num = 1 + cumsum(new_q == TRUE)) %>%
    dplyr::group_by(q_num, question) %>%
    dplyr::summarise(n_rows = dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_start = dplyr::case_when(
      dplyr::row_number() == 1 ~ 1,
      TRUE ~ 1 + cumsum(n_rows) - n_rows
    )) %>%
    dplyr::mutate(row_end = row_start + n_rows - 1)
  
  col.headers <- banner.data.frame %>%
    dplyr::mutate_all(., as.character) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::select(name) %>%
    dplyr::mutate(top = (stringr::str_split_fixed(name, "\\|", n = 3))[,1]) %>%
    dplyr::mutate(mid = (stringr::str_split_fixed(name, "\\|", n = 3))[,2]) %>%
    dplyr::mutate(bot = (stringr::str_split_fixed(name, "\\|", n = 3))[,3]) %>%
    dplyr::select(top, mid, bot)
  
  col.headers <- do.call(rbind, c(col.headers)) %>%
    dplyr::as_tibble(., .name_repair = "minimal")
  
  col.questions <- banner.data.frame %>%
    dplyr::mutate_all(., as.character) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::select(name) %>%
    dplyr::mutate(question = (stringr::str_split_fixed(name, "\\|", n = 2))[,1]) %>%
    dplyr::mutate(response = (stringr::str_split_fixed(name, "\\|", n = 2))[,2]) %>%
    dplyr::mutate(new_q = dplyr::case_when(
      dplyr::row_number() == 1 ~ FALSE,
      dplyr::row_number() >= 1 & question == dplyr::lag(question) ~ FALSE,
      dplyr::row_number() >= 1 & question != dplyr::lag(question) ~ TRUE
    )) %>%
    dplyr::mutate(q_num = 1 + cumsum(new_q == TRUE)) %>%
    dplyr::group_by(q_num, question) %>%
    dplyr::summarise(n_cols = dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(col_start = dplyr::case_when(
      dplyr::row_number() == 1 ~ 1,
      TRUE ~ 1 + cumsum(n_cols) - n_cols
    )) %>%
    dplyr::mutate(col_end = col_start + n_cols - 1)
  
  banner.settings <- list(
    total.row.position = total.row.position
  )
  
  message("DONE.")
  return(list(banner.data = data,
              banner.table = banner.table,
              banner.data.frame = banner.data.frame,
              banner.settings = banner.settings,
              row.questions = row.questions,
              weight.var = weight.var,
              date.var = date.var,
              col.headers = col.headers,
              col.questions = col.questions))
  
}

#' Format and save banner object to file / Google Drive.
#' 
#' Take the output from a call to \code{make_banners}, populate a spreadsheet with the banner data, format/stylize it in the SurveyMonkey research format, and save it locally and/or to a Google Drive folder. Currently allows for up to one level of nesting in columns.
#'
#' @param banners.output output list from a call to \code{make_banners()}.
#' @param title title of the tab/sheet.
#' @param file.path local file path to save banners as \code{.xlsx} file.
#' @param file.overwrite whether or not to overwrite the file specified at \code{file.path}; otherwise keep both and update the file path.
#' @param file.append whether or not to append the current tab to the file specified at \code{file.path} if it exists; if there is already a tab with the name specified, update the tab name.
#' @param drive.folder.path folder path in Google Drive to upload banners spreadsheet.
#' @param drive.overwrite if \code{drive.folder.path} is specified, whether or not to overwrite/update file in Google Drive if it already exists; otherwise upload the file with the same name but an updated timestamp.
#' @param tab.name name of the tab/sheet to populate.
#' @param logo whether or not to include either the SurveyMonkey logo ("svmk") or the Momentive logo ("mntv") in the banner headers.
#' @param include.moe whether or not to include the Margin of Error for the survey in the banner headers.
#' @param include.dates whether or not to include the date range for survey responses in the banner headers.
#' @param date.var if \code{include.dates} is set to \code{TRUE}, the date variable to look for in the data used to create \code{banners.output}.
#' @param weight.var the column in the data used to create \code{banners.output} corresponding to weights for the Margin of Error estimate.
#' @return a fully formatted spreadsheet in the form of an \code{openxlsx} object.
#' @export
#' @examples
#' data(auto.evs)
#' if (FALSE) { ## not run
#' # Prepare data for banners
#' auto.evs.coded = auto.evs %>%
#'   let(
#'     ## code/label categorical variables
#'     age3 = recode(age,
#'                   "18-34" = 18 %thru% 34 ~ 1, 
#'                   "35-64" = 35 %thru% 64 ~ 2, 
#'                   "65+"   = 65 %thru% hi ~ 3),
#'     gender  = recode(gender,
#'                      "Male"   = 1 ~ 1,
#'                      "Female" = 2 ~ 2),
#'     income3 = recode(as.numeric(income),
#'                      "<$50k"     = 1 %thru% 3 ~ 1,
#'                      "$50k-100k" = 4 %thru% 5 ~ 2,
#'                      ">$100k"    = 6 %thru% 7 ~ 3,
#'                      "No answer" = 8 ~ 4),
#'     educ2 = recode(as.numeric(education),
#'                    "<HS-College" = 1 %thru% 4 ~ 1,
#'                    ">College"    = 5 %thru% 6 ~ 2),
#'     purchase = recode(purchase,
#'                       "Purchased new"                            = 1 ~ 1,
#'                       "Purchased used (non-certified pre-owned)" = 2 ~ 2,
#'                       "Purchased used (certified pre-owned)"     = 3 ~ 3,
#'                       "Leased new"                               = 4 ~ 4,
#'                       "Leased used"                              = 5 ~ 5,
#'                       "Other"                                    = 6 ~ 6),
#'     vehage = recode(vehage,
#'                     "Less than 1 year old"   = 1 ~ 1,
#'                     "1 - 2 years old"        = 2 ~ 2,
#'                     "3 - 5 years old"        = 3 ~ 3,
#'                     "6 - 10 years old"       = 4 ~ 4,
#'                     "More than 10 years old" = 5 ~ 5),
#'     vehcost = recode(vehcost,
#'                      "Under $5,000"      = 1 ~ 1,
#'                      "$5,000 - $9,999"   = 2 ~ 2,
#'                      "$10,000 - $19,999" = 3 ~ 3,
#'                      "$20,000 - $29,999" = 4 ~ 4,
#'                      "$30,000 - $39,999" = 5 ~ 5,
#'                      "$40,000 - $49,999" = 6 ~ 6,
#'                      "$50,000 - $59,999" = 7 ~ 7,
#'                      "$60,000 or more"   = 8 ~ 8),
#'     ## label multi-response variables
#'     ## note: make sure to re-code dichotomous "one-hot encoded" columns into distinct level
#'     ev_heard_1 = recode(ev_heard_1, "Audi e-tron" = 1 ~ 1),
#'     ev_heard_2 = recode(ev_heard_2, "BMW i3" = 1 ~ 2),
#'     ev_heard_3 = recode(ev_heard_3, "Cadillac LYRIQ" = 1 ~ 3),
#'     ev_heard_4 = recode(ev_heard_4, "Chevrolet Bolt" = 1 ~ 4),
#'     ev_heard_5 = recode(ev_heard_5, "Faraday Future" = 1 ~ 5),
#'     ev_heard_6 = recode(ev_heard_6, "Fisker" = 1 ~ 6),
#'     ev_heard_7 = recode(ev_heard_7, "Hyundai Electric" = 1 ~ 7),
#'     ev_heard_8 = recode(ev_heard_8, "Jaguar I-Pace" = 1 ~ 8),
#'     ev_heard_9 = recode(ev_heard_9, "Kia Niro EV" = 1 ~ 9),
#'     ev_heard_10 = recode(ev_heard_10, "Lucid Motors" = 1 ~ 10),
#'     
#'     mech_factors_1 = recode(mech_factors_1, "Price / affordability" = 1 ~ 1),
#'     mech_factors_2 = recode(mech_factors_2, "Convenience" = 1 ~ 2),
#'     mech_factors_3 = recode(mech_factors_3, "Trust / relationship" = 1 ~ 3),
#'     mech_factors_4 = recode(mech_factors_4, "Availability" = 1 ~ 4),
#'     
#'     sub_services_1 = recode(sub_services_1, "In-car internet" = 1 ~ 1),
#'     sub_services_2 = recode(sub_services_1, "Remote car start capability" = 1 ~ 2),
#'     sub_services_3 = recode(sub_services_1, "Live traffic / navigation" = 1 ~ 3),
#'     sub_services_4 = recode(sub_services_1, "In-vehicle safety and security" = 1 ~ 4),
#'     sub_services_5 = recode(sub_services_1, "Multiple driver profiles" = 1 ~ 5),
#'     sub_services_6 = recode(sub_services_1, "Cloud dash cam monitoring" = 1 ~ 6),
#'     sub_services_7 = recode(sub_services_1, "Car software updates" = 1 ~ 7),
#'     sub_services_8 = recode(sub_services_1, "AI- and self-driving capabilities" = 1 ~ 8),
#'     sub_services_9 = recode(sub_services_1, "Car-only media and streaming services" = 1 ~ 9),
#'     sub_services_10 = recode(sub_services_1, "Heated seats" = 1 ~ 10)
#'   ) %>%
#'   apply_labels(gender         = "Gender",
#'                age3           = "Age: 3 categories",
#'                educ2          = "Education",
#'                income3        = "Income",
#'                vehage         = "How old is your main vehicle?",
#'                vehcost        = "In total, how much did you pay to buy or lease your main vehicle?",
#'                purchase       = "How did you purchase or lease your main vehicle?",
#'                ## note: for multiresponse variables, only need to name one column
#'                ev_heard_1     = "Which of the following electric vehicles have you heard of?",
#'                mech_factors_1 = "Select the top 3 factors that are most important to you when selecting an auto repair provider:",
#'                sub_services_1 = "Which of the following services would you be willing to pay a recurring subscription for in a car?") 
#' # Make and save banners  
#' auto.evs.banners <- auto.evs.coded %>%
#'   make_banners(row.vars           = list("purchase","vehage","ev_heard*",c("mech_factors_1","mech_factors_2","mech_factors_3","mech_factors_4")), 
#'                col.vars           = list("age3","educ2",c("sub_services_1","sub_services_2","sub_services_3","sub_services_4")), 
#'                weight.var         = "weight_genpop", 
#'                date.var           = "response_date", 
#'                total.row.position = "below", 
#'                preview            = TRUE) %>%
#'   write_banners(file.path         = "auto_evs.xlsx",
#'                 file.overwrite    = TRUE,
#'                 title             = "Automative/Electric Vehicles Study",
#'                 logo              = "mntv",
#'                 drive.overwrite   = TRUE,
#'                 drive.folder.path = "https://drive.google.com/drive/u/1/folders/0B-OW6-tDrcdMTWw1MFFhdVNQLTg")
#' }
write_banners <- function(banners.output, 
                          title = "Main",
                          file.path,
                          file.overwrite = TRUE,
                          file.append = FALSE,
                          drive.folder.path = NULL,
                          drive.overwrite = TRUE,
                          tab.name = "Main", 
                          logo = "mntv",
                          include.moe = TRUE,
                          include.dates = TRUE,
                          date.var = "StartDate",
                          weight.var = NULL,
                          ...) {
  
  if (!endsWith(file.path, ".xlsx"))
    stop("currently only supports saving to `.xlsx` files.")
  
  # Style
  SVMK_GREEN        = "#01B86E"
  MNTV_ORANGE       = "#F9AA6F"
  MNTV_ORANGE_LIGHT = "#FADCC5"
  MNTV_RED          = "#E67C73"  
  
  wrap_top         <- openxlsx::createStyle(valign="top", wrapText=TRUE)
  italic_right     <- openxlsx::createStyle(halign="right", textDecoration="italic")
  bold             <- openxlsx::createStyle(textDecoration="bold")
  bold_wrap_center <- openxlsx::createStyle(halign="center", textDecoration="bold", wrapText=TRUE)
  border_bottom    <- openxlsx::createStyle(border="bottom")
  border_right     <- openxlsx::createStyle(border="right")
  bold_red_bottom  <- openxlsx::createStyle(textDecoration="bold", fontColour="red", valign="bottom")
  percentage       <- openxlsx::createStyle(numFmt="0%", halign="center")
  integer          <- openxlsx::createStyle(numFmt="0", halign="center")  
  
  # Data
  banner.data <- banners.output$banner.data
  banner.table <- banners.output$banner.table
  banner.data.frame <- banners.output$banner.data.frame
  banner.settings <- banners.output$banner.settings
  row.questions <- banners.output$row.questions
  col.headers <- banners.output$col.headers
  col.questions <- banners.output$col.questions
  
  if (file.append & file.exists(file.path)) {
    wb <- openxlsx::loadWorkbook(file.path)
    #openxlsx::saveWorkbook(wb, file.path, overwrite = TRUE)
    existing.tabs <- openxlsx::getSheetNames(file.path)
    i <- 1
    tab.name.0 <- tab.name
    while (tab.name %in% existing.tabs) {
      tab.name <- paste0(tab.name.0, " (",i,")")
      i <- i + 1
    }
    message(sprintf("+ Attaching tab `%s` to existing workbook", tab.name))
    sh = openxlsx::addWorksheet(wb, tab.name, gridLines=FALSE)
  } else {
    message("+ Making new workbook")
    wb = openxlsx::createWorkbook()
    sh = openxlsx::addWorksheet(wb, tab.name, gridLines=FALSE)
  }    
  
  date_label <- ""
  if (include.dates) {
    if (!is.null(date.var) & date.var %in% colnames(banner.data)) {
      date.var <- date.var
      message("+ Calculating date range")
      date_label <- paste0(as.character(min(banner.data[[date.var]])), " to ", as.character(max(banner.data[[date.var]])))
    } else if (banners.output$date.var %in% colnames(banner.data)) {
      date.var <- banners.output$date.var
      message("+ Calculating date range")
      date_label <- paste0(as.character(min(banner.data[[date.var]])), " to ", as.character(max(banner.data[[date.var]])))
    }
  }
  moe_label <- ""
  if (include.moe) {
    message("+ Calculating margin of error")
    if (!("weight.var" %in% names(banners.output)) & !is.null(banners.output$weight.var)) {
      if (banners.output$weight.var %in% colnames(banner.data)) {
        banner.data$weight <- banner.data[[banners.output$weight.var]]
      } else if (!is.null(weight.var) & weight.var %in% colnames(banner.data)) {
        banner.data$weight <- banner.data[[weight.var]]
      } else {
        banner.data$weight <- 1
      }
    } else {
      banner.data$weight <- 1
    }
    moe <- simu_moe(weights = banner.data$weight)
    moe_label <- paste0("Margin of error estimate: ", as.character(plyr::round_any(moe*100, 0.5, ceiling)), "%")
  }
  
  writeData <- openxlsx::writeData
  
  # Headers
  if (logo == "svmk" | logo == TRUE)
    openxlsx::insertImage(wb, sheet = tab.name, file=system.file("extdata", "svmk.png", package = "svmkR"), startRow=1, startCol=1, width=3.5, height=0.5)
  if (logo == "mntv")
    openxlsx::insertImage(wb, sheet = tab.name, file=system.file("extdata", "mntv.png", package = "svmkR"), startRow=1, startCol=1, width=3.5, height=0.5)
  message(sprintf("+ Formatting tab `%s`", tab.name))

  openxlsx::writeData(wb, sheet = tab.name, title, startRow = 1, startCol = 4)
  openxlsx::writeData(wb, sheet = tab.name, date_label, startRow = 2, startCol = 4)
  openxlsx::writeData(wb, sheet = tab.name, moe_label, startRow = 3, startCol = 4)
  
  ## Allow for two layers of nesting
  if (all(col.headers[3,] == "")) {
    col.headers <- col.headers[1:2,]
    row_offset <- 6
    col_nested <- F
  } else {
    row_offset <- 7
    col_nested <- T
  }
  openxlsx::writeData(wb, sheet = tab.name, banner.data.frame, startRow = row_offset, startCol = 1)
  openxlsx::writeData(wb, sheet = tab.name, col.headers, startRow = 5, startCol = 1, colNames = FALSE)

  
  # Row Labels
  message("+ Merging row labels")
  for(i in 1:nrow(row.questions)) {
    start <- row_offset + (row.questions %>%
                    dplyr::filter(dplyr::row_number() == i) %>%
                    dplyr::select(row_start) %>%
                    dplyr::pull())
    end <- row_offset + (row.questions %>%
                  dplyr::filter(dplyr::row_number() == i) %>%
                  dplyr::select(row_end) %>%
                  dplyr::pull())
    openxlsx::mergeCells(wb, sheet = tab.name, cols = 1, rows = start:end)
  }
  
  # Column Labels
  message("+ Merging column labels")
  for(i in 1:nrow(col.questions)) {
    start <- 0 + (col.questions %>%
                    dplyr::filter(dplyr::row_number() == i) %>%
                    dplyr::select(col_start) %>%
                    dplyr::pull())
    end <- 0 + (col.questions %>%
                  dplyr::filter(dplyr::row_number() == i) %>%
                  dplyr::select(col_end) %>%
                  dplyr::pull())
    openxlsx::mergeCells(wb, sheet = tab.name, cols = start:end, rows = 5)
    if (col_nested) {
      for (j in col.questions$col_start[i]:col.questions$col_end[i]) {
        ### merge bottom label with middle label if no nested value is there
        if (col.headers[3,j] == "") {
          openxlsx::mergeCells(wb, sheet = tab.name, cols = j, rows = 6:7)
        }
      }
      
    }
  }
  # Style Header
  message("+ Styling header")
  openxlsx::setColWidths(wb, sheet = tab.name, col = 1:2, c(24, 32))
  openxlsx::setColWidths(wb, sheet = tab.name, col = 3:(max(col.questions$col_end)), 10)
  
  openxlsx::addStyle(wb, sheet = tab.name, rows = (row_offset + min(row.questions$row_start)):(row_offset + max(row.questions$row_end)), cols = 1,
                     style = wrap_top, stack = TRUE)
  if (col_nested) {
    openxlsx::addStyle(wb, sheet = tab.name, rows = (row_offset-2), cols = 3:(max(col.questions$col_end)),
                       style = bold_wrap_center, stack = TRUE)    
  }
  openxlsx::addStyle(wb, sheet = tab.name, rows = (row_offset-1), cols = 3:(max(col.questions$col_end)),
                     style = bold_wrap_center, stack = TRUE)
  openxlsx::addStyle(wb, sheet = tab.name, rows = row_offset, cols = 3:(max(col.questions$col_end)),
                     style = bold_wrap_center, stack = TRUE)
  openxlsx::addStyle(wb, sheet = tab.name, rows = 1:3, cols = 1,
                     style = bold_red_bottom, stack = TRUE)
  openxlsx::addStyle(wb, sheet = tab.name, rows = 1:3, cols = 4,
                     style = bold, stack = TRUE)
  
  # Style Totals
  if (banner.settings$total.row.position %in% c("below","above")) {
    message("+ Styling totals")
    for(i in 1:nrow(row.questions)) {
      start <- row_offset + (row.questions %>%
                      dplyr::filter(dplyr::row_number() == i) %>% {
                        if (banner.settings$total.row.position == "above")
                          dplyr::select(., row_start)
                        else if (banner.settings$total.row.position == "below")
                          dplyr::select(., row_end)
                      } %>%
                      dplyr::pull())
      ## colorize total labels
      n_style <- openxlsx::createStyle(halign="center",
                                       fontColour = "white",
                                       fgFill = "#262626",
                                       textDecoration="italic")
      openxlsx::addStyle(wb, 
                         sheet = tab.name, 
                         rows = start, 
                         cols = 2:(max(col.questions$col_end)), 
                         gridExpand = TRUE,
                         style = n_style,
                         stack = TRUE)
      ## colorize total integers
      openxlsx::conditionalFormatting(wb,
                                      sheet = tab.name,
                                      rows = start,
                                      cols = 3:(max(col.questions$col_end)),
                                      # style = c(MNTV_ORANGE_LIGHT, MNTV_ORANGE),
                                      style = c("#B8B8B8", "#000000"),
                                      # rule = c(0, 1),
                                      type = "colourScale")
    }
  }
  # Style Percentages
  message("+ Styling percentages")
  for(i in 1:nrow(row.questions)) {
    start <- (row.questions %>%
                dplyr::filter(dplyr::row_number() == i) %>%
                dplyr::select(row_start) %>%
                dplyr::pull())
    end <- (row.questions %>%
              dplyr::filter(dplyr::row_number() == i) %>%
              dplyr::select(row_end) %>%
              dplyr::pull())
    if (banner.settings$total.row.position == "above") {
      start <- start + (row_offset+1); end <- end + row_offset;
    } else if (banner.settings$total.row.position == "below") {
      start <- start + row_offset; end <- end + (row_offset-1);
    }
    
    ## colorize percentages
    openxlsx::conditionalFormatting(wb, sheet = tab.name,
                                    rows = start:end, 
                                    cols = 3:(max(col.questions$col_end)), 
                                    # style = c("white", SVMK_GREEN),
                                    # rule = c(0, 1),
                                    style = c("white", SVMK_GREEN, MNTV_RED),
                                    rule = c(0, .5, 1),
                                    type = "colourScale")
    ## force percentage types
    openxlsx::addStyle(wb, sheet = tab.name,
                       rows = start:end,
                       cols = 3:(max(col.questions$col_end)),
                       gridExpand = TRUE,
                       style = percentage,
                       stack = TRUE)
    
  }
  # Clean up
  message("+ Cleaning up")
  openxlsx::deleteData(wb, sheet = tab.name, rows = 5, cols = 1)
  openxlsx::deleteData(wb, sheet = tab.name, rows = 6, cols = 1)
  openxlsx::deleteData(wb, sheet = tab.name, rows = 5, cols = 2)
  openxlsx::deleteData(wb, sheet = tab.name, rows = 6, cols = 2)
  openxlsx::deleteData(wb, sheet = tab.name, rows = 5, cols = 3)
  openxlsx::addStyle(wb, sheet = tab.name, rows = 4:(row_offset + max(row.questions$row_end)), cols = 1:(max(col.questions$col_end)),
                     style = border_bottom, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet = tab.name, rows = 5:(row_offset + max(row.questions$row_end)), cols = 1:(max(col.questions$col_end)),
                     style = border_right, gridExpand=TRUE, stack = TRUE)
  openxlsx::freezePane(wb, sheet = tab.name, firstActiveRow = row_offset+1, firstActiveCol = 4)
  
  # Save
  if (!is.null(file.path)) {
    
    if (file.overwrite | !file.exists(file.path)) {
      file.path.0 <- file.path
      openxlsx::saveWorkbook(wb, file.path, overwrite = TRUE)
      message(sprintf("SAVED excel file to `%s`.", file.path))
    } else {
      i <- 1
      file.path.0 <- file.path
      while (file.exists(file.path)) {
        file.path <- gsub("\\.xls", sprintf(" (%i).xls", i), file.path.0)
        i <- i + 1
      }
      openxlsx::saveWorkbook(wb, file.path, overwrite = TRUE)
      message(sprintf("SAVED excel file to `%s`.", file.path))
    }
    
    if (!is.null(drive.folder.path)) {
      file.name <- basename(file.path)
      sheet.name <- gsub("\\.[a-zA-Z0-9]*$","", file.name)
      warning("Use `googledrive::drive_auth()` if Google Drive authentication fails.")
      
      drive.folder.files <- googledrive::drive_ls(drive.folder.path)
      
      if (sheet.name %in% drive.folder.files$name & drive.overwrite) {
        googledrive::drive_update(file = first(drive.folder.files$id[drive.folder.files$name == sheet.name]),
                                  media = file.path.0)
      } else {
        googledrive::drive_upload(media = file.path.0, 
                                  name = sheet.name,
                                  path = googledrive::as_dribble(drive.folder.path), 
                                  overwrite = TRUE,
                                  type = "spreadsheet")
      }
    }
  }
  
  return(wb)
}

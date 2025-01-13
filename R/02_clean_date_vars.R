#' clean_date_vars
#' @description Function that cleans the date variable formatting and creates two final variables, including a formatted date variable (dt)
#' and the imputation status (imp_status) of the date variable
#'
#' @param dt_var supplied date variable(s)
#'
#' @return a list with a formatted date variable (dt) and the imputation status (imp_status) of the date variable. Also returns a format
#' object (date_impf) to the current working environment
#' @export
#'
#' @importFrom stringr str_split_i
#' @importFrom dplyr case_when %>%
#' @importFrom labelled labelled
clean_date_vars <- function(dt_var){
  # Date format
  date_impf <- c("Full Date"                              = 0,
                 "Day replaced with 01"                   = 1,
                 "Month replaced with Jan"                = 2,
                 "Day and Month replaced with 01 and Jan" = 3)

  # Thank you to Winnie for the function!

  # date format: UNK-UNK-UNK

  # Extract date parts
  dt_y = str_split_i(dt_var, "-", 1)
  dt_m = str_split_i(dt_var, "-", 2)
  dt_d = str_split_i(dt_var, "-", 3)

  # Determine imputation status of date
  dt_imp_y = case_when(is.na(dt_y)                         ~ NA,
                       !is.na(dt_var) & grepl("U", dt_y)   ~  1,
                       TRUE                                ~ 0)
  dt_imp_m = case_when(is.na(dt_m)                         ~ NA,
                       !is.na(dt_var) & grepl("U", dt_m)   ~  1,
                       TRUE                                ~ 0)
  dt_imp_d = case_when(is.na(dt_d)                         ~ NA,
                       !is.na(dt_var) & grepl("U", dt_d)   ~  1,
                       TRUE                                ~ 0)

  # imputation status
  dt_imp = case_when(
    dt_imp_y==1                                 ~ NA,
    # full date
    dt_imp_y==0 & dt_imp_m == 0 & dt_imp_d == 0 ~ 0,
    # only day is missing - impute date as 01
    dt_imp_y==0 & dt_imp_m == 0 & dt_imp_d == 1 ~ 1,
    # only month is missing - leave missing
    dt_imp_y==0 & dt_imp_m == 1 & dt_imp_d == 0 ~ 2,
    # Both month and day are missing, impute month as Jan and day as 01
    dt_imp_y==0 & dt_imp_m == 1 & dt_imp_d == 1 ~ 3,
    TRUE                                        ~ NA)

  dt_imp = labelled::labelled(dt_imp, labels = date_impf)

  # create imputed date
  dt_m = ifelse(dt_imp_m == 1, "01", dt_m)
  dt_d = ifelse(dt_imp_d == 1, "01", dt_d)
  new_dt_var = ifelse(grepl("U", dt_y) | is.na(dt_var), yes=NA, no=paste(dt_y, dt_m, dt_d, sep = "-"))

  return(list(dt = as.Date(new_dt_var), imp_status = dt_imp))
  assign("date_impf", date_impf, envir = parent.frame())
}

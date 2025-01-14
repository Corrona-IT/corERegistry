#' clean_code_vars
#' @description Function that cleans the "_code" variables by either removing the corresponding variable
#' without the "_code" suffix (drop_dec_vars=TRUE option) or renaming the corresponding variable without
#' the "_code" suffix by appending "_dec" and subsequently removes the "_code" suffix from the desired variables
#'
#' @param . current data frame
#' @param drop_dec_vars Logical (TRUE or FALSE) value for whether to drop the description variables named without
#' the "_code" suffix (TRUE) or rename them and keep them by appending "_dec" (FALSE). The default is TRUE.
#'
#' @return cleaned data frame with desired _code variables without the "_code" suffix
#' @export
#'
#' @importFrom dplyr select rename_with %>%
#' @importFrom stringr str_replace
#' @importFrom tidyselect ends_with all_of any_of
clean_code_vars <- function(., drop_dec_vars=TRUE){
  curr_data <- .

  code_vars <- gsub("_code", "", colnames(.)[which(grepl("_code", colnames(.)))])

  if (drop_dec_vars==TRUE){
    cleaned_data <- curr_data %>%
      select(-any_of(code_vars)) %>%
      rename_with(~str_replace(., "_code", ""), .cols = ends_with('_code'))
  } else {
    cleaned_data <- curr_data %>%
      rename_with(~str_c(., "_dec"), .cols = any_of(code_vars)) %>%
      rename_with(~str_replace(., "_code", ""), .cols = ends_with('_code'))
  }


  return(cleaned_data)

}

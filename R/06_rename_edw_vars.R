#' rename_edw_vars
#' @description Function that renames all designated EDW DWH variables (old_varname) to the desired final analyctical variable name (new_varname)
#'
#'
#' @param . current data frame
#' @param rename_vars_df variable name mapping data frame (expected columns are the old_varname containing the name of the EDW DWH variable name to be renamed and
#' the new_varname containing the name of the final desired variable name)
#'
#' @return a data frame with renamed EDW DWH variables renamed to their desired final analytical name
#' @export
#'
#' @importFrom dplyr rename %>%
#' @importFrom glue glue
#' @importFrom rlang :=
rename_edw_vars <- function(., rename_vars_df){

  # Raw data is the current data frame being piped
  raw_data = .

  for (i in 1:length(rename_vars_df$old_varname)){
    edw_var <- rename_vars_df$old_varname[i]
    new_var <- rename_vars_df$new_varname[i]

    if (edw_var %in% colnames(raw_data)){

      if (new_var %in% colnames(raw_data)){
      } else{
        print(glue("Renaming {edw_var} to {new_var}"))

        raw_data <- raw_data %>%
          rename({{new_var}} := {edw_var})
      }


    } else {
      print(glue("{edw_var} does not exist in data"))
    }

  }

  return(raw_data)
}

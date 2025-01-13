#' assign_var_labels
#' @description Function to assign variable labels to the new variable names (designated as new_varname) and the desired variable
#' label (designated by new_varlabel)
#'
#'
#' @param . current data frame
#' @param var_labels_df variable labels data frame (expected columns are the new_varname containing the name of the variable and new_varlabel
#' containing the desired label of the variable)
#'
#' @return data frame with labelled variables
#' @export
#'
#' @importFrom labelled set_variable_labels
#' @importFrom dplyr %>%
#' @importFrom glue glue
#' @importFrom rlang :=
assign_var_labels <- function(., var_labels_df){
  # Raw data is the current data frame being piped
  raw_data = .

  for (i in 1:length(var_labels_df$new_varname)){
    new_var <- var_labels_df$new_varname[i]
    new_varlabel <- var_labels_df$new_varlabel[i]

    if (new_var %in% colnames(raw_data)){

      print(glue("Labelling {new_var} as {new_varlabel}"))

      raw_data <- raw_data %>%
        set_variable_labels({{new_var}} := {new_varlabel})

    } else {
      print(glue("{new_var} does not exist in data"))
    }

  }

  return(raw_data)
}

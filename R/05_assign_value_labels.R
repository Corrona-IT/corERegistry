#' assign_value_labels
#' @description Function to assign value labels to the new variable names (designated as new_varname) and the desired format label name
#' (designated by new_valuelabel)
#'
#' @param . current data frame
#' @param value_labels_df value labels data frame (expected columns are the new_varname containing the name of the variable and new_valuelabel
#' containing the desired desired format label name of the format object expected to be stored in the working environment)
#'
#' @return a data frame with targeted categorical variables labelled with their assigned value format
#' @export
#'
#' @importFrom dplyr mutate %>%
#' @importFrom labelled labelled
#' @importFrom glue glue
#' @importFrom rlang :=
assign_value_labels <- function(., value_labels_df){
  # Raw data is the current data frame being piped
  raw_data = .

  for (i in 1:length(value_labels_df$new_varname)){
    new_var <- value_labels_df$new_varname[i]
    new_valuelabel <- value_labels_df$new_valuelabel[i]

    if (new_var %in% colnames(raw_data)){

      print(glue("Labelling {new_var} values as {new_valuelabel}"))

      raw_data <- raw_data %>%
        mutate({{new_var}} := labelled::labelled(as.numeric(raw_data[[new_var]]), labels = get(new_valuelabel)))


    } else {
      print(glue("{new_var} does not exist in data"))
    }

  }

  return(raw_data)
}

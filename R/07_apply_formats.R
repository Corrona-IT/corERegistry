#' apply_formats
#' @description Function that takes in the current data frame, a specs data frame (with at least the column_name as the DWH current variable name,
#' the format label name and the view_name) and applies all of the label formatting to each variable
#'
#'
#' @param . current data frame
#' @param specs_data a data frame with expected variables including (column_name as the targerted variable name, format_name as the desired format
#' label name and view_name as the current view being cleaned and formatted)
#' @param view_name the name of the view being cleaned (i.e. bv_exits)
#'
#' @return a data frame with DWH lookup variables formatted from the meta data
#' @export
#'
#' @importFrom dplyr filter %>%
#' @importFrom labelled labelled
#' @importFrom glue glue
#' @importFrom rlang :=
apply_formats <- function(., specs_data, view_name){
  # Takes the current data and uses the bv_mapping data to apply labels to all of the variables needed to be labelled

  # Raw data is the current data frame being piped
  raw_data = .

  curr_specs <- specs_data %>%
    filter(view_name == view_name)

  for (i in 1:length(curr_specs$column_name)){

    if (curr_specs$column_name[i] %in% colnames(raw_data)){
      if (! is.na(curr_specs$format_name[i]) & ! all(is.na(raw_data[, curr_specs$column_name[i]]))){
        curr_var <- curr_specs$column_name[i]

        print(glue("Formatting {curr_var} with {curr_specs$format_name[i]}"))

        raw_data[[curr_var]] <- labelled::labelled(as.numeric(raw_data[[curr_var]]), labels = get(curr_specs$format_name[i]))
        str(raw_data[[curr_var]])

      } else {
        curr_var <- curr_specs$column_name[i]
        print(glue("No Need to format {curr_var}"))
      }
    }

  }

  return(raw_data)
}

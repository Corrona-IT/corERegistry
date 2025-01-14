#' create_dwh_formats
#' @description Function that creates the format objects for labeling from the supplied data frame with expected variables
#' including (dwh_lookup_acronym or column_name, dwh_lookup_code, dwh_lookup_value)
#'
#'
#' @param dwh_lookup_dat supplied data frame with expected variables including (dwh_lookup_acronym or column_name, dwh_lookup_code, dwh_lookup_value).
#' dwh_lookup_acronym should be the name of the format that ultimately gets new object stored to the working environment with "_f appended
#' to the name. dwh_lookup_code is the numeric value and dwh_lookup_value is the value text to be used to label the corresponding numeric value.
#' @param source_lookup lookup source variable (expecting either "dwh_lookup_acronym" or "column_name")
#'
#' @return a new generated format object for each unique dwh_lookup_acronym with "_f" appended to the name (i.e. yes_no_only becomes yes_no_only_f
#' stored in the R working environment)
#' @export
#'
#' @importFrom stringr str_remove
#' @importFrom dplyr filter mutate case_when distinct rename %>%
#' @importFrom textclean replace_white
#' @importFrom tidyselect any_of
#' @importFrom rlang :=
create_dwh_formats <- function(dwh_lookup_dat, source_lookup){
  lookup <- c(dwh_variable_name = "std_variable_name",
              dwh_lookup_acronym = "std_lookup_acronym",
              dwh_lookup_value = "std_lookup_value",
              dwh_lookup_code = "std_lookup_code")

  lookup_data <- dwh_lookup_dat %>%
    rename(any_of(lookup)) %>%
    filter(! is.na(dwh_lookup_code)) %>%
    mutate(lookup_name = get(source_lookup)) %>%
    distinct(lookup_name, dwh_lookup_code, dwh_lookup_value) %>%
    # clean the formatting of of the code and value of the lookups
    mutate(dwh_lookup_code = as.numeric(dwh_lookup_code),
           dwh_lookup_value = textclean::replace_white(dwh_lookup_value),
           dwh_lookup_value = gsub("'", " ", dwh_lookup_value)) %>%
    mutate(dwh_lookup_value = gsub('\\"','', dwh_lookup_value)) %>%
    mutate(dwh_lookup_value = gsub('\\\\','', dwh_lookup_value))

  unique_lookups <- unique(lookup_data$lookup_name)

  for (i in 1:length(unique_lookups)){
    print(i)
    print(unique_lookups[i])

    curr_lookup <- unique_lookups[i]
    curr_fomat_name <- paste(unique_lookups[i], "f", sep = "_")
    curr_lookup_data <- lookup_data %>%
      filter(lookup_name == curr_lookup) %>%
      mutate(dwh_lookup_value = str_remove(dwh_lookup_value, '/$'))

    curr_format <- paste("'", curr_lookup_data$dwh_lookup_value, "'", sep = "") %>%
      paste(., "=", curr_lookup_data$dwh_lookup_code, collapse=",\n") %>%
      paste("c(", ., ")", sep="") %>%
      parse(text=.) %>%
      eval()

    assign(curr_fomat_name, curr_format, envir = parent.frame())
    rm(curr_format)
  }
}

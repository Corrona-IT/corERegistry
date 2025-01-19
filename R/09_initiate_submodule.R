#' initiate_submodule
#'
#' @param submodule_to_add name of submodule to add to parent repository
#' @param submodule_git_path github path to submodule (i.e. https://github.com/Corrona-IT/EDW_UAT_Integration_Setup)
#'
#' @return an added submodule in the working directory repository
#' @export
#'
#' @importFrom glue glue
initiate_submodule <- function(submodule_to_add, submodule_git_path){
  system(glue("git submodule add {submodule_git_path} {submodule_to_add}"))

  .submod=glue::glue("\"{submodule_to_add} submodule\"")
  .commit<- glue::glue("git commit -m ", .submod)
  system(.commit)
}

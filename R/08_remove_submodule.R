#' remove_submodule
#' @description Function to remove a git submodule from a repository
#'
#'
#' @param submodule_to_rm name of the sumodule you want to remove
#'
#' @return does not return anything
#' @export
#' @importFrom glue glue
remove_submodule <- function(submodule_to_rm){
  print(glue("Removing {submodule_to_rm} submodule"))
  eval_statement1 <- glue("git rm ./{submodule_to_rm}")
  system(eval_statement1)

  eval_statement2 <- glue("rm -rf .git/modules/{submodule_to_rm}")
  system(eval_statement2)

  eval_statement3 <- glue("git commit -m \"Removed submodule {submodule_to_rm}\"")
  system(eval_statement3)

  eval_statement4 <- glue("rm -rf {submodule_to_rm}")
  system(eval_statement4)
  print(glue("Finished removing {submodule_to_rm} submodule from parent repository"))
}

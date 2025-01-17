% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_clean_code_vars.R
\name{clean_code_vars}
\alias{clean_code_vars}
\title{clean_code_vars}
\usage{
clean_code_vars(., drop_dec_vars = TRUE)
}
\arguments{
\item{.}{current data frame}

\item{drop_dec_vars}{Logical (TRUE or FALSE) value for whether to drop the description variables named without
the "_code" suffix (TRUE) or rename them and keep them by appending "_dec" (FALSE). The default is TRUE.}
}
\value{
cleaned data frame with desired _code variables without the "_code" suffix
}
\description{
Function that cleans the "_code" variables by either removing the corresponding variable
without the "_code" suffix (drop_dec_vars=TRUE option) or renaming the corresponding variable without
the "_code" suffix by appending "_dec" and subsequently removes the "_code" suffix from the desired variables
}

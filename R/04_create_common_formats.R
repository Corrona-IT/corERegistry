#' create_common_formats
#' @description Function that creates commonly used formats for race (4 categories, 7 categories) 3 smoker categories, female/male common variable
#' as well as some other common formats
#'
#'
#' @return various formats to be used for labeling (i.e. race7_f, race4_f, smoker3_f, female_male_f, etc.)
#' @export
#'
create_common_formats <- function(){
  race7_f <- c("White" = 1,
               "Black" = 2,
               "Asian" = 3,
               "Native American" = 4,
               "Native Hawaiian or Other Pacific Islander" = 5,
               "Multiracial" = 6,
               "Other/Not Multiracial" = 7)
  print("Creating race7_f format")
  assign("race7_f", race7_f, envir = parent.frame())

  race4_f <- c("White" = 1,
               "Black" = 2,
               "Asian" = 3,
               "Other" = 4)
  print("Creating race4_f format")
  assign("race4_f", race4_f, envir = parent.frame())

  race_hisp_f <- c("White, Non-Hispanic" = 1,
                   "Black, Non-Hispanic" = 2,
                   "Asian, Non-Hispanic" = 3,
                   "Other/Multiracial, Non-Hispanic" = 4,
                   "Hispanic" = 5)
  print("Creating race_hisp_f format")
  assign("race_hisp_f", race_hisp_f, envir = parent.frame())

  smoker3_f <- c("Never" = 0,
                 "Former" = 1,
                 "Current" = 2)
  print("Creating smoker3_f format")
  assign("smoker3_f", smoker3_f, envir = parent.frame())

  female_male_f <- c("Male" = 0,
                     "Female" = 1)
  print("Creating female_male_f format")
  assign("female_male_f", female_male_f, envir = parent.frame())

  bmi3_f <- c("Normal/underweight (<25)" = 1,
              "Overweight (25 to <30)" = 2,
              "Obese >=30" = 3)
  print("Creating bmi3_f format")
  assign("bmi3_f", bmi3_f, envir = parent.frame())

  bmi4_f <- c("Normal/underweight (<18.5)" = 1,
              "Normal (18.5 to <25)" = 2,
              "Overweight (25 to <30)" = 3,
              "Obese >=30" = 4)
  print("Creating bmi4_f format")
  assign("bmi4_f", bmi4_f, envir = parent.frame())
}

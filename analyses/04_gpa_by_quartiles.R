if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")

# Load and install all packages at once
pacman::p_load(
  tidymodels, tidyverse, rio, naniar, labelled, sjlabelled, haven, fixest,
  vtable, ggfixest, modelsummary, gtsummary, simputation, kableExtra, readxl,
  panelsummary, cowplot, parameters
)


## This code produces table 3.3 in the main paper ------------------------------
## Title : Differential effect of COVID-19 across GPA quartiles

### Loading the question level data ------------------------------------------

question_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/question_level.rds"
question_level <- read_rds(question_url)

### Loading the exam level data ------------------------------------------
## These scores are out of possible 40 points.

exam_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/exam_level.rds"
exam_level <- read_rds(exam_url) 

################################################################################################################
################################################################################################################


### Models with GPA quartiles as additional controls

exam_gpa_4 <- exam_level |>
  feols(fml = score ~ post + q1 + q2 + q3 + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

exam_gpa_int_4 <- exam_level |>
  feols(fml = score ~ post + q1 + q2 + q3 + post * q1 + post * q2 + post * q3 + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")


ques_gpa_4 <- question_level |>
  feols(fml = correct ~ post + q1 + q2 + q3 + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_gpa_int_4 <- question_level |>
  feols(fml = correct ~ post + q1 + q2 + q3 + post * q1 + post * q2 + post * q3 + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")



#### Creating a result table -----------------------------------------------------------



rowlabs_gpa_quartiles <- c(
  "post" = "postcovid",
  "q1" = "GPA (first quartile)",
  "q2" = "GPA (second quartile)",
  "q3" = "GPA (third quartile)",
  "post:q1" = "post x GPA (first quartile)",
  "post:q2" = "post x GPA (second quartile)",
  "post:q3" = "post x GPA (third quartile)"
)
# "time::5:lowgpa" = "Fall 2021 x Low GPA",
# "time::6:lowgpa" = "Spring 2022 x Low GPA",
# "online"  = "Online",
# "female1" = "Female",
# "r_hispa1" = "Hispanic",
# "r_black1" = "Black",
# "r_asian1" = "Asian",
# "r_other1" = "Other Race",
# "Fall1"   = "Fall",
# "parttime1" = "Part-time",
# "underclassperson1" = "At most Sophomore")
# "gpamiss1" = "GPAmiss1",
# "gpamiss2" = "GPAmiss2")


results_gpa <- panelsummary_raw(list(exam_gpa_4, exam_gpa_int_4, ques_gpa_4, ques_gpa_int_4),
                                coef_map = rowlabs_gpa_quartiles,
                                gof_map = c("nobs", "r.squared"),
                                stars = "econ") |>
  clean_raw(caption = "Differential effect of COVID-19 across GPA quartiles")



results_gpa |>
  kable_styling(font_size = 9, latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c(" ", "Final Exam Score\n(mean = 57.1, sd = 15.6)" = 2,
                     "Did Student Get The Answer Correct (Y/N)?\n(mean = 0.6, sd = 0.49)" = 2),
                   bold = T, italic = T, escape = TRUE) |>
  add_footnote("* p {< 0.1}, ** p {< 0.05}, *** p {< 0.01}. Final exam scores are based on a 100-point scale.
                   Heteroskedasticity-robust standard errors are used. All regressions include the following control variables:
                   cumulative GPA, gender, race, age, whether a student is at most a sophomore, part-time status of the student.
                   All regressions also include a dummy variable, gpamiss, which is 1 if cumulative GPA is imputed using the mean and 0
                   otherwise. All regressions include course instructor fixed-effects and session fixed-effects.",
           escape = FALSE,
           threeparttable = TRUE)

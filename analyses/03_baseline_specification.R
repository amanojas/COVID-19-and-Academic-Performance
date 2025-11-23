
library(pacman)

# Load and install all packages at once
pacman::p_load(
  tidymodels, tidyverse, rio, naniar, labelled, sjlabelled, haven, fixest,
  vtable, ggfixest, modelsummary, gtsummary, simputation, kableExtra, readxl,
  panelsummary, cowplot, parameters
)



## This code produces table 3.2 in the main paper ------------------------------
## Title : Baseline estimates of effect of COVID-19 on students' academic performance

### Loading the question level data ------------------------------------------

question_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/question_level.rds"
question_level <- read_rds(question_url)

### Loading the exam level data ------------------------------------------
## These scores are out of possible 40 points.

exam_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/exam_level.rds"
exam_level <- read_rds(exam_url) 


################################################################################################################
################################################################################################################
########### SIMPLE FIRST DIFFERENCE USING TOTAL EXAM SCORES AND MATCHED QUESTIONS IN ECO 1001 ##################
################################################################################################################
################################################################################################################


### Regressions using exam scores as outcome

exam_base <- feols(
  score ~ post + female + r_black + r_asian + r_hispa + r_other + online +
    cumgpa + parttime + gpamiss | instructor + session,
  data = exam_level,
  vcov = "hc1"
)

exam_base_long <- feols(
  score ~ time + female + r_black + r_asian + r_hispa + r_other + online +
    cumgpa + parttime + gpamiss | instructor + session,
  data = exam_level,
  vcov = "hc1"
)


##### -------------------------------------------------------------------------------------------------

## ### Regressions using performance on matched questions as outcome

ques_base <- feols(
  correct ~ post + female + r_black + r_asian + r_hispa + r_other + online +
    cumgpa + parttime + gpamiss | instructor + session,
  data = question_level,
  vcov = "hc1"
)

ques_base_long <- feols(
  correct ~ time + female + r_black + r_asian + r_hispa + r_other + online +
    cumgpa + parttime + gpamiss | instructor + session,
  data = question_level,
  vcov = "hc1"
)


#### Creating a result table -----------------------------------------------------------

rowlabs_base <- c(
  "post" = "postcovid",
  "time2" = "fall 2020",
  "time3" = "spring 2021",
  "time4" = "fall 2021",
  "time5" = "spring 2022"
)

# Re-create a kableExtra object (not a raw string)
results_base <- panelsummary(
  list(exam_base, exam_base_long, ques_base, ques_base_long),
  coef_map = rowlabs_base,
  gof_map = c("nobs") # adjust as needed
)

### Formatting the table ----------------------  

results_base |>
  kable_styling(font_size = 9, latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c(" " = 1, "Final Exam Score\n(mean = 57.1, sd = 15.6)" = 2,
                     "Did Student Get The Answer Correct (Y/N)?\n(mean = 0.6, sd = 0.49)" = 2),
                   bold = TRUE, italic = TRUE, escape = TRUE) |>
  add_footnote("* p {< 0.1}, ** p {< 0.05}, *** p {< 0.01}. Final exam scores are based on a 100-point scale.\nHeteroskedasticity-robust standard errors are used.\nAll regressions include the following control variables: cumulative GPA, gender, race, age, whether a student is at least a sophomore, part-time status of the student.\nAll regressions also include a dummy variable, gpamiss, which is 1 if cumulative GPA is imputed using the mean and 0 otherwise.\nAll regressions include course instructor fixed-effects and session fixed-effects.",
           escape = FALSE, threeparttable = TRUE)
           

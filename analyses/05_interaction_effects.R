if (!require("pacman")) {
  install.packages("pacman", repos = "http://cran.us.r-project.org")
}

# Load and install all packages at once
pacman::p_load(
  tidymodels,
  tidyverse,
  rio,
  naniar,
  labelled,
  sjlabelled,
  haven,
  fixest,
  vtable,
  ggfixest,
  modelsummary,
  gtsummary,
  simputation,
  kableExtra,
  readxl,
  panelsummary,
  cowplot,
  parameters
)


## This code produces table 3.4 in the main paper ------------------------------
## Title : Interaction effects of COVID-19 on students’ academic performance
## This includes both GPA comparison and online vs hybrid comparison.

### Loading the question level data ------------------------------------------

question_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/question_level.rds"
question_level <- read_rds(question_url)

### Loading the exam level data ------------------------------------------
## These scores are out of possible 40 points.

exam_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/exam_level.rds"
exam_level <- read_rds(exam_url)


################################################################################################################

#### Interaction effects (also difference-in-differences)

exam_gpa <- exam_level |>
  feols(
    fml = score ~ post +
      lowgpa +
      online +
      female +
      r_black +
      r_asian +
      r_hispa +
      r_other +
      parttime +
      gpamiss |
      session + instructor,
    vcov = "hc1"
  )

exam_gpa_did <- exam_level |>
  feols(
    fml = score ~ post +
      lowgpa +
      post * lowgpa +
      online +
      female +
      r_black +
      r_asian +
      r_hispa +
      r_other +
      parttime +
      gpamiss |
      session + instructor,
    vcov = "hc1"
  )


ques_gpa <- question_level |>
  feols(
    fml = correct ~ post +
      lowgpa +
      online +
      female +
      r_black +
      r_asian +
      r_hispa +
      r_other +
      parttime +
      gpamiss |
      session + instructor,
    vcov = "hc1"
  )

ques_gpa_did <- question_level |>
  feols(
    fml = correct ~ post +
      lowgpa +
      post * lowgpa +
      online +
      female +
      r_black +
      r_asian +
      r_hispa +
      r_other +
      parttime +
      gpamiss |
      session + instructor,
    vcov = "hc1"
  )


################################################################################################################
################################################################################################################
#################### IMPACT OF COVID ON CHANGE IN GAPS BETWEEN GROUPS (ONLINE VS HYBRID) #######################
################################################################################################################
################################################################################################################

### online vs hybrid (both outcomes)
## underclassperson status is not available in 2020. I do not include that
## since it might bias the results
### Only exam scores unweighted

exam_online <- exam_level |>
  feols(
    fml = score ~ post +
      online +
      female +
      r_black +
      r_asian +
      r_hispa +
      r_other +
      cumgpa +
      parttime +
      gpamiss |
      session + instructor,
    vcov = "hc1"
  )

exam_online_did <- exam_level |>
  feols(
    fml = score ~ post +
      online +
      post * online +
      female +
      r_black +
      r_asian +
      r_hispa +
      r_other +
      cumgpa +
      parttime +
      gpamiss |
      session + instructor,
    vcov = "hc1"
  )


ques_online <- question_level |>
  feols(
    fml = correct ~ post +
      online +
      female +
      r_black +
      r_asian +
      r_hispa +
      r_other +
      cumgpa +
      parttime +
      gpamiss |
      session + instructor,
    vcov = "hc1"
  )

ques_online_did <- question_level |>
  feols(
    fml = correct ~ post +
      online +
      post * online +
      female +
      r_black +
      r_asian +
      r_hispa +
      r_other +
      cumgpa +
      parttime +
      gpamiss |
      session + instructor,
    vcov = "hc1"
  )


###  Ask this question to prof.Joyce

# ques_online_did_trial <- question_level |>
#   filter(time %in% c("1", "2", "3")) |>
#   feols(fml = correct ~ post + online + post * online + female + r_black + r_asian + r_hispa + r_other + cumgpa +
#     parttime + gpamiss | session + instructor, vcov = "hc1")

### CREATE A TABLE FOR ALL INTERACTION EFFECTS IN ONE ------------------------------------------------------------

rowlabs_did <- c(
  "post" = "postcovid",
  "lowgpa" = "lowgpa",
  "post:lowgpa" = "post x lowgpa",
  "online" = "online",
  "post:online" = "post x online"
)

results_did <- panelsummary(
  list(exam_gpa, exam_gpa_did, ques_gpa, ques_gpa_did),
  list(exam_online, exam_online_did, ques_online, ques_online_did),
  panel_labels = c("Panel A: Low GPA vs High GPA", "Panel B: Online vs Hybrid"),
  mean_dependent = T,
  gof_map = c("nobs", "r.squared"),
  coef_map = rowlabs_did,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  pretty_num = T,
  caption = "Baseline Specification"
)


results_did |>
  kable_styling(
    font_size = 9,
    latex_options = c("scale_down", "HOLD_position")
  ) |>
  add_header_above(
    c(
      " ",
      "Final Exam Score" = 2,
      "Did Student Get The Answer Correct (Y/N)?" = 2
    ),
    bold = T,
    italic = T,
    escape = FALSE
  ) |>
  column_spec(5, width = "10em") |>
  add_footnote(
    "* p {< 0.1}, ** p {< 0.05}, *** p {< 0.01}. Heteroskedasticity-robust standard errors are used.
               All regressions include the following control variables: cumulative GPA, gender, race, and part-time 
               status of the student. All regressions also include a dummy variable, gpamiss, which is 1 if cumulative
               GPA is imputed using the mean and 0 otherwise. All regressions include session fixed-effects and course
               instructor fixed-effects.",
    escape = FALSE
  )


##### CREATE AND SAVE THE LIST OF MODELS

gpa_models_un <- list(exam_gpa, exam_gpa_did, ques_gpa, ques_gpa_did)
online_models_un <- list(
  exam_online,
  exam_online_did,
  ques_online,
  ques_online_did
)

saveRDS(
  gpa_models_un,
  "S:\\CUNY Coursework\\Paper2\\Aman_E1001\\journal_article\\unstandardised-interaction-models-gpa.rds"
)
saveRDS(
  online_models_un,
  "S:\\CUNY Coursework\\Paper2\\Aman_E1001\\journal_article\\unstandardised-interaction-models-online.rds"
)

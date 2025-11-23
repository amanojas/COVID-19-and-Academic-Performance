if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")

# Load and install all packages at once
pacman::p_load(
  tidymodels, tidyverse, rio, naniar, labelled, sjlabelled, haven, fixest,
  vtable, ggfixest, modelsummary, gtsummary, simputation, kableExtra, readxl,
  panelsummary, cowplot, parameters, patchwork
)


### Loading the question level data ------------------------------------------

question_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/question_level.rds"
question_level <- read_rds(question_url)

### Loading the exam level data ------------------------------------------
## These scores are out of possible 40 points.

exam_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/exam_level.rds"
exam_level <- read_rds(exam_url) 


################################################################################################################
################################################################################################################
#################################### HARD VS MEDIUM VS EASY QUESTIONS  #########################################
################################################################################################################
################################################################################################################


# Low vs High GPA (Hard vs Easy Questions)---------------------------------------------------------------------------------------------------


ques_gpa_easy <- question_level |>
  filter(difficulty == "e") |>
  feols(fml = correct ~ post + lowgpa + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_gpa_did_easy <- question_level |>
  filter(difficulty == "e") |>
  feols(fml = correct ~ post + lowgpa + post * lowgpa + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_gpa_hard <- question_level |>
  filter(difficulty == "h") |>
  feols(fml = correct ~ post + lowgpa + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_gpa_did_hard <- question_level |>
  filter(difficulty == "h") |>
  feols(fml = correct ~ post + lowgpa + post * lowgpa + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")


# Online hybrid (Hard vs Easy Questions)---------------------------------------------------------------------------------------------------

ques_online_hard <- question_level |>
  filter(difficulty == "h") |>
  feols(fml = correct ~ post + online + female + r_black + r_asian + r_hispa + r_other + cumgpa +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_online_did_hard <- question_level |>
  filter(difficulty == "h") |>
  feols(fml = correct ~ post + online + post * online + female + r_black + r_asian + r_hispa + r_other + cumgpa +
          parttime + gpamiss | session + instructor, vcov = "hc1")


ques_online_easy <- question_level |>
  filter(difficulty == "e") |>
  feols(fml = correct ~ post + online + female + r_black + r_asian + r_hispa + r_other + cumgpa +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_online_did_easy <- question_level |>
  filter(difficulty == "e") |>
  feols(fml = correct ~ post + online + post * online + female + r_black + r_asian + r_hispa + r_other + cumgpa +
          parttime + gpamiss | session + instructor, vcov = "hc1")


##### CREATE A TABLE------------------------------------------------
#####-------------------------------------------------------------------------------------------------------------------------
  
rowlabs_eh <- c(
  "post" = "postcovid",
  "lowgpa" = "lowgpa",
  "post:lowgpa" = "post x lowgpa",
  "online" = "online",
  "post:online" = "post x online"
)

results_all <- panelsummary(
  list(ques_gpa_hard, ques_gpa_did_hard, ques_gpa_easy, ques_gpa_did_easy),
  list(ques_online_hard, ques_online_did_hard, ques_online_easy, ques_online_did_easy),
  panel_labels = c("Panel A: Low GPA vs High GPA", "Panel B: Online vs Hybrid"),
  mean_dependent = T,
  gof_map = c("nobs", "r.squared"),
  coef_map = rowlabs_eh,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  pretty_num = T) 


results_all |>
  kable_styling(font_size = 9, latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c(" ", "Hard Questions" = 2, "Easy Questions" = 2),
                   bold = T, italic = T, escape = FALSE
  ) |>
  column_spec(5, width = "10em") |>
  add_footnote("* p {< 0.1}, ** p {< 0.05}, *** p {< 0.01}. Heteroskedasticity-robust standard errors are used.
               All regressions include the following control variables: cumulative GPA, gender, race, and part-time 
               status of the student. All regressions also include a dummy variable, gpamiss, which is 1 if cumulative
               GPA is imputed using the mean and 0 otherwise. All regressions include session fixed-effects and course
               instructor fixed-effects.", escape = FALSE)





##### Mean and SD of Dependent Variable --------------------------------------------------


# avg_fd_im |>
#   filter(diff == "h" | diff == "e") |>
#   group_by(diff) |>
#   summarise(mean_correct = mean(correct, na.rm = T),
#             sd_correct   = sd(correct, na.rm = T))
# 
# 
# dplyr::add_row(term = "Std. dev. of Dependent Variable",
#                `(1)` = "0.495",
#                `(2)` = "0.495",
#                `(3)` = "0.479",
#                `(4)` = "0.479",
#                .before = 8)


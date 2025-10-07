packages <- c(
  "tidymodels", "tidyverse", "rio", "naniar", "labelled", "sjlabelled", "haven", "fixest", "vtable", "simputation",
  "modelsummary", "gtsummary", "ggfixest","panelsummary", "parameters"
)
install.packages(packages, repos = "http://cran.us.r-project.org") # Installing packages at once
lapply(packages, library, character.only = T) # Loading the packages



#############################################
############ LOAD THE FULL DATA #############
#############################################

totalscores <- readRDS("S:\\CUNY Coursework\\Paper2\\Aman_E1001\\Data\\total-exam\\3-1-total-scores-all.rds")
imp_models <- readRDS("S:\\CUNY Coursework\\Paper2\\Aman_E1001\\Analyses\\2-1-data-with-diff-impute-gpa.rds")

imp_models$instruction_mode <- droplevels(imp_models$instruction_mode)

# Matched Questions ---------------------------------------------------------------------------------------------------


# 1     0
# 2     3.08
# 3     3.32
# 4     3.68
# 5     4


avg_fd_im <- imp_models |>
  filter(imp_model == "mean") |>
  filter(instruction_mode != "P") |>
  mutate(post = if_else(time > 2, 1, 0),
         time = factor(time),
         online = if_else(instruction_mode == "O", 1, 0),
         q1 = if_else(cumgpa <= 3.08, 1, 0),
         q2 = if_else(cumgpa > 3.08 & cumgpa <= 3.32, 1, 0),
         q3 = if_else(cumgpa > 3.32 & cumgpa <= 3.68, 1, 0),
         q4 = if_else(cumgpa > 3.68, 1, 0)) |>
  mutate(on = online, hyb = hybrid) |>
  mutate(across(where(is.character), as_factor))

avg_fd_im$instruction_mode <- droplevels(avg_fd_im$instruction_mode)

# Exam Scores ---------------------------------------------------------------------------------------------------

# 1  0
# 2  3.01
# 3  3.37
# 4  3.71
# 5  4


tscores <- totalscores |>
  filter(instruction_mode != "P") |>
  mutate(q1 = if_else(cumgpa <= 3.01, 1, 0),
         q2 = if_else(cumgpa > 3.01 & cumgpa <= 3.37, 1, 0),
         q3 = if_else(cumgpa > 3.37 & cumgpa <= 3.71, 1, 0),
         q4 = if_else(cumgpa > 3.71, 1, 0))

tscores$instruction_mode <- droplevels(tscores$instruction_mode)


################################################################################################################
################################################################################################################
#################### IMPACT OF COVID ON CHANGE IN GAPS BETWEEN GROUPS (LOW GPA VS HIGH GPA) ####################
################################################################################################################
################################################################################################################

exam_gpa_4 <- tscores |>
  feols(fml = score ~ post + q1 + q2 + q3 + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

exam_gpa_int_4 <- tscores |>
  feols(fml = score ~ post + q1 + q2 + q3 + post * q1 + post * q2 + post * q3 + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")


ques_gpa_4 <- avg_fd_im |>
  feols(fml = correct ~ post + q1 + q2 + q3 + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_gpa_int_4 <- avg_fd_im |>
  feols(fml = correct ~ post + q1 + q2 + q3 + post * q1 + post * q2 + post * q3 + online + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")


########## Standardized cofficients ------------------------------------------

sd_e <- standardise_parameters(exam_gpa_4)## scores- first diff
sd_e_int <- standardise_parameters(exam_gpa_int_4)## scores - interactions
sd_q <- standardise_parameters(ques_gpa_4)## questions - first diff
sd_q_int <- standardise_parameters(ques_gpa_int_4)## questions - interactions


#### First Model

sde_1 <- paste0("[",as.character(round(sd_e$Std_Coefficient[1],2)),"]")
sde_2 <- paste0("[",as.character(round(sd_e$Std_Coefficient[2],2)),"]")
sde_3 <- paste0("[",as.character(round(sd_e$Std_Coefficient[3],2)),"]")
sde_4 <- paste0("[",as.character(round(sd_e$Std_Coefficient[4],2)),"]")

#### Second Model
sdeint_1 <- paste0("[",as.character(round(sd_e_int$Std_Coefficient[1],2)),"]")
sdeint_2 <- paste0("[",as.character(round(sd_e_int$Std_Coefficient[2],2)),"]")
sdeint_3 <- paste0("[",as.character(round(sd_e_int$Std_Coefficient[3],2)),"]")
sdeint_4 <- paste0("[",as.character(round(sd_e_int$Std_Coefficient[4],2)),"]")
sdeint_5 <- paste0("[",as.character(round(sd_e_int$Std_Coefficient[14],2)),"]")
sdeint_6 <- paste0("[",as.character(round(sd_e_int$Std_Coefficient[15],2)),"]")
sdeint_7 <- paste0("[",as.character(round(sd_e_int$Std_Coefficient[16],2)),"]")

#### Third Model

sdq_1 <- paste0("[",as.character(round(sd_q$Std_Coefficient[1],2)),"]")
sdq_2 <- paste0("[",as.character(round(sd_q$Std_Coefficient[2],2)),"]")
sdq_3 <- paste0("[",as.character(round(sd_q$Std_Coefficient[3],2)),"]")
sdq_4 <- paste0("[",as.character(round(sd_q$Std_Coefficient[4],2)),"]")

#### Fourth Model

sdqint_1 <- paste0("[",as.character(round(sd_q_int$Std_Coefficient[1],2)),"]")
sdqint_2 <- paste0("[",as.character(round(sd_q_int$Std_Coefficient[2],2)),"]")
sdqint_3 <- paste0("[",as.character(round(sd_q_int$Std_Coefficient[3],2)),"]")
sdqint_4 <- paste0("[",as.character(round(sd_q_int$Std_Coefficient[4],2)),"]")
sdqint_5 <- paste0("[",as.character(round(sd_q_int$Std_Coefficient[14],2)),"]")
sdqint_6 <- paste0("[",as.character(round(sd_q_int$Std_Coefficient[15],2)),"]")
sdqint_7 <- paste0("[",as.character(round(sd_q_int$Std_Coefficient[16],2)),"]")


### ------------------------------------------------------------------------------------



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
  add_row(term = "", `Model 1` = sde_1, `Model 2` = sdeint_1,`Model 3` = sdq_1, `Model 4` = sdqint_1,
          .before = 3) |>
  add_row(term = "", `Model 1` = sde_2, `Model 2` = sdeint_2,`Model 3` = sdq_2, `Model 4` = sdqint_2,
          .before = 6) |>
  add_row(term = "", `Model 1` = sde_3, `Model 2` = sdeint_3,`Model 3` = sdq_3, `Model 4` = sdqint_3,
          .before = 9) |>
  add_row(term = "", `Model 1` = sde_4, `Model 2` = sdeint_4,`Model 3` = sdq_4, `Model 4` = sdqint_4,
          .before = 12) |>
  add_row(term = "", `Model 1` = "", `Model 2` = sdeint_5,`Model 3` = "", `Model 4` = sdqint_5,
          .before = 15) |>
  add_row(term = "", `Model 1` = "", `Model 2` = sdeint_6,`Model 3` = "", `Model 4` = sdqint_6,
          .before = 18) |>
  add_row(term = "", `Model 1` = "", `Model 2` = sdeint_7,`Model 3` = "", `Model 4` = sdqint_7,
          .before = 21) |>
  clean_raw(caption = "Differential Impact Across GPA quartiles")



results_gpa |>
  kable_styling(font_size = 9, latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c(" ", "Final Exam Score\n(mean = 57.1, sd = 15.6)" = 2,
                     "Did Student Get The Answer Correct (Y/N)?\n(mean = 0.6, sd = 0.49)" = 2),
                   bold = T, italic = T, escape = TRUE) |>
  add_footnote("* p {< 0.1}, ** p {< 0.05}, *** p {< 0.01}. 
               Heteroskedasticity-robust standard errors are used.
               All regressions include the following control variables: cumulative GPA, gender, race, and part-time status of the student. 
               All regressions also include a dummy variable, gpamiss, which is 1 if cumulative GPA is imputed using the mean and 0 otherwise. 
               All regressions include session fixed-effects and course instructor fixed-effects.", escape = TRUE)






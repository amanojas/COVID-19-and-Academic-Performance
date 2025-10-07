packages <- c(
  "tidymodels", "tidyverse", "rio", "naniar", "labelled", "sjlabelled", "haven", "fixest", "vtable", "simputation",
  "modelsummary", "gtsummary", "ggfixest","panelsummary", "parameters"
)
install.packages(packages, repos = "http://cran.us.r-project.org") # Installing packages at once
lapply(packages, library, character.only = T) # Loading the packages


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

exam_base <- exam_level |>
  feols(fml = score ~ post + female + r_black + r_asian + r_hispa + r_other + online +
          cumgpa + parttime + gpamiss | instructor + session, vcov = "hc1")

exam_base_long <- exam_level |>
  feols(fml = score ~ time + female + r_black + r_asian + r_hispa + r_other + online +
          cumgpa + parttime + gpamiss | instructor + session, vcov = "hc1")


##### ------- standardization coefficients with outcome exam scores
library(fixest)
# After fitting your model
exam_base <- feols(score ~ post + female + r_black + r_asian + r_hispa + r_other + online +
                     cumgpa + parttime + gpamiss | instructor + session, 
                   data = exam_level, vcov = "hc1")

# Calculate within-group (demeaned) variables
within_data <- demean(exam_level,
                      vars = c("post", "female", "r_black", "r_asian", "r_hispa", "r_other",
                               "online", "cumgpa", "parttime", "gpamiss", "score"),
                      by = c("instructor", "session"))

# Now, for each variable, calculate the within-group standard deviation
vars_to_std <- c("post", "female", "r_black", "r_asian", "r_hispa", "r_other",
                 "online", "cumgpa", "parttime", "gpamiss")
outcome <- "score"

sd_x <- sapply(vars_to_std, function(var) sd(within_data[[var]], na.rm=TRUE))
sd_y <- sd(within_data[[outcome]], na.rm=TRUE)

# Extract coefficients from your model
coef_model <- coef(exam_base)[vars_to_std]

# Manually calculate standardized coefficients
std_coef <- coef_model * (sd_x / sd_y)


# Choose predictors to standardize (excluding outcome and factor/fixed effect variables)
predictors_to_std <- c("post", "female", "r_black", "r_asian", "r_hispa", "r_other",
                       "online", "cumgpa", "parttime", "gpamiss")

# Create a new standardized dataset (leaving instructor/session/fixed effects unchanged)
exam_level_std <- exam_level %>%
  mutate(across(all_of(predictors_to_std), ~ as.numeric(scale(.))))

# Fit FE model using standardized predictors
exam_base_std <- feols(score ~ post + female + r_black + r_asian + r_hispa + r_other +
                         online + cumgpa + parttime + gpamiss | instructor + session,
                       data = exam_level_std, vcov = "hc1")

# Summary
summary(exam_base_std)
























### Regressions using exam scores as outcome

exam_base <- exam_level |>
  feols(fml = score ~ post + female + r_black + r_asian + r_hispa + r_other + online +
    cumgpa + parttime + gpamiss | instructor + session, vcov = "hc1")

exam_base_long <- exam_level |>
  feols(fml = score ~ time + female + r_black + r_asian + r_hispa + r_other + online +
    cumgpa + parttime + gpamiss | instructor + session, vcov = "hc1")

##### ------- standardization exam scores
standardize_parameters(exam_base, standardize = "posthoc") 

sd_eb <- standardize_parameters(exam_base)  ## pre-post exam
sd_el <- standardize_parameters(exam_base_long)  ## including time dummies exam

sd_ebmodel1 <- paste0("[",as.character(round(sd_eb$Std_Coefficient[1],2)),"]")

sd_elmodel22 <- paste0("[",as.character(round(sd_el$Std_Coefficient[1],2)),"]")
sd_elmodel23 <- paste0("[",as.character(round(sd_el$Std_Coefficient[2],2)),"]")
sd_elmodel24 <- paste0("[",as.character(round(sd_el$Std_Coefficient[3],2)),"]")
sd_elmodel25 <- paste0("[",as.character(round(sd_el$Std_Coefficient[4],2)),"]")


##### -------------------------------------------------------------------------------------------------

## Only for matched questions

ques_base <- avg_fd_im |>
  feols(fml = correct ~ post + female + r_black + r_asian + r_hispa + r_other + online +
    cumgpa + parttime + gpamiss | instructor + session, vcov = "hc1")

ques_base_long <- avg_fd_im |>
  feols(fml = correct ~ time + female + r_black + r_asian + r_hispa + r_other + online +
    cumgpa + parttime + gpamiss | instructor + session, vcov = "hc1")


##### ------- standardization questions

sd_qb <- standardize_parameters(ques_base)  ## pre-post questions 
sd_ql <- standardize_parameters(ques_base_long)  ## including time dummies questions

sd_qbmodel3 <- paste0("[",as.character(round(sd_qb$Std_Coefficient[1],2)),"]")

sd_qlmodel42 <- paste0("[",as.character(round(sd_ql$Std_Coefficient[1],2)),"]")
sd_qlmodel43 <- paste0("[",as.character(round(sd_ql$Std_Coefficient[2],2)),"]")
sd_qlmodel44 <- paste0("[",as.character(round(sd_ql$Std_Coefficient[3],2)),"]")
sd_qlmodel45 <- paste0("[",as.character(round(sd_ql$Std_Coefficient[4],2)),"]")




#### Creating a table -----------------------------------------------------------

rowlabs_base <- c(
  "post" = "postcovid",
  "time2" = "fall 2020",
  "time3" = "spring 2021",
  "time4" = "fall 2021",
  "time5" = "spring 2022"
)


results_base <- panelsummary_raw(
  list(exam_base, exam_base_long,  ques_base, ques_base_long),
  coef_map = rowlabs_base,
  gof_map = c("nobs", "r.squared"),
  stars = "econ") |>
  add_row(term = "", `Model 1` = sd_ebmodel1, `Model 2` = "",`Model 3` = sd_qbmodel3, `Model 4` = "",
          .before = 3) |>
  add_row(term = "", `Model 1` = "", `Model 2` = sd_elmodel22,`Model 3` = "", `Model 4` = sd_qlmodel42,
          .before = 6) |>
  add_row(term = "", `Model 1` = "", `Model 2` = sd_elmodel23,`Model 3` = "", `Model 4` = sd_qlmodel43,
          .before = 9) |>
  add_row(term = "", `Model 1` = "", `Model 2` = sd_elmodel24,`Model 3` = "", `Model 4` = sd_qlmodel44,
          .before = 12) |>
  add_row(term = "", `Model 1` = "", `Model 2` = sd_elmodel25,`Model 3` = "", `Model 4` = sd_qlmodel45,
          .before = 15) |>
  clean_raw(caption = "Baseline Specification")

results_base |>
  kable_styling(font_size = 9, latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c(" ", "Final Exam Score\n(mean = 57.1, sd = 15.6)" = 2,
                     "Did Student Get The Answer Correct (Y/N)?\n(mean = 0.6, sd = 0.49)" = 2),
                   bold = T, italic = T, escape = TRUE) |>
  footnote("* p {< 0.1}, ** p {< 0.05}, *** p {< 0.01}. Final exam scores are based on a 100-point scale.
           Heteroskedasticity-robust standard errors are used. All regressions include the following control variables: 
           cumulative GPA, gender, race, age, whether a student is at least a sophomore ,part-time status of the student. 
           All regressions also include a dummy variable, gpamiss, which is 1 if cumulative GPA is imputed using the mean and 0 otherwise. 
           All regressions include course instructor fixed-effects and session fixed-effects.",
           escape = FALSE, threeparttable = T, footnote_as_chunk = T)



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

### I am changing variable names for these two separate interactions to get
### around the issue of reporting specific results in the table


## FOR LOW vs HIGH GPA I, I use on and hyb ------------------------------------------------------------------------------------------------
## FOR ONLINE vs HYBRID, I use online and hybrid
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
         q4 = if_else(cumgpa > 3.71, 1, 0)) |>
  mutate(on = online, hyb = hybrid) 

tscores$instruction_mode <- droplevels(tscores$instruction_mode)


#############################################
############ LOAD THE MODELS #############
#############################################

gpa_models_un <- readRDS("S:\\CUNY Coursework\\Paper2\\Aman_E1001\\journal_article\\unstandardised-interaction-models-gpa.rds")
online_models_un <- readRDS("S:\\CUNY Coursework\\Paper2\\Aman_E1001\\journal_article\\unstandardised-interaction-models-online.rds")

#### Standardization of Coefficients 

#### ALl models for GPA --------------------------------------------

sd1 <- standardize_parameters(gpa_models_un[[1]])   ## GPA - exam
sd2 <- standardize_parameters(gpa_models_un[[2]])   ## GPA DID - exam
sd3 <- standardize_parameters(gpa_models_un[[3]])  ## GPA - questions
sd4 <- standardize_parameters(gpa_models_un[[4]])   ## GPA DID - questions

#### Top Panel

### First Model 
sd1_1 <- paste0("[",as.character(round(sd1$Std_Coefficient[1],2)),"]")
sd1_2 <- paste0("[",as.character(round(sd1$Std_Coefficient[2],2)),"]")

### Second Model
sd2_1 <- paste0("[",as.character(round(sd2$Std_Coefficient[1],2)),"]")
sd2_2 <- paste0("[",as.character(round(sd2$Std_Coefficient[2],2)),"]")
sd2_3 <- paste0("[",as.character(round(sd2$Std_Coefficient[12],2)),"]")

### Third Model 
sd3_1 <- paste0("[",as.character(round(sd3$Std_Coefficient[1],2)),"]")
sd3_2 <- paste0("[",as.character(round(sd3$Std_Coefficient[2],2)),"]")

### Fourth Model

sd4_1 <- paste0("[",as.character(round(sd4$Std_Coefficient[1],2)),"]")
sd4_2 <- paste0("[",as.character(round(sd4$Std_Coefficient[2],2)),"]")
sd4_3 <- paste0("[",as.character(round(sd4$Std_Coefficient[12],2)),"]")

#### ALl models for Online --------------------------------------------

### Bottom Panel

sd5 <- standardize_parameters(online_models_un[[1]])   ## ONLINE - exam
sd6 <- standardize_parameters(online_models_un[[2]])   ## ONLINE DID - exam
sd7 <- standardize_parameters(online_models_un[[3]])  ## ONLINE - questions
sd8 <- standardize_parameters(online_models_un[[4]])   ## ONLINE DID - questions


### First Model 
sd5_1 <- paste0("[",as.character(round(sd5$Std_Coefficient[1],2)),"]")
sd5_2 <- paste0("[",as.character(round(sd5$Std_Coefficient[2],2)),"]")

### Second Model
sd6_1 <- paste0("[",as.character(round(sd6$Std_Coefficient[1],2)),"]")
sd6_2 <- paste0("[",as.character(round(sd6$Std_Coefficient[2],2)),"]")
sd6_3 <- paste0("[",as.character(round(sd6$Std_Coefficient[12],2)),"]")

### Third Model 
sd7_1 <- paste0("[",as.character(round(sd7$Std_Coefficient[1],2)),"]")
sd7_2 <- paste0("[",as.character(round(sd7$Std_Coefficient[2],2)),"]")

### Fourth Model

sd8_1 <- paste0("[",as.character(round(sd8$Std_Coefficient[1],2)),"]")
sd8_2 <- paste0("[",as.character(round(sd8$Std_Coefficient[2],2)),"]")
sd8_3 <- paste0("[",as.character(round(sd8$Std_Coefficient[12],2)),"]")



###### ---------------------------------------------------------------------------------------------------------------

rowlabs_did <- c(
  "post" = "postcovid",
  "lowgpa" = "lowgpa",
  "post:lowgpa" = "post x lowgpa",
  "online" = "online",
  "post:online" = "post x online"
)

results_did <- panelsummary_raw(
  gpa_models_un,
  online_models_un,
  mean_dependent = FALSE,
  gof_map = c("nobs", "r.squared"),
  coef_map = rowlabs_did,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01)) |>
  add_row(term = "", `Model 1` = sd1_1, `Model 2` = sd2_1,`Model 3` = sd3_1, `Model 4` = sd4_1,
          .before = 3) |>
  add_row(term = "", `Model 1` = sd1_2, `Model 2` = sd2_2,`Model 3` = sd3_2, `Model 4` = sd4_2,
          .before = 6) |>
  add_row(term = "", `Model 1` = "", `Model 2` = sd2_3,`Model 3` = "", `Model 4` = sd4_3,
          .before = 9) |>
  add_row(term = "", `Model 1` = sd5_1, `Model 2` = sd6_1,`Model 3` = sd7_1, `Model 4` = sd8_1,
          .before = 14) |>
  add_row(term = "", `Model 1` = sd5_2, `Model 2` = sd6_2,`Model 3` = sd7_2, `Model 4` = sd8_2,
          .before = 17) |>
  add_row(term = "", `Model 1` = "", `Model 2` = sd6_3,`Model 3` = "", `Model 4` = sd8_3,
          .before = 20) |>
  clean_raw(caption = "Interaction Effects")
  


results_did |>
  kable_styling(font_size = 9, latex_options = c("scale_down", "HOLD_position")) |>
  add_header_above(c(" ", "Final Exam Score\n(mean = 57.1, sd = 15.6)" = 2,
                     "Did Student Get The Answer Correct (Y/N)?\n(mean = 0.6, sd = 0.49)" = 2),
                   bold = T, italic = T, escape = TRUE) |>
  pack_rows("Panel A: Low GPA vs High GPA", 1, 11, bold = T, italic = T) |>
  pack_rows("Panel B: Online vs Hybrid", 12, 22, bold = T, italic = T) |>
  column_spec(1, width = "15em") |>
  add_footnote("* p {< 0.1}, ** p {< 0.05}, *** p {< 0.01}. Heteroskedasticity-robust standard errors are used.
               All regressions include the following control variables: cumulative GPA, gender, race, and part-time 
               status of the student. All regressions also include a dummy variable, gpamiss, which is 1 if cumulative
               GPA is imputed using the mean and 0 otherwise. All regressions include session fixed-effects and course
               instructor fixed-effects.", escape = FALSE)



# panel_labels = c("Panel A: Low GPA vs High GPA", "Panel B: Online vs Hybrid"),


# 
# #### Mean and SD of the dependent variables 
# tscores |>
#      summarise(mean_score = mean(score, na.rm = T),
#              sd_score   = sd(score, na.rm = T))
# avg_fd_im |>
#   summarise(mean_correct = mean(correct, na.rm = T),
#             sd_correct   = sd(correct, na.rm = T))


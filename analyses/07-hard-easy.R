packages <- c(
  "tidymodels", "tidyverse", "rio", "naniar", "labelled", "sjlabelled", "haven", "fixest", "vtable", "simputation",
  "modelsummary", "gtsummary", "ggfixest"
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
  mutate(qcode = as.character(code)) |>
  mutate(on = online, hyb = hybrid) |>
  separate(col = qcode, into = c("ch", "diffnum"), sep = c(-2)) |>
  separate(col = diffnum, into = c("diff", "num"), sep = c(1)) |>
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
#################################### HARD VS MEDIUM VS EASY QUESTIONS  #########################################
################################################################################################################
################################################################################################################


# Low vs High GPA (Hard vs Easy Questions)---------------------------------------------------------------------------------------------------


ques_gpa_easy <- avg_fd_im |>
  filter(diff == "e") |>
  feols(fml = correct ~ post + lowgpa + on + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_gpa_did_easy <- avg_fd_im |>
  filter(diff == "e") |>
  feols(fml = correct ~ post + lowgpa + post * lowgpa + on + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_gpa_hard <- avg_fd_im |>
  filter(diff == "h") |>
  feols(fml = correct ~ post + lowgpa + on + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_gpa_did_hard <- avg_fd_im |>
  filter(diff == "h") |>
  feols(fml = correct ~ post + lowgpa + post * lowgpa + on + female + r_black + r_asian + r_hispa + r_other +
          parttime + gpamiss | session + instructor, vcov = "hc1")


# Online hybrid (Hard vs Easy Questions)---------------------------------------------------------------------------------------------------

ques_online_hard <- avg_fd_im |>
  filter(diff == "h") |>
  feols(fml = correct ~ post + online + female + r_black + r_asian + r_hispa + r_other + cumgpa +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_online_did_hard <- avg_fd_im |>
  filter(diff == "h") |>
  feols(fml = correct ~ post + online + post * online + female + r_black + r_asian + r_hispa + r_other + cumgpa +
          parttime + gpamiss | session + instructor, vcov = "hc1")


ques_online_easy <- avg_fd_im |>
  filter(diff == "e") |>
  feols(fml = correct ~ post + online + female + r_black + r_asian + r_hispa + r_other + cumgpa +
          parttime + gpamiss | session + instructor, vcov = "hc1")

ques_online_did_easy <- avg_fd_im |>
  filter(diff == "e") |>
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


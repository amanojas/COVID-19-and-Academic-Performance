packages <- c(
  "tidymodels", "tidyverse", "rio", "naniar", "labelled", "sjlabelled", "haven", "fixest", "vtable", "simputation",
  "modelsummary", "gtsummary", "ggfixest", "cowplot"
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

# - \usepackage[nolists]{endfloat}

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
         q4 = if_else(cumgpa > 3.71, 1, 0)) |>
  mutate(on = online, hyb = hybrid)

tscores$instruction_mode <- droplevels(tscores$instruction_mode)




#####################################################################################
#####################################################################################
############### LONG TERM EFFECTS ON HIGH VS LOW GPA USING YEAR DUMMIES #############
#####################################################################################
#####################################################################################

exam_longterm_gpa <- tscores |>
  feols(fml = score ~ i(factor_var = time, var = lowgpa, ref = 1) + online + female + r_hispa + r_black + r_asian + r_other
    + session + parttime + gpamiss | time + instructor, vcov = "hc1")

ques_longterm_gpa <- avg_fd_im |>
  feols(fml = correct ~ i(factor_var = time, var = lowgpa, ref = 1) + online + female + r_hispa + r_black + r_asian + r_other
    + session + parttime + gpamiss | time + instructor, vcov = "hc1")

# msummary(list(exam_longterm_gpa, ques_longterm_gpa),
#   stars = c("*" = .1, "**" = .05, "***" = .01)
# )


#####################################################################################
#####################################################################################
############### LONG TERM EFFECTS ON ONLINE VS HYBRID USING YEAR DUMMIES ############
#####################################################################################
#####################################################################################


exam_longterm_online <- tscores |>
  filter(time %in% c("1", "2", "3")) |>
  feols(fml = score ~ i(factor_var = time, var = online, ref = 1) + cumgpa + female + r_hispa + r_black + r_asian + r_other
    + parttime + gpamiss | instructor, vcov = "hc1")

ques_longterm_online <- avg_fd_im |>
  filter(time %in% c("1", "2", "3")) |>
  feols(fml = correct ~ i(factor_var = time, var = online, ref = 1) + cumgpa + female + r_hispa + r_black + r_asian + r_other
    + parttime + gpamiss | instructor, vcov = "hc1")


# Based on Exam Scores--------------------------------------------------------------------------------------------------------------------

p1 <- ggiplot(
  list("Low GPA vs High GPA" = exam_longterm_gpa, "Online vs Hybrid" = exam_longterm_online),
  main = " ",
  ref.line = 1,
  xlab = "Semester",
  multi_style = "facet",
  geom_style = "errorbar",
  theme = theme_minimal() +
    theme(
      text = element_text(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
) +
  geom_line(group = 1) +
  ylim(-16,2)


# Based on Matched Questions--------------------------------------------------------------------------------------------------------------------

p2 <- ggiplot(
  list("Low GPA vs High GPA" = ques_longterm_gpa, "Online vs Hybrid" = ques_longterm_online),
  main = " ",
  ref.line = 1,
  xlab = "Semester",
  multi_style = "facet",
  geom_style = "errorbar",
  theme = theme_minimal() +
    theme(
      text = element_text(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
) +
  geom_line(group = 1) +
  ylim(-0.2, 0.2)


###########################################################################################################
###########################################################################################################
###########################################################################################################

plot_grid(
  p1, p2,
  labels = c("     Effect on Exam Scores",
             "Effect on Mean Prob. of Answering a Similar Question"),
  ncol = 1)


tscores |>
  select(score) |>
  ggplot(aes(x = score)) +
  geom_histogram(bins = 100)

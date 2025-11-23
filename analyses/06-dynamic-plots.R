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



#####################################################################################
#####################################################################################
############### LONG TERM EFFECTS ON HIGH VS LOW GPA USING YEAR DUMMIES #############
#####################################################################################
#####################################################################################

exam_longterm_gpa <- exam_level |>
  feols(fml = score ~ i(factor_var = time, var = lowgpa, ref = 1) + online + female + r_hispa + r_black + r_asian + r_other
    + session + parttime + gpamiss | time + instructor, vcov = "hc1")

ques_longterm_gpa <- question_level |>
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


exam_longterm_online <- exam_level |>
  filter(time %in% c("1", "2", "3")) |>
  feols(fml = score ~ i(factor_var = time, var = online, ref = 1) + cumgpa + female + r_hispa + r_black + r_asian + r_other
    + parttime + gpamiss | instructor, vcov = "hc1")

ques_longterm_online <- question_level |>
  filter(time %in% c("1", "2", "3")) |>
  feols(fml = correct ~ i(factor_var = time, var = online, ref = 1) + cumgpa + female + r_hispa + r_black + r_asian + r_other
    + parttime + gpamiss | instructor, vcov = "hc1")



# Create plots with consistent minimal styling
plot_gpa_exam <- ggiplot(exam_longterm_gpa, 
                         main = "Exam Scores: Low vs High GPA",
                         xlab = "Time Period",
                         ylab = "Estimates",
                         ref.line = FALSE,
                         pt.join = TRUE,
                         geom_style = "pointrange",
                         ci_level = 0.95) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10, face = "plain"),
        axis.title = element_text(size = 10))

plot_gpa_ques <- ggiplot(ques_longterm_gpa, 
                         main = "Matched Questions: Low vs High GPA",
                         xlab = "Time Period",
                         ylab = "Estimates",
                         ref.line = FALSE,
                         pt.join = TRUE,
                         geom_style = "pointrange") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10, face = "plain"),
        axis.title = element_text(size = 10))

plot_online_exam <- ggiplot(exam_longterm_online, 
                            main = "Exam Scores: Online vs Hybrid",
                            xlab = "Time Period",
                            ylab = "Estimates",
                            ref.line = FALSE,
                            pt.join = TRUE,
                            geom_style = "pointrange") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10, face = "plain"),
        axis.title = element_text(size = 10))

plot_online_ques <- ggiplot(ques_longterm_online, 
                            main = "Matched Questions: Online vs Hybrid",
                            xlab = "Time Period",
                            ylab = "Estimates",
                            ref.line = FALSE,
                            pt.join = TRUE,
                            geom_style = "pointrange") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10, face = "plain"),
        axis.title = element_text(size = 10))

(plot_gpa_exam + plot_gpa_ques) / 
  (plot_online_exam + plot_online_ques) 




packages <- c(
  "tidymodels", "tidyverse", "rio", "naniar", "labelled", "sjlabelled", "haven", "fixest", "vtable", "simputation",
  "modelsummary", "gtsummary"
)
install.packages(packages, repos = "http://cran.us.r-project.org") # Installing packages at once
lapply(packages, library, character.only = T) # Loading the packages


##### LOAD THE DATA #####
#############################################
############ LOAD THE FULL DATA #############
#############################################

totalscores <- readRDS("/Users/amandesai/Desktop/research/covid_learning_loss/Aman_E1001/Data/total-exam/3-1-total-scores-all.rds")
imp_models <- readRDS("/Users/amandesai/Desktop/research/covid_learning_loss/Aman_E1001/Analyses/2-1-data-with-diff-impute-gpa.rds")

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
  mutate(
    post = if_else(time > 2, 1, 0),
    time = factor(time),
    online = if_else(instruction_mode == "O", 1, 0),
    q1 = if_else(cumgpa <= 3.08, 1, 0),
    q2 = if_else(cumgpa > 3.08 & cumgpa <= 3.32, 1, 0),
    q3 = if_else(cumgpa > 3.32 & cumgpa <= 3.68, 1, 0),
    q4 = if_else(cumgpa > 3.68, 1, 0),
    gpamiss = factor(if_else(gpamiss == 0, 0, 1))
  ) |>
  mutate(qcode = as.character(code)) |>
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
  mutate(
    q1 = if_else(cumgpa <= 3.01, 1, 0),
    q2 = if_else(cumgpa > 3.01 & cumgpa <= 3.37, 1, 0),
    q3 = if_else(cumgpa > 3.37 & cumgpa <= 3.71, 1, 0),
    q4 = if_else(cumgpa > 3.71, 1, 0),
    gpamiss = factor(if_else(gpamiss == 0, 0, 1))
  )

tscores$instruction_mode <- droplevels(tscores$instruction_mode)



######## CREATE FINAL MATCHED QUESTION DATA #########

question_level <- avg_fd_im |>
  select(student_id, correct, post, female, starts_with("r_"), cumgpa,
    parttime, underclassperson, instructor, session,
    year, online, lowgpa, difficulty = diff, gpamiss, q1, q2, q3, q4, age,
    time
  ) |>
  group_by(student_id) |>
  mutate(ID = cur_group_id()) |>
  relocate(ID, .before = student_id) |>
  ungroup() |>
  select(-student_id)

saveRDS(question_level, "/Users/amandesai/Desktop/research/covid/data/question_level.rds")
write_csv(question_level, "/Users/amandesai/Desktop/research/covid/data/question_level.csv")

######## CREATE FINAL EXAM LEVEL DATA #########

exam_level <- tscores |>
  select(student_id, score, post, female, starts_with("r_"), cumgpa,
         parttime, underclassperson, instructor, session,
         year, online, lowgpa, gpamiss,
         q1, q2, q3, q4, age, time
  ) |>
  group_by(student_id) |>
  mutate(ID = cur_group_id()) |>
  relocate(ID, .before = student_id) |>
  ungroup() |>
  select(-student_id)

saveRDS(exam_level, "/Users/amandesai/Desktop/research/covid/data/exam_level.rds")
write_csv(exam_level, "/Users/amandesai/Desktop/research/covid/data/exam_level.csv")


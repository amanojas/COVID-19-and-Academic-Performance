packages <- c("tidymodels","tidyverse","rio","naniar", "labelled", "sjlabelled","haven","fixest", "vtable", "simputation",
              "modelsummary", "gtsummary", "estimatr", "cobalt")
install.packages(packages, repos='http://cran.us.r-project.org') # Installing packages at once
lapply(packages, library, character.only = T) 

### Loading the question level data ------------------------------------------

question_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/question_level.rds"
question_level <- read_rds(question_url)

### Loading the exam level data ------------------------------------------
## These scores are out of possible 40 points.

exam_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/exam_level.rds"
exam_level <- read_rds(exam_url)



############################################################################################
######################## Overall GPA without imputation Unweighted  ########################
############################################################################################

## I use original data for descriptive statistics (it includes missing values )

final_data <- readRDS("S:\\CUNY Coursework\\Paper2\\Aman_E1001\\Analyses\\1-1-final.rds") |>
  filter(!is.na(female)) |>
  mutate(time_period = factor(if_else(year <= 2019, "Pre-Covid", "Post-Covid")))
  

final_data$instruction_mode <- droplevels(final_data$instruction_mode)

f1 <- final_data |>
  filter(instruction_mode != "P") |>
  mutate(Fall = case_when(session == "F" ~ 1, TRUE ~ 0),
         Exam_A = case_when(version == "A" ~ 1, TRUE ~ 0),
         r_hispa = case_when(race == "HISPANIC" ~ 1, TRUE ~ 0),
         r_black = case_when(race == "AFRAM" ~ 1, TRUE ~ 0),
         r_asian = case_when(race == "ASIAN" ~ 1, TRUE ~ 0),
         r_other = case_when(race == "OTHER" ~ 1, TRUE ~ 0),
         online = case_when(instruction_mode == "O" ~ 1 , TRUE ~ 0),
         newperiod = fct_relevel(time_period, c("Pre-Covid", "Post-Covid"))) |>
  select(student_id, newperiod, correct, Hispanic = r_hispa, Black = r_black,
         Asian = r_asian, `Other race` = r_other,Fall, Online =  online, GPA = cumgpa,
         `SAT verbal` = satver, `SAT math` =  satmath, Female = female, Age = age, 
         Parttime = parttime, transfer, `Native Language English?` = englishnative, 
         `Sophomore or below` = underclassperson, year) |>
  group_by(student_id) |>
  mutate(avgcor = mean(correct, na.rm = T))|>
  ungroup() |>
  distinct(student_id, .keep_all = TRUE) |>
  select(-c("year","transfer"))



f12 <- inner_join(f1,
                  tscores |>
  select(student_id, score), by = "student_id") |>
  select(-student_id) |>
  relocate(score, .before = correct)
### I created this table in word and then used excel to report in latex

datasummary_balance(formula = ~ newperiod, 
                    data = f12,
                    fmt = 3,
                    stars = TRUE,
                    output = "table-unweighted.docx")

datasummary_balance(formula = ~ newperiod, data = f12,
                    fmt = 3, stars = TRUE)

### Pre - 843
### post - 3812


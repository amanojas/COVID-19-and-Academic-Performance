if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")

# Load and install all packages at once
pacman::p_load(
  tidymodels, tidyverse, rio, naniar, labelled, sjlabelled, haven, fixest,
  vtable, ggfixest, modelsummary, gtsummary, simputation, kableExtra, readxl,
  panelsummary, cowplot, parameters, rmarkdown
)

question_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/question_level.rds"
question_level <- read_rds(question_url)
sum_stats_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/summary_stats_data.rds"
f12 <- read_rds(sum_stats_url)







datasummary_balance(formula = ~ newperiod, 
                    data = f12,
                    fmt = 3,
                    stars = TRUE,
                    output = "table-unweighted.docx")

datasummary_balance(formula = ~ newperiod, data = f12,
                    fmt = 3, stars = TRUE)

### Pre - 843
### post - 3812


packages <- c(
  "tidymodels", "tidyverse", "rio", "naniar", "labelled", "sjlabelled", "haven", "fixest", "vtable", "simputation",
  "modelsummary", "gtsummary", "estimatr", "cobalt"
)
install.packages(packages, repos = "http://cran.us.r-project.org") # Installing packages at once
lapply(packages, library, character.only = T)


### Loading the question level data ------------------------------------------

question_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/question_level.rds"
question_level <- read_rds(question_url)

### Loading the exam level data ------------------------------------------
## These scores are out of possible 40 points.

exam_url <- "https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/exam_level.rds"
exam_level <- read_rds(exam_url)


### Loading the course withdrawal data ------------------------------------------
## It has share of students who took withdrawal, CR, NC options. 
full_grades <- read_dta("https://raw.githubusercontent.com/amanojas/COVID-19-and-Academic-Performance/main/data/grades-by-sem.dta") # s2020 available
full_g <- full_grades |>
  mutate(year = if_else(semester == "S19" | semester == "F19", 2019,
                        if_else(semester == "S20" | semester == "F20", 2020,
                                if_else(semester == "S21" | semester == "F21", 2021, 2022)
                        )
  )) |>
  mutate(session = factor(if_else(semester %in% c("S19", "S20", "S21", "S22"), "S", "F"))) |>
  mutate(session = fct_relevel(session, c("S", "F")))



#############################################################
############ Figures as they appear in the paper ############ 
#############################################################

## Figure 1: Average final GPA in ECO 1001 across semesters ------------------

# Define semester labels
gpa_labels <- c(
  "Spring 2019", "Fall 2019", "Spring 2020", "Fall 2020",
  "Spring 2021", "Fall 2021", "Spring 2022"
)

# Summarize GPA statistics by semester
gpa_summary <- full_g |>
  dplyr::select(year, session, coursegpa) |>
  dplyr::group_by(year, session) |>
  dplyr::summarise(
    avg_gpa = mean(coursegpa, na.rm = TRUE),
    sd_gpa = sd(coursegpa, na.rm = TRUE),
    n = dplyr::n(),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    margin_error = 1.96 * (sd_gpa / sqrt(n)),
    semester = factor(seq_along(year), labels = gpa_labels)
  )

# Plot the results
ggplot(gpa_summary, aes(x = semester, y = avg_gpa, label = round(avg_gpa, 2))) +
  geom_bar(stat = "identity", width = 0.3, fill = "skyblue", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = avg_gpa - margin_error, ymax = avg_gpa + margin_error),
    width = 0.1, color = "darkblue"
  ) +
  geom_text(
    size = 3, vjust = -2.5, color = "black"
  ) +
  ylim(0, 4) +
  labs(
    x = NULL,
    y = "Course GPA",
    title = ""
  ) +
  theme_minimal(base_size = 13)




## Figure 2: Withdrawal rates in ECO 1001 across semesters ------------------

## 376 students withdrew + 39 students withdrew
## 296 students CR
## 55 students NC
## 160 missing coursegpa

# Semester labels
withdrawal_labels <- c(
  "Spring 2019", "Fall 2019", "Spring 2020", "Fall 2020",
  "Spring 2021", "Fall 2021", "Spring 2022"
)

# Summarize withdrawal-related measures
withdrawal_summary <- full_g|>
  dplyr::select(year, session, CR, NC, wd)|>
  dplyr::group_by(year, session)|>
  dplyr::summarise(
    avg_NC = mean(NC, na.rm = TRUE),
    avg_CR = mean(CR, na.rm = TRUE),
    avg_WD = mean(wd, na.rm = TRUE),
    .groups = 'drop'
  )|>
  dplyr::mutate(semester = factor(seq_along(year), labels = withdrawal_labels))|>
  tidyr::pivot_longer(
    cols = c("avg_NC", "avg_CR", "avg_WD"),
    names_to = "measure",
    values_to = "avg"
  )|>
  dplyr::mutate(avg = 100 * avg)

# Set up color scheme for measures
withdrawal_colors <- c(
  "avg_NC" = "skyblue",
  "avg_CR" = "steelblue",
  "avg_WD" = "darkblue"
)

# Plot the withdrawal rates
ggplot(withdrawal_summary, aes(x = semester, y = avg, fill = measure, label = paste0(round(avg, 2), "%"))) +
  geom_col(width = 0.5, position = position_stack(), alpha = 0.7) +
  geom_text(
    size = 3, vjust = -2.2, position = position_stack(vjust = 0.9), color = "black"
  ) +
  ylim(0, 50) +
  scale_fill_manual(values = withdrawal_colors,
                    labels = c("No Credit", "Credit", "Withdrawal")) +
  labs(
    x = NULL,
    y = "Withdrawal, Credit, No Credit Rate (%)",
    title = ""
  ) +
  scale_x_discrete(labels = withdrawal_labels) +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())


########################################################################
############## Figures as they appear in the appendix ##################
########################################################################

### Figure A1: Average final exam scores in ECO 1001 across semesters ------------------

# Semester labels for exam scores

exam_labels <- c(
  "Spring 2019", "Fall 2019", "Fall 2020",
  "Spring 2021", "Fall 2021", "Spring 2022"
)

# Summarize exam score statistics

exam_summary <- exam_level|>
  dplyr::select(score, year, session)|>
  dplyr::mutate(session = forcats::fct_relevel(session, c("S", "F")))|>
  dplyr::group_by(year, session)|>
  dplyr::summarise(
    avg_score = mean(score * 0.4, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    n = dplyr::n(),
    std_error = sd_score / sqrt(n),
    .groups = 'drop'
  )|>
  dplyr::mutate(
    semester = factor(seq_along(year), labels = exam_labels),
    margin_error = 1.96 * std_error
  )

# Plot exam scores across semesters

ggplot(exam_summary, aes(x = semester, y = avg_score, label = round(avg_score, 2))) +
  geom_bar(stat = "identity", width = 0.3, fill = "skyblue", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = avg_score - margin_error, ymax = avg_score + margin_error),
    width = 0.2, color = "darkblue"
  ) +
  geom_text(
    size = 3, vjust = -3.5, color = "black"
  ) +
  ylim(0, 50) +
  labs(
    x = NULL,
    y = "Average Final Exam Score",
    title = ""
  ) +
  scale_x_discrete(labels = exam_labels) +
  theme_minimal(base_size = 13)



### Figure A2: Student shares in low and high GPA groups ------------------
## S2019,F2019,F2020,S2021,F2021,S2022
## S2019/F2019 combined due to small sample size

# Semester labels for GPA share plot
gpa_share_labels <- c(
  "Spring/Fall 2019", "Fall 2020",
  "Spring 2021", "Fall 2021", "Spring 2022"
)


# Prepare summary data: student share by GPA type
stu_share_gpa_summary <- question_level|>
  dplyr::distinct(id, .keep_all = TRUE)|>
  dplyr::group_by(time, id, typegpa)|>
  dplyr::count()|>
  dplyr::group_by(time, typegpa)|>
  dplyr::count(name = "n")|>
  dplyr::group_by(time)|>
  dplyr::mutate(share = n / sum(n))|>
  dplyr::ungroup()

# Color palette for GPA types
gpa_type_colors <- c("High GPA" = "skyblue", "Low GPA" = "darkblue")

stu_share_gpa_summary <- stu_share_gpa_summary |>
  dplyr::mutate(typegpa = dplyr::recode(typegpa,
                                      "high" = "High GPA",
                                      "low"  = "Low GPA"))

# Plot share of students by GPA type
ggplot(stu_share_gpa_summary, aes(x = factor(time), y = share, fill = typegpa, label = round(share, 2))) +
  geom_col(
    width = 0.5,
    alpha = 0.8,
    position = position_dodge(0.6),
    color = "black"
  ) +
  geom_text(
    size = 3,
    vjust = -1,
    position = position_dodge(0.6)
  ) +
  ylim(0, 1) +
  labs(
    x = NULL,
    y = "Share of High and Low GPA Students",
    fill = "GPA Type",
    title = ""
  ) +
  scale_fill_manual(values = gpa_type_colors) +
  scale_x_discrete(labels = gpa_share_labels) +
  theme_minimal(base_size = 13)



### Figure A3: Average GPA in low and high GPA groups in ECO 1001 across semesters ------------------
## S2019,F2019,F2020,S2021,F2021,S2022
# Prepare semester labels
mean_gpa_labels <- c(
  "Spring/Fall 2019", "Fall 2020",
  "Spring 2021", "Fall 2021", "Spring 2022"
)

# Summarize mean cumulative GPA by group
stu_mean_gpa_summary <- question_level %>%
  dplyr::distinct(id, .keep_all = TRUE) %>%
  dplyr::select(time, cumgpa, typegpa) %>%
  dplyr::group_by(time, typegpa) %>%
  dplyr::summarise(
    avg_gpa = mean(cumgpa, na.rm = TRUE),
    sd_score = sd(cumgpa, na.rm = TRUE),
    n = dplyr::n(),
    std_error = sd_score / sqrt(n),
    .groups = "drop"
  ) |>
  dplyr::mutate(margin_error = 1.96 * std_error)

# GPA type colors (ensure levels match your data)
gpa_type_colors <- c("High GPA" = "skyblue", "Low GPA" = "darkblue")

# If your data uses values like 'high'/'low', relabel before plotting
stu_mean_gpa_summary <- stu_mean_gpa_summary %>%
  dplyr::mutate(typegpa = dplyr::recode(typegpa,
                                        "high" = "High GPA",
                                        "low"  = "Low GPA"))

# Plot mean GPA by group across semesters
ggplot(stu_mean_gpa_summary, aes(
  x = factor(time),
  y = avg_gpa,
  group = typegpa,
  fill = typegpa,
  label = round(avg_gpa, 2)
)) +
  geom_col(
    width = 0.5,
    alpha = 0.8,
    position = position_dodge(0.6),
    color = "black"
  ) +
  geom_text(
    size = 3,
    vjust = -1,
    position = position_dodge(0.6),
    color = "black"
  ) +
  ylim(0, 6) +
  labs(
    x = NULL,
    y = "Mean GPA of High and Low GPA Students",
    fill = "GPA Type",
    title = ""
  ) +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4", "5"),
    labels = mean_gpa_labels
  ) +
  scale_fill_manual(values = gpa_type_colors) +
  theme_minimal(base_size = 13)


### Figure A4: Student shares in hybrid and online classes ------------------
#### S2019,F2019,F2020,S2021,F2021,S2022

# Semester labels for instruction mode plot
inst_mode_labels <- c(
  "Spring/Fall 2019", "Fall 2020",
  "Spring 2021", "Fall 2021", "Spring 2022"
)

# Prepare summary data: student share by instruction mode
stu_share_inst_summary <- question_level %>%
  dplyr::distinct(id, .keep_all = TRUE) %>%
  dplyr::group_by(time, id, instruction_mode) %>%
  dplyr::count() %>%
  dplyr::group_by(time, instruction_mode) %>%
  dplyr::count(name = "n") %>%
  dplyr::group_by(time) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  dplyr::ungroup()

# Set color palette for instruction modes -- ensure it matches your data
inst_mode_colors <- c("Hybrid" = "skyblue", "Online" = "darkblue")

# Relabel for legend, if your values are different
stu_share_inst_summary <- stu_share_inst_summary %>%
  dplyr::mutate(instruction_mode = dplyr::recode(instruction_mode,
                                                 "H" = "Hybrid",
                                                 "O" = "Online"
  ))

# Plot student share by instruction mode
ggplot(stu_share_inst_summary, aes(
  x = factor(time),
  y = share,
  fill = instruction_mode,
  label = round(share, 2)
)) +
  geom_col(
    width = 0.2,
    alpha = 0.7,
    position = position_dodge(0.4),
    color = "black"
  ) +
  geom_text(
    size = 3,
    vjust = -1,
    position = position_dodge(0.4)
  ) +
  ylim(0, 1) +
  labs(
    x = NULL,
    y = "Share of Students in Hybrid and Online Classes",
    fill = "Instruction Mode",
    title = ""
  ) +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4", "5"),
    labels = inst_mode_labels
  ) +
  scale_fill_manual(values = inst_mode_colors) +
  theme_minimal(base_size = 13)



####### ------------------------------------------------------------------------------------



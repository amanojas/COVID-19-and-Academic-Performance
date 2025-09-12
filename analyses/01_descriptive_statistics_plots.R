packages <- c("tidymodels","tidyverse","rio","naniar", "labelled", "sjlabelled","haven","fixest", "vtable", "simputation",
              "modelsummary", "gtsummary", "estimatr", "cobalt")
install.packages(packages, repos='http://cran.us.r-project.org') # Installing packages at once
lapply(packages, library, character.only = T) 


### Loading the question level data ------------------------------------------

question_url <- "/Users/amandesai/Desktop/research/covid_19/data/question_level.rds"
question_level <- readRDS(question_url)


#### THE GPA VALUES ARE IMPUTED USING MEAN GPA ####

avg_fd_im <- imp_models |>
  filter(imp_model == "mean") |>
  filter(instruction_mode != "P") |>
  mutate(post = if_else(time > 2, 1, 0),
         online = if_else(instruction_mode == "O", 1, 0),
         topgpa = if_else(cumgpa >= 3.7 , 1, 0),
         midgpa = if_else(cumgpa >=2.7 & cumgpa < 3.7, 1, 0),
         botgpa = if_else(cumgpa < 2.7, 1, 0))


avg_fd_im$instruction_mode <- droplevels(avg_fd_im$instruction_mode)


#### S2019,F2019,F2020,S2021,F2021,S2022

labs1 <- c("Spring/Fall 2019", "Fall 2020",
           "Spring 2021", "Fall 2021", "Spring 2022")

### STUDENT SHARE IN HIGH AND LOW GPA GROUPS

stu_share_gpa <- avg_fd_im |>
  distinct(student_id, .keep_all = T) |>
  group_by(time, student_id, typegpa) |>
  count() |>
  group_by(time,typegpa) |>
  count() |>
  group_by(time) |>
  mutate(share = n/ sum(n)) |>
  ggplot(aes(x = factor(time), y = share, fill = typegpa, label = paste0(round(share,2)))) +
  geom_col(width = 0.5, alpha = 0.8, position = "dodge", color = "black") +
  ylim(c(0,6)) +
  geom_text(size = 3, vjust = -0.75, position = position_dodge(0.6)) +
  labs(x = "", y = "Share of High and Low GPA Students", fill = "GPA Type") +
  scale_x_discrete(breaks = c("1","2","3","4","5")
                   ,labels = labs1)  +
  theme_minimal()

### STUDENT AVERAGE GPAs IN HIGH AND LOW GPA GROUPS

stu_mean_gpa <- avg_fd_im |>
  distinct(student_id, .keep_all = T) |>
  select(time, cumgpa, typegpa) |>
  group_by(time, typegpa) |>
  summarise(avggpa = mean(cumgpa))|>
  ggplot(aes(x = factor(time), y = avggpa, group = typegpa, fill = typegpa,
             label = round(avggpa,2))) +
  geom_col(width = 0.5, alpha = 0.8, position = "dodge", color = "black") +
  ylim(c(0,6)) +
  geom_text(size = 3, vjust = -0.75, position = position_dodge(0.8),
            colour = "black") +
  labs(x = "", y = "Mean GPA of High and Low GPA Students", fill = "GPA Type") +
  scale_x_discrete(breaks = c("1","2","3","4","5"), labels = labs1) +
  theme_minimal()





################################################################################
######################## ECO 1001 GPA across semesters #########################
################################################################################

full_grades <- read_dta("S:\\CUNY Coursework\\Paper2\\Aman_E1001\\Analyses\\grades-by-sem.dta") # s2020 available


full_g <- full_grades |>
  mutate(year = if_else(semester == "S19" | semester == "F19", 2019,
                        if_else(semester == "S20" | semester == "F20", 2020,
                             if_else(semester == "S21" | semester == "F21", 2021, 2022)))) |>
  mutate(session = factor(if_else(semester %in% c("S19","S20","S21","S22"), "S", "F"))) |>
  mutate(session = fct_relevel(session, c("S", "F")))


glimpse(full_g)



  
         
## 376 students withdrew + 39 students withdrew
## 296 students CR
## 55 students NC
## 160 missing coursegpa

##############################################################################
################## Average Course GPA Across Semesters #######################
##############################################################################


gpalabs <- c("Spring 2019","Fall 2019", "Spring 2020" ,"Fall 2020", 
                        "Spring 2021", "Fall 2021", "Spring 2022")
  
full_g |>
  select(year, session, coursegpa) |>
  group_by(year, session) |>
  summarise(avggpa = mean(coursegpa, na.rm = T),
            sd = sd(coursegpa, na.rm = T),
            obs = n()) |>
  ungroup() |>
  mutate(margin.error = 1.96*(sd/sqrt(obs))) |>
  add_column(time = 1:7) |>
  ggplot(aes(x = factor(time), y = avggpa, label = round(avggpa,2))) +
  geom_bar(stat = "identity", position=position_dodge(), width = 0.30, alpha = 0.6) +
  geom_errorbar(aes(ymin=avggpa - margin.error, ymax= avggpa + margin.error), width=.1,
                position=position_dodge(0.6))+
  ylim(c(0,4)) +
  geom_text(size = 3, vjust = -1.5, position = position_dodge(0.25),
            colour = "black") +
  labs(x = "", y = "Average Final GPA in ECO 1001 across Semesters") +
  scale_x_discrete(breaks = c("1","2","3","4","5","6","7")
                   ,labels = gpalabs) +
  theme_minimal()
  


##############################################################################
################## Average Withdrawal Rate Across Semesters ##################
##############################################################################


wlabels <- c("Spring 2019","Fall 2019", "Spring 2020" ,"Fall 2020", 
             "Spring 2021", "Fall 2021", "Spring 2022")

full_g |>
  select(year, session, CR, NC, wd) |>
  group_by(year, session) |>
  summarise(NC = mean(NC, na.rm = T),
            CR = mean(CR, na.rm = T), 
            WD = mean(wd, na.rm = T)) |>
  ungroup() |>
  add_column(time = factor(1:7)) |>
  pivot_longer(cols = c("NC", "CR", "WD"),
               names_to = "measure",
               values_to = "avg") |>
  mutate(avg = 100 * avg) |>
  ggplot(aes(x = factor(time), y = avg, fill = measure, label = paste0(round(avg,2), "%"))) +
  geom_col(alpha = 0.6, width = 0.50) +
  geom_text(size = 2.5, vjust = -2.5, position = position_stack(0.),
            colour = "black") +
  ylim(c(0,50)) +
  labs(x = "", y = "Withdrawal Rate") +
  scale_x_discrete(breaks = c("1","2","3","4","5","6","7")
                   ,labels = wlabels) +
  theme_minimal() +
  theme(legend.title=element_blank())




full_g |>
  select(year, session, wd) |>
  group_by(year, session, wd) |>
  count() |>
  ungroup() |>
  group_by(year, session) |>
  mutate(prop = 100*(n/sum(n))) |>
  ungroup() |>
  filter(wd == 1) |>
  add_column(time = 1:7) |>
  ggplot(aes(x = factor(time), y = prop, label = paste0(round(prop,2), "%"))) +
  geom_col(alpha = 0.6, width = 0.25) +
  geom_text(size = 3, vjust = -0.95, position = position_dodge(0.25),
            colour = "black") +
  ylim(c(0,10)) +
  labs(x = "", y = "Withdrawal Rates in ECO 1001 across Semesters") +
  scale_x_discrete(breaks = c("1","2","3","4","5","6","7")
                   ,labels = wlabels) +
  theme_minimal()
  

##########################################################################
################## Average Exam Scores Across Semesters ##################
##########################################################################



totalscores <- read_rds("S:\\CUNY Coursework\\Paper2\\Aman_E1001\\Data\\total-exam\\3-1-total-scores-all.rds")

labs2 <- c("Spring 2019","Fall 2019", "Fall 2020", 
           "Spring 2021", "Fall 2021", "Spring 2022")
totalscores |>
  select(score, year, session) |>
  mutate(session = fct_relevel(session, c("S","F"))) |>
  group_by(year, session) |>
  add_count(year,session) |>
  summarise(avg = mean(score*0.4), sd = sd(score), n_avg = mean(n),
            std = sd/sqrt(n_avg)) |>
  add_column(time = 1:6) |>
  mutate(margin.error = 1.96*std) |>
  ggplot(aes(x = factor(time), y = avg, label = round(avg,2))) +
  geom_bar(stat = "identity", position=position_dodge(), width = 0.30, alpha = 0.6) +
  geom_errorbar(aes(ymin=avg - margin.error, ymax=avg + margin.error), width=.2,
                position=position_dodge(0.6)) +
  geom_text(size = 3, vjust = -1.5, position = position_dodge(0.25),
            colour = "black") +
  ylim(c(0,75)) +
  labs(x = "", y = "Average Final Exam Scores in ECO 1001 across Semesters") +
  scale_x_discrete(breaks = c("1","2","3","4","5","6")
                   ,labels = labs2) +
  theme_minimal()

#geom_text(size = 3, vjust = -0.75, position = position_dodge(0.65)) +
## 4,737 students
#######################################################################################################
#######################################################################################################
#######################################################################################################

#########################################################







#########################################################################################
#########################################################################################
############################### ONLINE VS HYBRID ########################################
#########################################################################################
#########################################################################################



#### S2019,F2019,F2020,S2021

labs1 <- c("Spring/Fall 2019","Fall 2020", 
           "Spring 2021", "Fall 2021", "Spring 2022")

stu_share_inst <- avg_fd_im |>
  distinct(student_id, .keep_all = T) |>
  group_by(time, student_id, instruction_mode) |>
  count() |>
  group_by(time,instruction_mode) |>
  count()  |>
  group_by(time) |>
  mutate(share = n/ sum(n)) |>
  ggplot(aes(x = factor(time), y = share, fill = instruction_mode, label = paste0(round(share,2)))) +
  geom_col(width = 0.2, alpha = 0.7, position = "dodge", color = "black") +
  ylim(c(0,1)) +
  geom_text(size = 3, vjust = -0.75, position = position_dodge(0.65)) +
  labs(x = "", y = "Share of Students in Hybrid and Online Classes", fill = "Instruction Mode") +
  scale_x_discrete(breaks = c("1","2","3","4","5")
                   ,labels = labs1) +
  theme_minimal()






avg_fd_im |>
  select(session, year, time, instruction_mode, instructor) |>
  group_by(session, year, time, instruction_mode) |>
  count()




avg_fd_im |>
  select(student_id) |>
  distinct()

# Boutalaa Mohamed abd elaziz

#-------------------------------------------------------------------------------

#SET THE DIRECTORY and LOAD THE DATA
load("data/takehome_B.rda")

library(dplyr)
library(ggplot2)
#SET SEED
set.seed(10001)

# II

# 1- Dataset Overview
ls()
str(takehome_health) #to understand the structure
dim(takehome_health) #to know the size

# 2- Checking for Duplicates
sum(duplicated(takehome_health$nhispid)) #counting the number of duplicates in the data
takehome_health <- takehome_health[!duplicated(takehome_health$nhispid), ] #removing the duplicates
sum(duplicated(takehome_health$nhispid)) #checking again if the duplicates are removed

# 3- Examine and Clean Categorical Variables
#Sex variable
table(takehome_health$sex) #Checking sex var
takehome_health$sex <- trimws(takehome_health$sex) #Remove extra spaces
takehome_health$sex <- tolower(takehome_health$sex) #Make all lowercase
table(takehome_health$sex) #Checking again

#Educ variable
table(takehome_health$educ)
takehome_health$educ <- trimws(takehome_health$educ)
takehome_health$educ <- tolower(takehome_health$educ)
table(takehome_health$educ)

#Marstcur
table(takehome_health$marstcur)
takehome_health$marstcur <- trimws(takehome_health$marstcur)
takehome_health$marstcur <- tolower(takehome_health$marstcur)
table(takehome_health$marstcur)

#Empstat
table(takehome_health$empstat) #variable is clean

#Region
table(takehome_health$region)
takehome_health$region <- trimws(takehome_health$region)
takehome_health$region <- tolower(takehome_health$region)
table(takehome_health$region)

# 4- Summary of Missing Values
missing_count <- sapply(takehome_health, function(x) sum(is.na(x)))
missing_percent <- round(missing_count / nrow(takehome_health)*100, 2)
missing_table <- data.frame(
  variable = names(missing_count),
  Missing = missing_count,
  Percent = missing_percent
  )
rownames(missing_table) <- NULL
missing_table <- missing_table[missing_table$Missing > 0,]
missing_table <- missing_table[order(-missing_table$Percent), ]

#Check for Logical Inconsistencies
#checking if there is a pregnant male
table(takehome_health$pregnantnow, takehome_health$sex) #no filtering needed

#checking for working children
table(takehome_health$empstat, useNA = "ifany")
sum(takehome_health$age < 12 & 
      takehome_health$empstat == "Working for pay at job/business", 
    na.rm = TRUE) #no filtering needed

# III
# 1-Healthcare Access Barriers

#Clean variables
takehome_health$sex     <- tolower(trimws(takehome_health$sex))
takehome_health$pooryn  <- tolower(trimws(takehome_health$pooryn))
takehome_health$gotwelf <- tolower(trimws(takehome_health$gotwelf))

#Create age_group variable
takehome_health$age_group <- cut(
  takehome_health$age,
  breaks = c(0, 17, 44, 64, Inf),
  labels = c("0–17", "18–44", "45–64", "65+"),
  right = TRUE
)

#Filter clean dataset
takehome_health_clean <- takehome_health %>%
  filter(
    !is.na(sex), sex %in% c("male", "female"),
    !is.na(pooryn), pooryn %in% c("below poverty threshold", "at or above poverty threshold"),
    !is.na(age_group),
    !is.na(incfam97on2),
    !is.na(gotwelf)
  )

#Grouped summarised table
healthcare_summary <- takehome_health_clean %>%
  group_by(age_group, sex, pooryn, incfam97on2, gotwelf) %>%
  summarise(
    `Doctor_visit (%)`             = round(mean(docvis2w == "Yes", na.rm = TRUE) * 100, 2),
    `Delayed_care (%)`             = round(mean(delaycost == "Yes", na.rm = TRUE) * 100, 2),
    `Dental_care_unaffordable (%)` = round(mean(ybardental == "Yes", na.rm = TRUE) * 100, 2),
    `Prescription_unaffordable (%)` = round(mean(ybarmeds == "Yes", na.rm = TRUE) * 100, 2),
    `Mental_care_unaffordable (%)` = round(mean(ybarmental == "Yes", na.rm = TRUE) * 100, 2),
    `Home_care (%)`                = round(mean(homecare2w == "Yes", na.rm = TRUE) * 100, 2)
  )  %>%
  rename(
    `Poverty Status`   = pooryn,
    `Income Category`  = incfam97on2,
    `Welfare Receipt`  = gotwelf,
    `Gender`           = sex,
    `Age Group`        = age_group
  )

write.csv(healthcare_summary, "healthcare_summary.csv", row.names = FALSE)
getwd()

# 2- Mental Health Access and Utilization
#Clean variables 
takehome_health$sawment <- tolower(trimws(takehome_health$sawment))
takehome_health$deprx   <- tolower(trimws(takehome_health$deprx))
takehome_health$depfreq <- tolower(trimws(takehome_health$depfreq))
takehome_health$worfreq <- tolower(trimws(takehome_health$worfreq))

#
mental_health_clean <- takehome_health %>%
  filter(
    !is.na(sex), sex %in% c("male", "female"),
    !is.na(pooryn), pooryn %in% c("below poverty threshold", "at or above poverty threshold"),
    !is.na(age_group),
    !is.na(incfam97on2),
    !is.na(gotwelf)
  )

mental_health_summary <- mental_health_clean %>%
  group_by(age_group, sex, pooryn, incfam97on2, gotwelf) %>%
  summarise(
    `Saw mental health professional (%)` = round(mean(sawment == "yes", na.rm = TRUE) * 100, 2),
    `Took medication for depression (%)` = round(mean(deprx == "yes", na.rm = TRUE) * 100, 2),
    `Reported depression (%)` = round(mean(depfreq %in% c("daily", "weekly", "monthly", "a few times a year"), na.rm = TRUE) * 100, 2),
    `Reported anxiety (%)`    = round(mean(worfreq %in% c("daily", "weekly", "monthly", "a few times a year"), na.rm = TRUE) * 100, 2)
  ) %>%
  rename(
    `Age Group`         = age_group,
    `Gender`            = sex,
    `Poverty Status`    = pooryn,
    `Income Category`   = incfam97on2,
    `Welfare Receipt`   = gotwelf
  )
write.csv(mental_health_summary, "mental_health_summary.csv", row.names = FALSE)
# 3-Chronic Disease Indicator
#Clean the data
takehome_health$diabeticev <- tolower(trimws(takehome_health$diabeticev))
takehome_health$cheartdiev <- tolower(trimws(takehome_health$cheartdiev))
takehome_health$strokev    <- tolower(trimws(takehome_health$strokev))
takehome_health$asthmaev   <- tolower(trimws(takehome_health$asthmaev))
takehome_health$arthglupev <- tolower(trimws(takehome_health$arthglupev))
takehome_health$cancerev   <- tolower(trimws(takehome_health$cancerev))

#Create indicator: 1 if has at least one chronic illness
takehome_health$chronic_disease <- ifelse(
  takehome_health$diabeticev %in% c("yes"),
  1,
  0
)

#Add others
takehome_health$chronic_disease <- ifelse(
  takehome_health$cheartdiev == "yes" |
    takehome_health$strokev == "yes" |
    takehome_health$asthmaev == "yes" |
    takehome_health$arthglupev == "yes" |
    takehome_health$cancerev == "yes" |
    takehome_health$chronic_disease == 1,
  1,
  0
)

#Filter and group the dataset
chronic_clean <- takehome_health %>%
  filter(
    !is.na(chronic_disease),
    !is.na(age_group),
    !is.na(sex), sex %in% c("male", "female"),
    !is.na(pooryn), pooryn %in% c("below poverty threshold", "at or above poverty threshold"),
    !is.na(incfam97on2),
    !is.na(gotwelf)
  )
#
chronic_summary <- chronic_clean %>%
  group_by(age_group, sex, pooryn, incfam97on2, gotwelf) %>%
  summarise(
    `Chronic Disease Prevalence (%)` = round(mean(chronic_disease == 1) * 100, 2)
  ) %>%
  rename(
    `Age Group`        = age_group,
    `Gender`           = sex,
    `Poverty Status`   = pooryn,
    `Income Category`  = incfam97on2,
    `Welfare Receipt`  = gotwelf
  )

#Test for significance by poverty status
table_cd <- table(
  chronic_clean$pooryn,
  chronic_clean$chronic_disease
)

chisq.test(table_cd)

write.csv(chronic_summary, "chronic_summary.csv", row.names = FALSE)

# V
# 1- Relationship Between Age and Delayed Healthcare

#Clean the data
plot_data1 <- takehome_health %>%
  mutate(
    delaycost = tolower(trimws(delaycost)),
    pooryn = tolower(trimws(pooryn))
  ) %>%
  filter(!is.na(age), !is.na(pooryn), !is.na(delaycost)) %>%
  mutate(delay_binary = ifelse(delaycost == "yes", 1, 0))
# Draw a reproducible sample to reduce computation time for visualization
plot_data_sample <- plot_data1 %>% sample_n(5000)
plot_data_sample_clean <- plot_data1 %>%
  sample_n(5000) %>%
  filter(pooryn %in% c("below poverty threshold", "at or above poverty threshold"))
#Plot
p <- ggplot(plot_data_sample_clean, aes(x = age, y = delay_binary, color = pooryn)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +
  labs(
    title = "Probability of Delaying Care by Age and Poverty Status",
    x = "Age",
    y = "Probability of Delayed Care",
    color = "Poverty Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    plot.title = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )
#Save it
ggsave("fig_delaycare_by_age.png", p, width = 8, height = 5, dpi = 300, bg = "white")

#2- Chronic Disease Prevalence by Age Group and Income Category
# Draw a reproducible sample to reduce computation time for visualization
chronic_sample <- chronic_clean %>%
  sample_n(5000) %>%
  mutate(
    age_group = case_when(
      age < 18 ~ "<18",
      age >= 18 & age <= 44 ~ "18–44",
      age >= 45 & age <= 64 ~ "45–64",
      age >= 65 ~ "65+"
    )
  )
#Summarize chronic disease prevalence by age group and income
chronic_summary <- chronic_sample %>%
  filter(!is.na(age_group), !is.na(incfam97on2)) %>%
  group_by(age_group, incfam97on2) %>%
  summarise(prevalence = mean(chronic_disease, na.rm = TRUE) * 100) %>%
  ungroup()
#Plot
p2 <- ggplot(chronic_summary, aes(x = age_group, y = prevalence, fill = incfam97on2)) +
  geom_col(position = "dodge") +
  labs(
    title = "Chronic Disease Prevalence by Age Group and Income",
    x = "Age Group",
    y = "Prevalence (%)",
    fill = "Income Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    plot.title = element_text(color = "black"),
    legend.title = element_text(color = "black")
    )
#Save it
ggsave("fig_chronic_by_age_income.png",p2, width = 8, height = 5, dpi = 300, bg = "white")

#3- MultiPanel Figure – Healthcare Utilization by Demographics
#Clean the data 
util_data <- takehome_health %>%
  mutate(
    docvis2w = tolower(trimws(docvis2w)),
    docvisit = ifelse(docvis2w == "yes", 1, 0)
  ) %>%
  filter(
    !is.na(sex),
    !is.na(educ),
    !is.na(empstat),
    !is.na(docvisit),
    sex %in% c("male", "female")
  )
#Using a sample
util_sample <- util_data %>% sample_n(5000)
#Summarise by group
util_summary <- util_sample %>%
  group_by(sex, educ, empstat) %>%
  summarise(pct_docvisit = mean(docvisit) * 100) %>%
  ungroup()
#Plot
p3 <- ggplot(util_summary, aes(x = empstat, y = pct_docvisit, fill = sex)) +
  geom_col(position = "dodge") +
  facet_wrap(~ educ, scales = "free_x") +
  scale_fill_manual(values = c("female" = "#F8766D", "male" = "#00BFC4")) +
  labs(
    title = "Doctor Visits by Gender, Education, and Employment",
    x = "Employment Status",
    y = "Percent with Doctor Visit",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white")
  )
#Save it
ggsave("fig_docvis_facet.png", p3, width = 20, height = 12, dpi = 400)

# Install packages
install.packages("openxlsx")
install.packages("gtsummary")

library(openxlsx)
library(dplyr)
library(gtsummary)

# Import dataset
file_path <- " "    # Based on file path
demographics <- read.xlsx(file_path, sheet = "Demographics")
baseline_devices <- read.xlsx(file_path, sheet = "All Baseline+devices")


# ————————————————— Demographic Analysis —————————————————————————————————————
# Define
demographics <- demographics %>%
  filter(!subject %in% c("8-A", "17-A")) %>%
  mutate(
    sex_cat = case_when(
      sex == "Male"   ~ 0,
      sex == "Female" ~ 1,
      TRUE ~ NA_real_),
    
    race_cat = case_when(
      `race/ethnicity` == "White" ~ 0,
      `race/ethnicity` == "White/ Latino" ~ 0,
      `race/ethnicity` == "Black" ~ 1,
      `race/ethnicity` == "Asian" ~ 2,
      TRUE ~ NA_real_),
    
    visual_acuity = case_when(
      visual.acuity == "No vision" ~ 0,
      visual.acuity == "Able to see lights" ~ 1,
      visual.acuity == "Able to see shapes" ~ 2,
      visual.acuity == "Able to read large text" ~ 3,
      TRUE ~ NA_real_))



# Demographic Table
demographic_table <- tbl_summary(
  mutate(
    demographics,
    sex_cat = factor(sex_cat, levels = c(0, 1), labels = c("Male", "Female")),
    race_cat = factor(race_cat, levels = c(0, 1, 2), labels = c("White", "Black", "Asian")),
    visual_acuity = factor(
      visual_acuity,
      levels = c(0, 1, 2, 3),
      labels = c("No vision", "Able to see lights", "Able to see shapes", "Able to read large text")))
  
  [, c("sex_cat", "age", "race_cat", "visual_acuity")],
  statistic = list(
    all_categorical() ~ "{n} ({p}%)",
    age ~ "{median} [{p25}, {p75}]"),
  
  label = list(
    sex_cat    ~ "Sex",
    age        ~ "Age",
    race_cat   ~ "Race/Ethnicity",
    visual_acuity ~ "Visual Acuity"),
  missing = "no")

# Display table
demographic_table
# ————————————————————————————————————————————————————————————————————————————


# ————————————————— Route & Device ————————————————————————————————————————
# Rename
baseline_devices <- baseline_devices %>%
  rename(success = `success.y/n`,
         found_location = `Find.location`,
         time_sec = `time`,
         walk_order = `walkorder`)

# baseline -> Baseline
baseline_devices <- baseline_devices %>%
  mutate(
    device = if_else(tolower(device) == "baseline", "Baseline", device))


# Separate: eg. L10 & L10R in different categories
route_completed_separate <- baseline_devices %>%
  filter(success == "y") %>%
  count(route, name = "n_completed") %>%
  arrange(route) %>%
  rename(
    Route = route,
    `Total Number of Completed Walks` = n_completed)

route_completed_separate



# Combine: eg. L10 & L10R in the same category
route_completed_combined <- baseline_devices %>%
  filter(success == "y") %>%
  mutate(route_group = sub("R$", "", route)) %>%  # remove "R" from route names
  count(route_group, name = "n_completed") %>%
  arrange(route_group) %>%
  rename(
    Route = route_group,
    `Total Number of Completed Walks` = n_completed)

route_completed_combined



# Route-specific completion rates (R routes combined)
specific_completion_rates <- baseline_devices %>%
  mutate(
    route_combined = sub("R$", "", route)) %>%
  group_by(route_combined) %>%
  
  summarise(
    total_walks = n(),
    completed_walks = sum(success == "y", na.rm = TRUE),
    completion_rate = 100 * completed_walks / total_walks,
    .groups = "drop") %>%
  arrange(route_combined) %>%
  
  rename(
    Route = route_combined,
    `Total Walks` = total_walks,
    `Completed Walks` = completed_walks,
    `Completion Rate (%)` = completion_rate) %>%
  mutate(
    `Completion Rate (%)` = round(`Completion Rate (%)`, 1))

specific_completion_rates



# Route-specific completion rates by navigation condition (Baseline vs App-assisted)
route_baseline_vs_app <- baseline_devices %>%
  mutate(
    route_combined = sub("R$", "", route))%>%
  
  group_by(route_combined, device) %>%
  summarise(
    total_walks = n(),
    completed_walks = sum(success == "y", na.rm = TRUE),
    completion_rate = 100 * completed_walks / total_walks,
    .groups = "drop") %>%
  arrange(route_combined, device) %>%
  
  rename(
    Route = route_combined,
    Condition = device,
    `Total Walks` = total_walks,
    `Completed Walks` = completed_walks,
    `Completion Rate (%)` = completion_rate) %>%
  mutate(
    `Completion Rate (%)` = round(`Completion Rate (%)`, 1))

route_baseline_vs_app



# Route-specific completion rates by navigation condition (R routes combined)
specific_completion_by_condition <- baseline_devices %>%
  mutate(
    route_combined = sub("R$", "", route)) %>%

  group_by(route_combined, device) %>%
  summarise(
    total_walks = n(),
    completed_walks = sum(success == "y", na.rm = TRUE),
    completion_rate = 100 * completed_walks / total_walks,
    .groups = "drop") %>%
  arrange(route_combined, device) %>%
  
  rename(
    Route = route_combined,
    `Application Condition` = device,
    `Total Walks` = total_walks,
    `Completed Walks` = completed_walks,
    `Completion Rate (%)` = completion_rate) %>%
  mutate(
    `Completion Rate (%)` = round(`Completion Rate (%)`, 1))

specific_completion_by_condition

# ————————————————————————————————————————————————————————————————————————————


# ———————————————— Variable Definition —————————————————————————————————————————
# Outcome: Success or not (Succee = 1, Fail = 0)
analysis <- baseline_devices
analysis <- analysis %>%
  mutate(completed = if_else(success == "y", 1, 0))
# Quick check
analysis %>% count(completed)


# Covariate: route, walk order, visual acuity
analysis <- analysis %>%
  mutate(route_combined = sub("R$", "", route),
         walk_order = walk_order) %>%
  left_join(
         demographics %>% 
         select(subject, visual_acuity), by = "subject")


# Exposure 1: Baseline vs App-assisted
analysis <- analysis %>%
  mutate(
    application = if_else(device == "Baseline", "Baseline","App-assisted"))
# check
analysis %>% count(device, application)


# Exposure 2: Baseline/Clew/Goodmaps/Navilens

# ————————————————————————————————————————————————————————————————————————————



# ———————————————— Logistic Regression —————————————————————————————————————————
analysis$route_combined <- factor(analysis$route_combined)
analysis$visual_acuity <- factor(analysis$visual_acuity)


# Model for Exposure 1
analysis$application <- factor(analysis$application, levels = c("Baseline", "App-assisted"))

# Logistic Regression
logit_ex1_m1 <- glm(
  completed ~ application, family = binomial(), data = analysis)

logit_ex1_m2 <- glm(
  completed ~ application + route_combined, family = binomial(), data = analysis)

logit_ex1_m3 <- glm(
  completed ~ application + walk_order, family = binomial(), data = analysis)

logit_ex1_m4 <- glm(
  completed ~ application + visual_acuity, family = binomial(), data = analysis)

logit_ex1_m5 <- glm(
  completed ~ application + route_combined + walk_order + visual_acuity,
  family = binomial(), data = analysis)

summary(logit_ex1_m1)
summary(logit_ex1_m2)
summary(logit_ex1_m3)
summary(logit_ex1_m4)
summary(logit_ex1_m5)

# OR + 95% CI
exp(cbind(OR = coef(logit_ex1_m1), confint(logit_ex1_m1)))
exp(cbind(OR = coef(logit_ex1_m2), confint(logit_ex1_m2)))
exp(cbind(OR = coef(logit_ex1_m3), confint(logit_ex1_m3)))
exp(cbind(OR = coef(logit_ex1_m4), confint(logit_ex1_m4)))
exp(cbind(OR = coef(logit_ex1_m5), confint(logit_ex1_m5)))


# Firth Logistic Regression
firth_ex1_m1 <- logistf(completed ~ application, data = analysis)

firth_ex1_m2 <- logistf(
  completed ~ application + route_combined, data = analysis)

firth_ex1_m3 <- logistf(
  completed ~ application + walk_order, data = analysis)

firth_ex1_m4 <- logistf(
  completed ~ application + visual_acuity, data = analysis)

firth_ex1_m5 <- logistf(
  completed ~ application + route_combined + walk_order + visual_acuity,
  data = analysis)

summary(firth_ex1_m1)
summary(firth_ex1_m2)
summary(firth_ex1_m3)
summary(firth_ex1_m4)
summary(firth_ex1_m5)

# OR + 95% CI
exp(cbind(OR = coef(firth_ex1_m1), confint(firth_ex1_m1)))
exp(cbind(OR = coef(firth_ex1_m2), confint(firth_ex1_m2)))
exp(cbind(OR = coef(firth_ex1_m3), confint(firth_ex1_m3)))
exp(cbind(OR = coef(firth_ex1_m4), confint(firth_ex1_m4)))
exp(cbind(OR = coef(firth_ex1_m5), confint(firth_ex1_m5)))



# Model for Exposure 2
analysis$device <- factor(analysis$device, levels = c("Baseline", "Clew", "Goodmaps", "NaviLens"))

# Logistic Regression
logit_ex2_m1 <- glm(completed ~ device, family = binomial(), data = analysis)

logit_ex2_m2 <- glm(completed ~ device + route_combined, family = binomial(), data = analysis)

logit_ex2_m3 <- glm(completed ~ device + walk_order, family = binomial(), data = analysis)

logit_ex2_m4 <- glm(completed ~ device + visual_acuity, family = binomial(), data = analysis)

logit_ex2_m5 <- glm(completed ~ device + route_combined + walk_order + visual_acuity,
                    family = binomial(), data = analysis)

summary(logit_ex2_m1)
summary(logit_ex2_m2)
summary(logit_ex2_m3)
summary(logit_ex2_m4)
summary(logit_ex2_m5)

# OR + 95% CI
exp(cbind(OR = coef(logit_ex2_m1), confint(logit_ex2_m1)))
exp(cbind(OR = coef(logit_ex2_m2), confint(logit_ex2_m2)))
exp(cbind(OR = coef(logit_ex2_m3), confint(logit_ex2_m3)))
exp(cbind(OR = coef(logit_ex2_m4), confint(logit_ex2_m4)))
exp(cbind(OR = coef(logit_ex2_m5), confint(logit_ex2_m5)))


# Firth Logistic Regression
# Exposure 2: Firth Logistic Regression
firth_ex2_m1 <- logistf(completed ~ device, data = analysis)

firth_ex2_m2 <- logistf(completed ~ device + route_combined, data = analysis)

firth_ex2_m3 <- logistf(completed ~ device + walk_order, data = analysis)

firth_ex2_m4 <- logistf(completed ~ device + visual_acuity, data = analysis)

firth_ex2_m5 <- logistf(completed ~ device + route_combined + walk_order + visual_acuity,
                        data = analysis)

summary(firth_ex2_m1)
summary(firth_ex2_m2)
summary(firth_ex2_m3)
summary(firth_ex2_m4)
summary(firth_ex2_m5)

# OR + 95% CI
exp(cbind(OR = coef(firth_ex2_m1), confint(firth_ex2_m1)))
exp(cbind(OR = coef(firth_ex2_m2), confint(firth_ex2_m2)))
exp(cbind(OR = coef(firth_ex2_m3), confint(firth_ex2_m3)))
exp(cbind(OR = coef(firth_ex2_m4), confint(firth_ex2_m4)))
exp(cbind(OR = coef(firth_ex2_m5), confint(firth_ex2_m5)))

# ————————————————————————————————————————————————————————————————————————————

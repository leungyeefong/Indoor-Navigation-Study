# Install packages
install.packages("openxlsx")
install.packages("gtsummary")
install.packages("logistf") 


library(dplyr)
library(gtsummary)
library(logistf)
library(openxlsx)
library(tibble)
library(tidyr)
library(emmeans)

# Import dataset
file_path <- " "  # Based on the file path
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


# Combine: eg. S2 & S2R in the same category, except for L10 and L10R
make_route_group <- function(route) {
  ifelse(route == "L10R", "L10R", sub("R$", "", route))}


route_completed_combined <- baseline_devices %>%
  filter(success == "y") %>%
  mutate(route_group = make_route_group(route)) %>%
  count(route_group, name = "n_completed") %>%
  arrange(route_group) %>%
  rename(
    Route = route_group,
    `Total Number of Completed Walks` = n_completed)

route_completed_combined



# Route-specific completion rates (R routes combined)
specific_completion_rates <- baseline_devices %>%
  mutate(route_group = make_route_group(route)) %>%
  group_by(route_group) %>%
  
  summarise(
    total_walks = n(),
    completed_walks = sum(success == "y", na.rm = TRUE),
    completion_rate = 100 * completed_walks / total_walks,
    .groups = "drop" ) %>%
  arrange(route_group) %>%
  
  rename(
    Route = route_group,
    `Total Walks` = total_walks,
    `Completed Walks` = completed_walks,
    `Completion Rate (%)` = completion_rate) %>%
  mutate(
    `Completion Rate (%)` = round(`Completion Rate (%)`, 1))

specific_completion_rates



# Route-specific completion rates by navigation condition (R routes combined)
specific_completion_by_condition <- baseline_devices %>%
  mutate(route_group = make_route_group(route)) %>%
  
  group_by(route_group, device) %>%
  summarise(
    total_walks = n(),
    completed_walks = sum(success == "y", na.rm = TRUE),
    completion_rate = 100 * completed_walks / total_walks,
    .groups = "drop") %>%
  arrange(route_group, device) %>%
  
  rename(
    Route = route_group,
    `Application Condition` = device,
    `Total Walks` = total_walks,
    `Completed Walks` = completed_walks,
    `Completion Rate (%)` = completion_rate) %>%
  
  mutate(
    `Completion Rate (%)` = round(`Completion Rate (%)`, 1))

specific_completion_by_condition

# ————————————————————————————————————————————————————————————————————————————



# ———————————————— Analysis ——————————————————————————————————————————————————
analysis <- baseline_devices %>%
  mutate(
    completed = if_else(success == "y", 1, 0),
    route_group = make_route_group(route),
    walk_order = as.integer(walk_order),
    device = factor(device,
                    levels = c("Baseline","Goodmaps","Clew","NaviLens"))) %>%
  left_join(
    demographics %>%
      select(subject, age, sex_cat, race_cat, visual_acuity),
    by = "subject") %>%
  mutate(
    sex_cat = factor(sex_cat, levels = c(0,1),
                     labels = c("Male","Female")),
    race_cat = factor(race_cat, levels = c(0,1,2),
                      labels = c("White","Black","Asian")),
    visual_acuity = factor(
      visual_acuity,
      levels = c(0,1,2,3),
      labels = c("No vision",
                 "Able to see lights",
                 "Able to see shapes",
                 "Able to read large text"),
      ordered = TRUE))

# Quick check
analysis %>% count(route_group, device)




# Fully adjusted Firth
# Route included in models
fit_firth_main <- function(dat, ref_level){
  dat <- dat %>% mutate(device = relevel(device, ref = ref_level))
  logistf(
    completed ~ device + route_group + walk_order + visual_acuity + age + sex_cat + race_cat,
    data = dat)}


fit_baseline <- fit_firth_main(analysis, "Baseline")
fit_goodmaps <- fit_firth_main(analysis, "Goodmaps")
fit_clew     <- fit_firth_main(analysis, "Clew")


extract_device_or <- function(fit, model_name){
  
  tibble::tibble(
    term  = names(fit$coefficients),
    OR    = sprintf("%.2f", exp(fit$coefficients)),
    LCI   = sprintf("%.2f", exp(fit$ci.lower)),
    UCI   = sprintf("%.2f", exp(fit$ci.upper)),
    p     = signif(fit$prob, 3),
    model = model_name) |>
    dplyr::filter(grepl("^device", term))}


all_device_results <- bind_rows(
  extract_device_or(fit_baseline, "Baseline ref"),
  extract_device_or(fit_goodmaps, "Goodmaps ref"),
  extract_device_or(fit_clew, "Clew ref"))

all_device_results




# Interaction model (device-by-route): used to generate route-specific adjusted ORs
analysis <- analysis %>% mutate(device = relevel(device, ref = "Baseline"))


fit_interaction <- logistf(
  completed ~ device * route_group + walk_order + visual_acuity + age + sex_cat + race_cat,
  data = analysis)

# Inspect interaction terms (device:route_group) and overall model output
summary(fit_interaction)



# Set up a function
fmt_or_ci <- function(or, lcl, ucl) sprintf("%.2f (%.2f, %.2f)", or, lcl, ucl)
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))



# Table 1: Route-specific adjusted ORs (each app vs Baseline), from interaction model
emm <- emmeans(fit_interaction, ~ device | route_group)

vs_baseline <- contrast(
  emm,
  method = "trt.vs.ctrl",
  ref = "Baseline",
  adjust = "none")

tab_vs_baseline <- summary(vs_baseline, type = "response", infer = TRUE) %>%
  as.data.frame() %>%
  transmute(
    Route = route_group,
    Comparison = contrast,
    OR_CI = fmt_or_ci(odds.ratio, lower.CL, upper.CL),
    p = fmt_p(p.value)) %>%
  
  tidyr::separate(Comparison, into = c("App", "Baseline"), sep = " - ", remove = FALSE) %>%
  select(Route, App, OR_CI, p) %>%
  tidyr::pivot_wider(
    names_from = App,
    values_from = c(OR_CI, p),
    names_glue = "{App}_{.value}") %>%
  arrange(Route)

tab_vs_baseline




# Table 2: Route-specific adjusted ORs (app-to-app comparisons only; Baseline comparisons removed)
# Use revpairwise to keep comparison direction consistent; then drop any Baseline contrasts
pairwise <- contrast(emm, method = "revpairwise", adjust = "none")

tab_pairwise <- summary(pairwise, type="response", infer=TRUE) %>%
  as.data.frame() %>%
  filter(!grepl("Baseline", contrast)) %>%
  transmute(
    Route = route_group,
    Comparison = gsub(" - ", " vs ", contrast),
    OR_CI = sprintf("%.2f (%.2f, %.2f)", odds.ratio, lower.CL, upper.CL),
    p = fmt_p(p.value)) %>%
  arrange(Route)

tab_pairwise

# optional
tab_pairwise_wide <- tab_pairwise %>%
  tidyr::pivot_wider(names_from = Comparison, values_from = c(OR_CI, p))

tab_pairwise_wide

# ————————————————————————————————————————————————————————————————————————————

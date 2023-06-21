#lOAD libraries (unsure about what each library do, will specify)
library(tidyverse)
library(tidyr)
library(haven) 
library(dplyr)
library(gdata) #remane.vars
library(data.table) #sprsplit & table 
library(here)
library(renv)
library(data.table) 
library(ggplot2)

theme_set(theme_bw())
theme_set(theme_classic())


# Load Fertility Patients (FP) dataset.
fertility_patients = here::here("Fertility_probability_judgement/Dataset/fp_taska_cleansed.csv")
fp = read_csv(fertility_patients)

# Load Fertility Health Care Professional (HCP) dataset.
fertility_doctors = here::here("Fertility_probability_judgement/Dataset/hcp_taska_cleansed.csv")
hcp = read_csv(fertility_doctors)

# INCORRECT coding in this data set. 
# Organize datasets.
colnames(fp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_fp
fp <- fp_all %>%
  select(task_a_columns_fp, item_string)  %>%
  mutate(id = row_number())

colnames(hcp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_hcp
hcp <- hcp_all %>%
  select(task_a_columns_hcp, item_string)  %>%
  mutate(id = row_number())

# Descriptive analysis.
fp <- fp[sapply(fp, is.numeric)]  
fp_a_before_sd <- apply(fp, 2, sd, na.rm=TRUE)
fp_a_before_mean <- apply(fp, 2, mean, na.rm=TRUE)
fp_a_before_median <- apply(fp, 2, median, na.rm=TRUE)

fp_taska  <- fp_taska[sapply(fp_taska, is.numeric)] 
fp_a_after_sd <- apply(fp_taska, 2, sd, na.rm=TRUE)
fp_a_after_mean <- apply(fp_taska, 2, mean, na.rm=TRUE)
fp_a_after_median <- apply(fp_taska, 2, median, na.rm=TRUE)

# Descriptive analysis.
hcp <- hcp[sapply(hcp, is.numeric)]  
hcp_a_before_sd <- apply(hcp, 2, sd, na.rm=TRUE)
hcp_a_before_mean <- apply(hcp, 2, mean, na.rm=TRUE)
hcp_a_before_median <- apply(hcp, 2, median, na.rm=TRUE)

hcp_taska  <- hcp_taska[sapply(hcp_taska, is.numeric)] 
hcp_a_after_sd <- apply(hcp_taska, 2, sd, na.rm=TRUE)
hcp_a_after_mean <- apply(hcp_taska, 2, mean, na.rm=TRUE)
hcp_a_after_median <- apply(hcp, 2, median, na.rm=TRUE)

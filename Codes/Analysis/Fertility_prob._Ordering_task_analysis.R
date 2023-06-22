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
library(ggridges)
library(PlackettLuce)

theme_set(theme_bw())
theme_set(theme_classic())

fertility_patient = here::here("Fertility_probability_judgement/Dataset/FP_n281.sav")
fp_all = haven::read_sav(fertility_patient)
fertility_doctors = here::here("Fertility_probability_judgement/Dataset/HCP_n263.sav")
hcp_all = haven::read_sav(fertility_doctors)

# Load clean data sets. 
fertility_patient_clean = here::here("Fertility_probability_judgement/Dataset/fp_taska.csv")
fp_ta = read.csv(fertility_patient_clean)

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

# Plackett_Luce model analysis OLD data sets
colnames(fp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_fp
fp_all1 <- fp_all %>%
  select(task_a_columns_fp, item_string)  %>%
  mutate(id = row_number())

# Task A inferential analysis.
col_order_fp <- c("task_a_almost_certainly", "task_a_highly_likely",   
                  "task_a_very_good_chance",  "task_a_likely", 
                  "task_a_probable", "task_a_probably",
                  "task_a_we_believe", "task_a_better_than_even",
                  "task_a_about_even", "task_a_chances_are_slight",
                  "task_a_we_doubt", "task_a_little_chance",
                  "task_a_probably_not", "task_a_unlikely",
                  "task_a_improbable",  "task_a_highly_unlikely",
                  "task_a_almost_no_chance")

fp_ranked <- fp_all1[, col_order_fp] %>% 
  mutate(id = row_number())
fp_long <- pivot_longer(fp_ranked, cols = c(1:17), names_to ="term", values_to = "ranking") 

fp_pl_format <- pivot_wider(fp_long, names_from = "ranking", values_from = "term") %>%
  select(-id)
order <- c("1","2","3","4","5","6","7","8","9","10","11","12",
           "13","14","15","16","NA")
fp_pl_format <- fp_pl_format[, order] 

fp_pl_format %>%
  select(-"NA") ->fp_pl_format

fp_pl_ranked <- as.rankings(fp_pl_format, input = "orderings")

pmodel_fp <- PlackettLuce(fp_pl_ranked, npseudo = 0) 
qv_fp <- qvcalc(pmodel_fp)
qv_fp$qvframe <- qv_fp$qvframe[order(coef(pmodel_fp)),]

# <Figure 8>: Rankings of terms given by Plackettluce model (patients).
fp_all1 <- as.data.frame(apply(fp_all1, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)
fp_all1 %>%
  select(-id) %>% 
  colMeans(na.rm = TRUE) %>% 
  sort() %>% 
  names() %>%
  substring(8,100) %>%
  str_replace("_", " ")-> term_order_fp
plot(qv_fp,  
     main = "Rankings of terms given by Plackettluce (patients)",
     xaxt="n", xlim = c(1, 17), ylim = c(-6,6))
axis(1, at = seq_len(17), labels = term_order_fp, las = 2, cex.axis = 0.6)
coefs_pl_fp <- round(coef(pmodel_fp), 2) %>% sort()



# Plackett_Luce model analysis CLEAN data sets
# Task A inferential analysis.
col_order_fp <- c("task_a_almost_certainly", "task_a_highly_likely",   
                  "task_a_very_good_chance",  "task_a_likely", 
                  "task_a_probable", "task_a_probably",
                  "task_a_we_believe", "task_a_better_than_even",
                  "task_a_about_even", "task_a_chances_are_slight",
                  "task_a_we_doubt", "task_a_little_chance",
                  "task_a_probably_not", "task_a_unlikely",
                  "task_a_improbable",  "task_a_highly_unlikely",
                  "task_a_almost_no_chance")

fp_ranked <- fp_ta[, col_order_fp] %>% 
  mutate(id = row_number())
fp_long <- pivot_longer(fp_ranked, cols = c(1:17), names_to ="term", values_to = "ranking") 

fp_pl_format <- pivot_wider(fp_long, names_from = "ranking", values_from = "term") %>%
  select(-id)
order <- c("1","2","3","4","5","6","7","8","9","10","11","12",
           "13","14","15","16","NA")
fp_pl_format <- fp_pl_format[, order] 

fp_pl_format %>%
  select(-"NA") ->fp_pl_format

fp_pl_ranked <- as.rankings(fp_pl_format, input = "orderings")

pmodel_fp <- PlackettLuce(fp_pl_ranked, npseudo = 0) 
qv_fp <- qvcalc(pmodel_fp)
qv_fp$qvframe <- qv_fp$qvframe[order(coef(pmodel_fp)),]

# <Figure 8>: Rankings of terms given by Plackettluce model (patients).
fp_ta <- as.data.frame(apply(fp_ta, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)
fp_ta %>%
  select(-id, -item_string) %>% 
  colMeans(na.rm = TRUE) %>% 
  sort() %>% 
  names() %>%
  substring(8,100) %>%
  str_replace("_", " ")-> term_order_fp
plot(qv_fp,  
     main = "Rankings of terms given by Plackettluce (patients)",
     xaxt="n", xlim = c(1, 17), ylim = c(-6,6))
axis(1, at = seq_len(17), labels = term_order_fp, las = 2, cex.axis = 0.6)
coefs_pl_fp <- round(coef(pmodel_fp), 2) %>% sort()

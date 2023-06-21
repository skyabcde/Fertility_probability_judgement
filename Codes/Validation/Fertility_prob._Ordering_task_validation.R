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
fertility_patient = here::here("Fertility_probability_judgement/Dataset/FP_n281.sav")
fp_all = haven::read_sav(fertility_patient)

# Load Fertility Health Care Professional (HCP) dataset.
fertility_doctors = here::here("Fertility_probability_judgement/Dataset/HCP_n263.sav")
hcp_all = haven::read_sav(fertility_doctors)

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

# Pairwise correlation between individual participant given their ordered ranking, FP. 
colnames(fp) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_cleanfp

expand.grid(id1 = fp$id, id2 = fp$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp[fp$id == id1, task_a_columns_cleanfp])
      id2_responses = unlist(fp[fp$id == id2, task_a_columns_cleanfp])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> task_a_pairwise_fp

task_a_pairwise_fp %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> task_a_avr_cor_fp

# Pairwise ordinal correlations graph, FP. 
task_a_pairwise_fp %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = task_a_avr_cor_fp$id1, ordered = TRUE),
    id2 = factor(id2, levels = task_a_avr_cor_fp$id1, ordered = TRUE)
    ) -> task_a_pairwise_fp_ordered

task_a_pairwise_fp_ordered %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations between patients"))

# Pairwise correlation between individual participant given their ordered ranking, HCP. 
colnames(hcp) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns_cleanhcp

expand.grid(id1 = hcp$id, id2 = hcp$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp[hcp$id == id1, task_a_columns_cleanhcp])
      id2_responses = unlist(hcp[hcp$id == id2, task_a_columns_cleanhcp])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> task_a_pairwise_hcp

task_a_pairwise_hcp %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> task_a_avr_cor_hcp

# Pairwise ordinal correlations graph, HCP. 
task_a_pairwise_hcp %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = task_a_avr_cor_hcp$id1, ordered = TRUE),
    id2 = factor(id2, levels = task_a_avr_cor_hcp$id1, ordered = TRUE)
  ) -> task_a_pairwise_hcp_ordered

task_a_pairwise_hcp_ordered %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations between doctors"))

# The correlation between participant's ordering and original item strings ordering, FP.
data.frame(id1 = fp$id) %>% 
  mutate(
    item_string_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(fp[fp$id == id1, task_a_columns_cleanfp]) 
      if(all(is.na(a))) return(NA)
      id1_responses = a %>% 
        sort() %>%
        names() %>%
        stringr::str_remove(pattern = 'task_a_') %>%
        stringr::str_replace_all(
          pattern = '_', replacement = ' '
        )
      item_string_order = strsplit(fp[fp$id == id1,]$item_string,split = ';') [[1]]
      cor(1:16, match(table = id1_responses, item_string_order), method = 'k')
    })) -> task_a_ori_cor_fp 

# Correlation between average and original graph, FP. 
task_a_avr_cor_fp %>% 
  left_join(task_a_ori_cor_fp, by="id1") -> task_a_avr_ori_fp 
task_a_avr_ori_fp %>% ggplot(aes(mean_cor, item_string_cor)) +
  geom_point()
# BACKGROUND, x/y axis name, SORTING NEEDED. 

# The correlation between participant's ordering and original item strings ordering, HCP.
data.frame(id1 = hcp$id) %>% 
  mutate(
    item_string_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(hcp[hcp$id == id1, task_a_columns_cleanhcp]) 
      if(all(is.na(a))) return(NA)
      id1_responses = a %>% 
        sort() %>%
        names() %>%
        stringr::str_remove(pattern = 'task_a_') %>%
        stringr::str_replace_all(
          pattern = '_', replacement = ' '
        )
      item_string_order = strsplit(hcp[hcp$id == id1,]$item_string,split = ';') [[1]]
      cor(1:16, match(table = id1_responses, item_string_order), method = 'k')
    })) -> task_a_ori_cor_hcp 

# Correlation between average and original graph, HCP. 
task_a_avr_cor_hcp %>% 
  left_join(task_a_ori_cor_hcp, by="id1") -> task_a_avr_ori_hcp
task_a_avr_ori_hcp %>% ggplot(aes(mean_cor, item_string_cor)) +
  geom_point() 
# BACKGROUND, x/y axis name, SORTING NEEDED. 

# The correlation between each participant's ordering and the centroid participant's ordering, FP.
expand.grid(id1 = fp$id, id2 = 14) %>%
  mutate(
    centroid_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp[fp$id == id1, task_a_columns_cleanfp])
      id2_responses = unlist(fp[fp$id == id2, task_a_columns_cleanfp])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(centroid_cor) 
  ) -> task_a_centroid_cor_fp

# Correlation between average and centroid graph, FP. 
task_a_centroid_cor_fp %>% 
  left_join(task_a_ori_cor_fp, by="id1") -> task_a_avr_centroid_fp
task_a_avr_centroid_fp %>% ggplot(aes(centroid_cor, item_string_cor)) +
  geom_point()

# The correlation between each participant's ordering and the centroid participant's ordering, HCP.
expand.grid(id1 = hcp$id, id2 = 33) %>% # Create a data frame that contains all combinations of IDs
  mutate(
    centroid_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp[hcp$id == id1, task_a_columns_cleanhcp])
      id2_responses = unlist(hcp[hcp$id == id2, task_a_columns_cleanhcp])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(centroid_cor) 
  ) -> task_a_centroid_cor_hcp

# Correlation between average and centroid graph, HCP. 
task_a_centroid_cor_hcp %>% 
  left_join(task_a_ori_cor_hcp, by="id1") -> task_a_avr_centroid_hcp
task_a_avr_centroid_hcp %>% ggplot(aes(centroid_cor, item_string_cor)) +
  geom_point()


# Verification. 
fertility_patient = here::here(".git/Dataset/fp_taska.csv")
fp_taska <- read.csv(fertility_patient)

colnames(fp_taska) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns
expand.grid(id1 = fp_taska$id, id2 = fp_taska$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp_taska[fp_taska$id == id1, task_a_columns])
      id2_responses = unlist(fp_taska[fp_taska$id == id2, task_a_columns])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> cor_all_taska

cor_all_taska %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> cor_ave_taska 

cor_all_taska %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = cor_ave_taska$id1, ordered = TRUE),
    id2 = factor(id2, levels = cor_ave_taska$id1, ordered = TRUE)
  ) -> cor_all_taska

cor_all_taska %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations between patients"))

# Descriptive analysis.
fp <- fp[sapply(fp, is.numeric)]  
fp_a_before_sd <- apply(fp, 2, sd, na.rm=TRUE)
fp_a_before_mean <- apply(fp, 2, mean, na.rm=TRUE)
fp_a_before_median <- apply(fp, 2, median, na.rm=TRUE)

fp_taska  <- fp_taska[sapply(fp_taska, is.numeric)] 
fp_a_after_sd <- apply(fp_taska, 2, sd, na.rm=TRUE)
fp_a_after_mean <- apply(fp_taska, 2, mean, na.rm=TRUE)
fp_a_after_median <- apply(fp_taska, 2, median, na.rm=TRUE)

#----------------------------------------------------------------------------------------------------




# Save the cleansed task A data from doctors. 
write.csv(hcp_taska, ".git/Dataset/hcp_taska.csv")

fertility_doctors = here::here(".git/Dataset/hcp_taska.csv")
hcp_taska <- read.csv(fertility_doctors)

colnames(hcp_taska) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns
expand.grid(id1 = hcp_taska$id, id2 = hcp_taska$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp_taska[hcp_taska$id == id1, task_a_columns])
      id2_responses = unlist(hcp_taska[hcp_taska$id == id2, task_a_columns])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(cor) 
  ) -> cor_all_taska

cor_all_taska %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  summarise(mean_cor = mean(cor)) %>%
  arrange(mean_cor) %>% 
  mutate(rank_cor = row_number()) -> cor_ave_taska 

cor_all_taska %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = cor_ave_taska$id1, ordered = TRUE),
    id2 = factor(id2, levels = cor_ave_taska$id1, ordered = TRUE)
  ) -> cor_all_taska

cor_all_taska %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Pairwise ordinal correlations between doctors"))

# Descriptive analysis.
hcp <- hcp[sapply(hcp, is.numeric)]  
hcp_a_before_sd <- apply(hcp, 2, sd, na.rm=TRUE)
hcp_a_before_mean <- apply(hcp, 2, mean, na.rm=TRUE)
hcp_a_before_median <- apply(hcp, 2, median, na.rm=TRUE)

hcp_taska  <- hcp_taska[sapply(hcp_taska, is.numeric)] 
hcp_a_after_sd <- apply(hcp_taska, 2, sd, na.rm=TRUE)
hcp_a_after_mean <- apply(hcp_taska, 2, mean, na.rm=TRUE)
hcp_a_after_median <- apply(hcp, 2, median, na.rm=TRUE)

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

#----------------------------------------------------------------------------------------------------
# Fertililty patients dataset.
fertility_patients_taskbonly = here::here("Dataset/fp_taskb.csv")
fp_taskb <- read.csv(fertility_patients_taskbonly)
fertility_patient_taskaonly = here::here("Dataset/fp_taska.csv")
fp_onlyA = read.csv(fertility_patient_taskaonly)

# Task B
fp_taskb <- as.data.frame(apply(fp_taskb, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

# Pairwise correlation test between participants in Task B.
expand.grid(id1=fp_taskb$id, id2=fp_taskb$id) %>%
  mutate(cor = purrr::map2_dbl(id1, id2, function(id1,id2){
    id1_rating=unlist(fp_taskb[fp_taskb$id == id1,])
    id2_rating=unlist(fp_taskb[fp_taskb$id == id2,])
    cor(id1_rating, id2_rating, method = 'k', use = 'pairwise')
  })) %>%
  filter(id1 != id2) -> cor_taskb

cor_taskb %>%  filter(cor != 0) -> cor_taskb

cor_taskb %>%
  group_by(id1) %>% 
  summarise(mean_cor_b = mean(cor)) %>% 
  arrange(mean_cor_b) %>%
  mutate(rank_cor_b = row_number()) -> cor_mean_taskb_ranked

# Plotting
cor_taskb %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = cor_mean_taskb_ranked$id1, ordered = TRUE),
    id2 = factor(id2, levels = cor_mean_taskb_ranked$id1, ordered = TRUE)
  ) -> cor_taskb1

cor_taskb1 %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = cor)) + 
  geom_contour_filled() +
  scale_x_continuous(name = "x", expand = c(0,0)) +
  scale_y_continuous(name = "y", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") + 
  ggtitle(ggtitle(label = "Task B Rating Pairwise Ordinal Correlations"))

# Correlation test between the best participant and all other participants in Task B.
expand.grid(id1 = fp_taskb$id, id2 = 243) %>% # Create a data frame that contains all combinations of IDs
  mutate(
    best_cor_b = purrr::map2_dbl(id2, id1, function(id2, id1){
      id1_responses = unlist(fp_taskb[fp_taskb$id == id2,])
      id2_responses = unlist(fp_taskb[fp_taskb$id == id1,])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(best_cor_b)
  ) -> cor_best_taskb

# Task A
colnames(fp_onlyA) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns
fp_onlyA <- as.data.frame(apply(fp_onlyA, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

# Pairwise correlation test between participants in Task A.
expand.grid(id1 = fp_onlyA$id, id2 = fp_onlyA$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp_onlyA[fp_onlyA$id == id1, task_a_columns])
      id2_responses = unlist(fp_onlyA[fp_onlyA$id == id2, task_a_columns])
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

# Correlation test between the participant and their original item string in Task A.
data.frame(id1 = fp_onlyA$id) %>% 
  mutate(
    item_string_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(fp_onlyA[fp_onlyA$id == id1, task_a_columns]) 
      if(all(is.na(a))) return(NA)
      id1_responses = a %>% 
        sort() %>%
        names() %>%
        stringr::str_remove(pattern = 'task_a_') %>%
        stringr::str_replace_all(
          pattern = '_', replacement = ' '
        )
      item_string_order = strsplit(fp_onlyA[fp_onlyA$id == id1,]$item_string,split = ';') [[1]]
      cor(1:16, match(table = id1_responses, item_string_order), method = 'k')
    })) -> cors_item_taska

# Correlation test between the best participant and all other participants in Task A.
expand.grid(id1 = fp_onlyA$id, id2 = 14) %>% # Create a data frame that contains all combinations of IDs
  mutate(
    best_cor_a = purrr::map2_dbl(id2, id1, function(id2, id1){
      id1_responses = unlist(fp_onlyA[fp_onlyA$id == id2,])
      id2_responses = unlist(fp_onlyA[fp_onlyA$id == id1,])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(best_cor_a) 
  ) -> cor_best_taska

# Comparison between Task A&B.
cor_mean_taskb_ranked %>%
  left_join(cor_ave_taska #from task A
            , by="id1") -> cor_mean_ab
cor_best_taskb %>%
  left_join(cor_best_taska, by="id1") -> cor_best_ab

cor_mean_ab %>% 
  mutate(newcol = case_when(
    is.na(mean_cor) ~ 1.1,
    TRUE ~ mean_cor
  )) -> cor_mean_ab

cor_mean_ab %>%
  ggplot(aes(mean_cor_b,newcol)) +
  geom_point() +
  labs(title = "FP Comparison between Mean Correlations between Task AB",
       y = "Task A Mean Correlation", x = "Task B Mean Correlation") 

cor_best_ab %>% 
  mutate(newcol = case_when(
    is.na(best_cor_a) ~ 1.1,
    TRUE ~ best_cor_a
  )) -> cor_best_ab

cor_best_ab %>%
  ggplot(aes(best_cor_b, newcol)) +
  geom_point() +
  labs(title = "FP Comparison between the Correlations between each participant and the best participant between Task AB",
       y = "Task A Best Correlation", x = "Task B Best Correlation") 

cor_best_ab %>%
  filter(cor_best_ab$best_cor_a != "NA") %>%
  select(-id1.y, -id2.y, -newcol) -> cor_best_ab

write.csv(cor_best_ab, "Dataset/fp_cor_taskab.csv", row.names=FALSE)

#----------------------------------------------------------------------------------------------------
# Health care professional dataset.
fertility_doctors_taskbonly = here::here("Dataset/hcp_taskb.csv")
hcp_taskb <- read.csv(fertility_doctors_taskbonly)
fertility_doctors_taskaonly = here::here("Dataset/hcp_taska.csv")
hcp_taska = read.csv(fertility_doctors_taskaonly)

# Task B
hcp_taskb <- as.data.frame(apply(hcp_taskb, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

# Pairwise correlation test between participants in Task B.
expand.grid(id1=hcp_taskb$id, id2=hcp_taskb$id) %>%
  mutate(cor = purrr::map2_dbl(id1, id2, function(id1,id2){
    id1_rating=unlist(hcp_taskb[hcp_taskb$id == id1,])
    id2_rating=unlist(hcp_taskb[hcp_taskb$id == id2,])
    cor(id1_rating, id2_rating, method = 'k', use = 'pairwise')
  })) %>%
  filter(id1 != id2) -> cor_all_taskb

cor_all_taskb %>%
  filter(cor!=0)%>%
  group_by(id1) %>% 
  summarise(mean_cor_b = mean(cor)) %>% 
  arrange(mean_cor_b) %>%
  mutate(rank_cor_b = row_number()) -> cor_ave_taskb

# Correlation test between the best participant and all other participants in Task B
expand.grid(id1 = hcp_taskb$id, id2 = 241) %>% 
  mutate(
    best_cor_b = purrr::map2_dbl(id2, id1, function(id2, id1){
      id1_responses = unlist(hcp_taskb[hcp_taskb$id == id2,])
      id2_responses = unlist(hcp_taskb[hcp_taskb$id == id1,])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(best_cor_b)
  ) -> cor_best_taskb

# Task A
hcp_onlyA %>% select(-X) -> hcp
colnames(hcp) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns

# Pairwise correlation test between participants in Task A.
expand.grid(id1 = hcp$id, id2 = hcp$id) %>% 
  mutate(
    cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp[hcp$id == id1, task_a_columns])
      id2_responses = unlist(hcp[hcp$id == id2, task_a_columns])
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

# Correlation test between the each participant and their original item string in Task A.
data.frame(id1 = hcp$id) %>% 
  mutate(
    item_string_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(hcp[hcp$id == id1, task_a_columns]) 
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
    })) -> cors_item_taska 

# Correlation test between the best participant and all other participants in Task B.
expand.grid(id1 = hcp$id, id2 = 44) %>% 
  mutate(
    best_cor_a = purrr::map2_dbl(id2, id1, function(id2, id1){
      id1_responses = unlist(hcp[hcp$id == id2, task_a_columns])
      id2_responses = unlist(hcp[hcp$id == id1, task_a_columns])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(best_cor_a) 
  ) -> cor_best_taska

# Comparison between task A&B.
cor_ave_taskb %>%
  left_join(cor_ave_taska 
            , by="id1") -> cor_mean_ab
cor_ave_taskb %>%
  left_join(cors_item_taska 
            , by="id1") -> cor_meanb_itema
cor_best_taskb %>%
  left_join(cor_best_taska, by="id1") -> cor_best_ab

cor_mean_ab %>% 
  mutate(newcol = case_when(
    is.na(mean_cor) ~ 1.1,
    TRUE ~ mean_cor
  )) -> cor_mean_ab

cor_mean_ab %>%
  ggplot(aes(mean_cor_b,newcol)) +
  geom_point() +
  labs(title = "HCP Comparison between Mean Correlations between Task AB",
       y = "Task A Mean Correlation", x = "Task B Mean Correlation") 

cor_meanb_itema %>% 
  mutate(newcol = case_when(
    is.na(item_string_cor) ~ 1.1,
    TRUE ~ item_string_cor
  )) -> cor_meanb_itema

cor_meanb_itema %>%
  ggplot(aes(mean_cor_b,newcol)) +
  geom_point() +
  labs(title = "HCP Comparison between Mean Correlation and Item Correlation between Task AB",
       y = "Task A Item Correlation", x = "Task B Mean Correlation")

cor_best_ab %>% 
  mutate(newcol = case_when(
    is.na(best_cor_a) ~ 1.1,
    TRUE ~ best_cor_a
  )) -> cor_best_ab

cor_best_ab %>%
  ggplot(aes(best_cor_b, newcol)) +
  geom_point() +
  labs(title = "HCP Comparison between the Correlations between each participant and the best participant between Task AB",
       # subtitle = "(the mean correlation refers to the cor between each answer and all other answers)",
       y = "Task A Best Correlation", x = "Task B Best Correlation") 

cor_best_ab %>%
  filter(cor_best_ab$best_cor_a != "NA") %>%
  select(-id1.y, -id2.y, -newcol) -> cor_best_ab

write.csv(cor_best_ab, "Dataset/hcp_cor_taskab.csv", row.names=FALSE)
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
fertility_patient_taskaonly = here::here(".git/Dataset/fp_taska.csv")
fp_taska <- read.csv(fertility_patient_taskaonly)
fertility_patients_taskbonly = here::here(".git/Dataset/fp_taskb.csv")
fp_taskb <- read.csv(fertility_patients_taskbonly)

# Task A
colnames(fp_taska) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns
fp_taska <- as.data.frame(apply(fp_taska, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

# Correlation test between the best participant and all other participants in Task A.
expand.grid(id1 = fp_taska$id, id2 = 14) %>% # Create a data frame that contains all combinations of IDs
  mutate(
    best_cor_a = purrr::map2_dbl(id2, id1, function(id2, id1){
      id1_responses = unlist(fp_taska[fp_taska$id == id2,])
      id2_responses = unlist(fp_taska[fp_taska$id == id1,])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(best_cor_a) 
  ) -> cor_best_taska_fp

# Task B
fp_taskb <- as.data.frame(apply(fp_taskb, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

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
  ) -> cor_best_taskb_fp

# Comparison between Task A&B.
cor_best_taskb_fp %>%
  left_join(cor_best_taska_fp, by="id1") -> cor_best_ab_fp

cor_best_ab_fp %>%
  filter(cor_best_ab$best_cor_a != "NA") %>%
  select(-id1.y, -id2.y) -> cor_best_ab_fp

write.csv(cor_best_ab_fp, ".git/Dataset/fp_cor_taskab.csv", row.names=FALSE)

# Plotting
y_missing = 1.1

cor_best_ab_fp %>% 
  mutate(taska_missing = is.na(best_cor_a),
         best_cor_a2 = case_when(
    is.na(best_cor_a) ~ !!y_missing,
    TRUE ~ best_cor_a
  )) -> cor_best_ab_fp
view(cor_best_ab_fp)

cor_best_ab_fp %>%
  ggplot(aes(x = best_cor_b, y = best_cor_a2)) +
  geom_point(aes(x = best_cor_b, y = best_cor_a2, color = taska_missing), shape = 16, inherit.aes = FALSE) +
  coord_cartesian(
    xlim = c(-1,1),
    ylim = c(0.5,1.1),
    expand = FALSE,
    clip = "off"
  ) +
  labs(
    y = "Task A individual correlation with the best answer",
    x = "Task B individual correlation with the best answer",
    caption = ""
  ) +
  scale_y_continuous(breaks = c(seq(0.5,1.0,0.1), y_missing),
                     labels = c(seq(0.5,1.0,0.1), "NA")) +
  scale_x_continuous(breaks = seq(-1,1,0.5),
                     labels = seq(-1,1,0.5)) +
  scale_colour_grey(name = "Task A data", labels = c("Existed", "Missing")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
  ) -> plot_corab_fp

plot_corab_fp

#----------------------------------------------------------------------------------------------------
# Health care professional dataset.
fertility_doctors_taskbonly = here::here(".git/Dataset/hcp_taskb.csv")
hcp_taskb <- read.csv(fertility_doctors_taskbonly)
fertility_doctors_taskaonly = here::here(".git/Dataset/hcp_taska.csv")
hcp_taska = read.csv(fertility_doctors_taskaonly)

# Task A
hcp_taska %>% select(-X) -> hcp_taska
colnames(hcp_taska) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> task_a_columns
hcp_taska <- as.data.frame(apply(hcp_taska, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

# Correlation test between the best participant and all other participants in Task B.
expand.grid(id1 = hcp_taska$id, id2 = 44) %>% 
  mutate(
    best_cor_a = purrr::map2_dbl(id2, id1, function(id2, id1){
      id1_responses = unlist(hcp_taska[hcp_taska$id == id2, task_a_columns])
      id2_responses = unlist(hcp_taska[hcp_taska$id == id1, task_a_columns])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(best_cor_a) 
  ) -> cor_best_taska_hcp

# Task B
hcp_taskb <- as.data.frame(apply(hcp_taskb, 2, as.numeric), na.rm = TRUE) %>% round(digits=0)

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
  ) -> cor_best_taskb_hcp

# Comparison between task A&B.
cor_best_taskb %>%
  left_join(cor_best_taska, by="id1") -> cor_best_ab_hcp

cor_best_ab_hcp %>%
  filter(cor_best_ab$best_cor_a != "NA") %>%
  select(-id2.x, -id2.y) -> cor_best_ab_hcp

write.csv(cor_best_ab_hcp, ".git/Dataset/hcp_cor_taskab.csv", row.names=FALSE)

# Plotting
y_missing = 1.1

cor_best_ab_hcp %>% 
  mutate(taska_missing = is.na(best_cor_a),
         best_cor_a2 = case_when(
           is.na(best_cor_a) ~ !!y_missing,
           TRUE ~ best_cor_a
         )) -> cor_best_ab_2
#view(cor_best_ab_2)

cor_best_ab_2 %>%
  ggplot(aes(x = best_cor_b, y = best_cor_a2)) +
  geom_point(aes(x = best_cor_b, y = best_cor_a2, color = taska_missing), shape = 16, inherit.aes = FALSE) +
  coord_cartesian(
    xlim = c(-1,1),
    ylim = c(0.5,1.1),
    expand = FALSE,
    clip = "off"
  ) +
  labs(
    y = "Task A individual correlation with the best answer",
    x = "Task B individual correlation with the best answer",
    caption = ""
  ) +
  scale_y_continuous(breaks = c(seq(0.5,1.0,0.1), y_missing),
                     labels = c(seq(0.5,1.0,0.1), "NA")) +
  scale_x_continuous(breaks = seq(-1,1,0.5),
                     labels = seq(-1,1,0.5)) +
  scale_colour_grey(name = "Task A data", labels = c("Existed", "Missing")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
  ) -> plot_corab_hcp

plot_corab_hcp
